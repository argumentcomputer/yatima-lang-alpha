use crate::{
  debug,
  file,
  graph::PackageGraph,
};
use multiaddr::Multiaddr;
use sp_cid::Cid;
use sp_ipld::Ipld;
use std::{
  collections::HashMap,
  rc::Rc,
  sync::{
    Arc,
    Mutex,
  },
};
use yatima_core::{
  anon,
  defs::{
    Def,
    Defs,
  },
  package::{
    Entry,
    Index,
    Package,
  },
};

/// For dealing with wasm situations where only callbacks are possible.
pub enum CallbackResult<T> {
  Sync(T),
  Callback,
}

/// Monitors the execution of several callbacks.
#[derive(Clone)]
pub struct CallbackMonitor<T> {
  pub result: Arc<Mutex<T>>,
  callback_completed: Arc<Mutex<HashMap<String, bool>>>,
  final_callback: Arc<dyn FnOnce(Arc<Mutex<T>>)>,
  executed: bool,
}

impl<T> CallbackMonitor<T> {
  pub fn new<U: Default>(final_callback: Arc<dyn FnOnce(Arc<Mutex<U>>)>) -> CallbackMonitor<U> {
    let result = Arc::new(Mutex::new(Default::default()));
    let callback_completed: Arc<Mutex<HashMap<String, bool>>> =
      Arc::new(Mutex::new(Default::default()));
    CallbackMonitor { result, callback_completed, final_callback, executed: false }
  }

  /// Tell the monitor to wait until notified by this callback id
  pub fn register_callback(&self, id: String) {
    let mut mon = self.callback_completed.lock().unwrap();
    mon.insert(id, false);
  }

  /// Let the monitor know that a callback has completed.
  pub fn notify(self, id: String) {
    let mut mons = self.callback_completed.lock().unwrap();
    debug!("Notified by {}", &id);
    mons.insert(id, true);
    if mons.values().all(|&b| b) {
      debug!("final_callback");
      let fc = self.final_callback.clone();
      fc(self.result.clone());
    }
  }
}

/// This trait describes the interactions with
/// externally stored IPLD structures.
pub trait Store: std::fmt::Debug {
  /// Get an IPLD from and Multiaddr
  fn get_by_multiaddr(&self, multiaddr: Multiaddr) -> Result<Ipld, String>;

  /// Load a package in a environment agnostic way
  fn load_by_name(&self, path: Vec<&str>) -> Result<Ipld, String>;

  /// Load a package in a environment agnostic way
  /// Necessary to circumvent the async limitations of wasm.
  fn load_by_name_with_callback(&self, path: Vec<&str>, callback: Box<dyn FnOnce(Ipld)>);

  /// Put an IPLD expression into the store
  fn put(&self, expr: Ipld) -> Cid;

  /// Get an IPLD expression from the store
  fn get(&self, link: Cid) -> Option<Ipld>;

  /// Get an IPLD expression from the store and call the callback
  /// asynchronously. Necessary to circumvent the async limitations of wasm.
  fn get_with_callback(&self, link: Cid, callback: Box<dyn FnOnce(Ipld)>);

  /// If this platform requires using callbacks to handle async requests.
  fn needs_callback(&self) -> bool;
}

/// Load all the package defs from the store.
pub fn load_package_defs(
  store: Rc<dyn Store>,
  package: Rc<Package>,
  monitor_opt: Option<CallbackMonitor<Defs>>,
) -> Result<CallbackResult<Defs>, String> {
  let Index(def_refs) = &package.index;
  let imports = &package.imports;
  if store.needs_callback() {
    let monitor =
      monitor_opt.as_ref().expect("When needing callback a CallbackMonitor must be provided");
    let ptr = &monitor.result;
    for import in imports {
      let store_c = store.clone();
      let monitor_c = monitor.clone();
      let import_id = import.cid.to_string();
      monitor.register_callback(import_id.clone());
      store.get_with_callback(
        import.cid.clone(),
        Box::new(move |package_ipld| {
          let imported_package =
            Package::from_ipld(&package_ipld).map_err(|e| format!("{:?}", e)).unwrap();
          // let imported_defs =
          load_package_defs(store_c, Rc::new(imported_package), Some(monitor_c.clone()))
              .unwrap();
          // let defs = ptr.lock().unwrap();
          // *defs = defs.merge(imported_defs, &import);
          monitor_c.notify(import_id.clone());
        }),
      );
    }
    for (name, cid) in def_refs {
      let name = name.clone();
      let store_c1 = store.clone();
      let ptr_c = ptr.clone();
      let entry_id = cid.clone().to_string();
      let monitor_c = monitor.clone();
      let monitor_c1 = monitor.clone();
      let monitor_c2 = monitor.clone();
      monitor.register_callback(entry_id.clone());
      store.get_with_callback(
        cid.clone(),
        Box::new(move |entry_ipld| {
          let entry = Entry::from_ipld(&entry_ipld).map_err(|e| format!("{:?}", e)).unwrap();
          let store_c2 = store_c1.clone();
          let type_anon_id = entry.type_anon.to_string();
          monitor_c.register_callback(type_anon_id.clone());
          store_c1.get_with_callback(
            entry.type_anon,
            Box::new(move |type_anon_ipld| {
              let type_anon =
                anon::Anon::from_ipld(&type_anon_ipld).map_err(|e| format!("{:?}", e)).unwrap();
              let term_anon_id = entry.term_anon.to_string();
              monitor_c1.register_callback(type_anon_id.clone());
              store_c2.get_with_callback(
                entry.term_anon,
                Box::new(move |term_anon_ipld| {
                  let term_anon = anon::Anon::from_ipld(&term_anon_ipld)
                    .map_err(|e| format!("{:?}", e))
                    .unwrap();
                  let mut defs = ptr_c.lock().unwrap();
                  defs.insert(
                    name,
                    Def::unembed(entry, type_anon, term_anon)
                      .map_err(|e| format!("{:?}", e))
                      .unwrap(),
                  );
                  monitor_c2.notify(term_anon_id);
                }),
              );
              monitor_c1.notify(type_anon_id);
            }),
          );
          monitor_c.notify(entry_id);
        }),
      );
    }
    Ok(CallbackResult::Callback)
  }
  else {
    let mut defs = Defs::new();
    for import in imports {
      if let Some(package_ipld) = store.get(import.cid.clone()) {
        let imported_package =
          Package::from_ipld(&package_ipld).map_err(|e| format!("{:?}", e))?;
        if let CallbackResult::Sync(imported_defs) =
          load_package_defs(store.clone(), Rc::new(imported_package), None)?
        {
          defs = defs.merge(imported_defs, &import);
        }
        else {
          panic!("Illegal callback response");
        }
      }
      else {
        return Err(format!("Failed to load {} at {}", import.name, import.cid));
      }
    }
    for (name, cid) in def_refs {
      if let Some(entry_ipld) = store.get(cid.clone()) {
        let entry = Entry::from_ipld(&entry_ipld).map_err(|e| format!("{:?}", e))?;
        if let Some(type_anon_ipld) = store.get(entry.type_anon) {
          let type_anon =
            anon::Anon::from_ipld(&type_anon_ipld).map_err(|e| format!("{:?}", e))?;
          if let Some(term_anon_ipld) = store.get(entry.term_anon) {
            let term_anon =
              anon::Anon::from_ipld(&term_anon_ipld).map_err(|e| format!("{:?}", e))?;
            defs.insert(
              name.clone(),
              Def::unembed(entry, type_anon, term_anon).map_err(|e| format!("{:?}", e))?,
            );
          }
          else {
            return Err(format!("Failed to term_anon at {}", &entry.term_anon));
          }
        }
        else {
          return Err(format!("Failed to type_anon at {}", &entry.type_anon));
        }
      }
      else {
        return Err(format!("Failed to load {} at {}", name, package.cid()));
      }
    }
    Ok(CallbackResult::Sync(defs))
  }
}

/// Show the contents of a link
pub fn show(
  store: Rc<dyn Store>,
  cid: Cid,
  typ_: String,
  var_index: bool,
) -> Result<String, String> {
  if let Some(ipld) = store.get(cid) {
    match typ_.as_str() {
      "package" => {
        let pack = Package::from_ipld(&ipld)?;
        Ok(format!("{}", pack))
      }
      "graph" => {
        let pack = Package::from_ipld(&ipld)?;
        let mut graph = PackageGraph::new();
        graph.add_package(
          |cid| store.get(cid).and_then(|ref ipld| Package::from_ipld(ipld).ok()),
          pack,
        );
        Ok(format!("{}", graph.to_dot()))
      }
      "entry" => {
        let entry = Entry::from_ipld(&ipld)?;
        println!("{}", entry);
        let mut s = format!("{}\n", entry);
        let def = file::parse::entry_to_def(entry, store).expect("valid def");
        s += format!("{}", def.pretty("#^".to_string(), var_index)).as_str();
        Ok(s)
      }
      "anon" => {
        let pack = anon::Anon::from_ipld(&ipld)?;
        Ok(format!("{:?}", pack))
      }
      _ => Ok(format!("{:?}", ipld)),
    }
  }
  else {
    Err(format!("cannot find {}", cid))
  }
}
