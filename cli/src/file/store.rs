use directories_next::ProjectDirs;

use bytecursor::ByteCursor;
use multiaddr::Multiaddr;
use sp_cid::Cid;
use sp_ipld::{
  dag_cbor::{
    cid,
    DagCborCodec,
  },
  Codec,
  Ipld,
};
use std::{
  collections::HashMap,
  fs,
  path::{
    Path,
    PathBuf,
  },
  rc::Rc,
  sync::{
    Arc,
    Mutex,
  },
};
#[cfg(not(target_arch = "wasm32"))]
use tokio::{
  runtime::Handle,
  task,
};
use yatima_core::defs::Defs;
use yatima_utils::{
  debug,
  file::parse,
  ipfs::IpfsApi,
  store::{
    Callback,
    Store,
  },
};

pub fn hashspace_directory() -> PathBuf {
  let proj_dir =
    ProjectDirs::from("io", "yatima", "hashspace").unwrap_or_else(|| {
      panic!(
        "Error: No valid $HOME directory could be retrieved from the \
        operating system. Please open an issue at \
        \"https://github.com/yatima-inc/yatima/issues\" \
        if you see this message.")
    });
  let path = proj_dir.cache_dir();
  match fs::read_dir(&path) {
    Ok(_) => (),
    Err(_) => {
      let path_name = path.to_str().unwrap_or_else(|| {
        panic!(
          "Error: hashspace path {} contains invalid Unicode. \
           Please open an issue at \
          \"https://github.com/yatima-inc/yatima/issues\" \
          if you see this message", path.to_string_lossy())
      });
      println!("Creating new hashspace at {}", path_name);
      fs::create_dir_all(path).unwrap_or_else(|_| {
        panic!(
        "Error: cannot create hashspace path {}, likely due to lacking \
         sufficient filesystem permissions. \
         Please contact your system administrator or open an issue at \
         \"https://github.com/yatima-inc/yatima/issues\"", path_name)
      });
      let mut perms = fs::metadata(path)
        .unwrap_or_else(|_| {
          panic!(
          "Error: cannot read metadata on hashspace path {}. \
            Please contact your system administrator or open an issue at \
            \"https://github.com/yatima-inc/yatima/issues\"", path_name)
        })
        .permissions();
      perms.set_readonly(false);
      fs::set_permissions(path, perms).unwrap_or_else(|_| {
        panic!(
        "Error: cannot set hashspace path {} as writeable. \
            Please contact your system administrator or open an issue at \
            \"https://github.com/yatima-inc/yatima/issues\"", path_name)
      })
    }
  }
  PathBuf::from(path)
}

pub fn fs_get(link: Cid) -> Option<Ipld> {
  let dir = hashspace_directory();
  let path = dir.as_path().join(Path::new(&link.to_string()));
  let file: Vec<u8> = fs::read(path).ok()?;
  let res: Ipld =
    DagCborCodec.decode(ByteCursor::new(file)).expect("valid cbor bytes");
  Some(res)
}

pub fn fs_put(expr: Ipld) -> Cid {
  let dir = hashspace_directory();
  let link = cid(&expr);
  let path = dir.as_path().join(Path::new(&link.to_string()));
  fs::write(path, DagCborCodec.encode(&expr).unwrap().into_inner())
    .unwrap_or_else(|_| {
      panic!(
    "Error: cannot write to hashspace path {}. \
     Please open an issue at \
     \"https://github.com/yatima-inc/yatima/issues\" \
     if you see this message",
    link)
    });
  link
}

#[derive(Debug, Clone)]
pub struct FileStoreOpts {
  /// Write to the file system
  pub use_file_store: bool,
  /// The relative root directory
  pub root: PathBuf,
}

#[derive(Debug, Clone)]
pub struct FileStore {
  pub opts: FileStoreOpts,
  /// Put and get data from the IPFS daemon
  pub ipfs_api: Option<IpfsApi>,
  /// This is used when use_file_store is false
  mem_store: Arc<Mutex<HashMap<Cid, Ipld>>>,
}

impl FileStore {
  pub fn new(opts: FileStoreOpts, ipfs_api: Option<IpfsApi>) -> Self {
    FileStore { opts, mem_store: Default::default(), ipfs_api }
  }
}

#[cfg(not(target_arch = "wasm32"))]
impl Store for FileStore {
  fn get_by_multiaddr(&self, _addr: Multiaddr) -> Result<Ipld, String> {
    // ipfs::get(addr);
    // TODO implement
    Err("Not implemented".to_owned())
  }

  fn needs_callback(&self) -> bool { false }

  fn get_with_callback(&self, _link: Cid, _callback: Callback<Ipld, Defs>) {
    panic!("Not implemented for this platform.")
  }

  fn load_by_name_with_callback(&self, _path: Vec<&str>, _callback: Callback<Ipld, Defs>) {
    panic!("Not implemented for this platform.")
  }

  fn load_by_name(&self, path: Vec<&str>) -> Result<Ipld, String> {
    let mut fs_path = self.opts.root.clone();
    for n in path {
      fs_path.push(n);
    }
    fs_path.set_extension("ya");
    let env = parse::PackageEnv::new(
      self.opts.root.clone(),
      fs_path,
      Rc::new(self.clone()),
    );
    let (_cid, p, _ds) = parse::parse_file(env)?;
    let ipld = p.to_ipld();
    Ok(ipld)
  }

  fn get(&self, link: Cid) -> Option<Ipld> {
    if !self.opts.use_file_store {
      self.mem_store.lock().unwrap().get(&link).cloned()
    }
    else {
      fs_get(link).or_else(|| {
        self
          .ipfs_api
          .as_ref()
          .map(|api| {
            task::block_in_place(move || {
              Handle::current().block_on(async move { api.dag_get(link.to_string()).await.ok() })
            })
          })
          .flatten()
      })
    }
  }

  fn put(&self, expr: Ipld) -> Cid {
    self.ipfs_api.as_ref().map(|api| {
      let expr = expr.clone();
      task::block_in_place(move || {
        Handle::current().block_on(async move {
          match api.dag_put(expr).await {
            Ok(_r) => {
              // debug!("Put success {:?}\n", r)
              ()
            }
            Err(e) => debug!("Put error {:?}\n", e),
          }
        })
      });
    });
    if !self.opts.use_file_store {
      let link = cid(&expr);
      self.mem_store.lock().unwrap().insert(link, expr);
      link
    }
    else {
      fs_put(expr)
    }
  }
}

#[cfg(target_arch = "wasm32")]
impl Store for FileStore {
  fn get_by_multiaddr(&self, _addr: Multiaddr) -> Result<Ipld, String> {
    // ipfs::get(addr);
    // TODO implement
    Err("Not implemented".to_owned())
  }

  fn load_by_name(&self, path: Vec<&str>) -> Result<Ipld, String> {
    let mut fs_path = self.opts.root.clone();
    for n in path {
      fs_path.push(n);
    }
    fs_path.set_extension("ya");
    let env = parse::PackageEnv::new(self.opts.root.clone(), fs_path, Rc::new(self.clone()));
    let (_cid, p, _ds) = parse::parse_file(env)?;
    let ipld = p.to_ipld();
    Ok(ipld)
  }

  fn get(&self, link: Cid) -> Option<Ipld> {
    if !self.opts.use_file_store {
      self.mem_store.lock().unwrap().get(&link).map(|ipld| ipld.clone())
    }
    else {
      fs_get(link)
    }
  }

  fn put(&self, expr: Ipld) -> Cid {
    if !self.opts.use_file_store {
      let link = cid(&expr);
      self.mem_store.lock().unwrap().insert(link, expr);
      link
    }
    else {
      fs_put(expr)
    }
  }
}
