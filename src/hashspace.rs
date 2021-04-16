use crate::utils::log;
use base_x;
#[cfg(not(target_arch = "wasm32"))]
use directories_next::ProjectDirs;
use hashexpr::{
  base::Base,
  link::Link,
  Expr,
};
#[cfg(target_arch = "wasm32")]
use std::sync::{
  Arc,
  Mutex,
};
use std::{
  fmt,
  fs,
  path::{
    Path,
    PathBuf,
  },
};

pub mod cache;
#[cfg(target_arch = "wasm32")]
use crate::wasm_binds;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen_futures::spawn_local;
#[cfg(not(target_arch = "wasm32"))]
pub mod server;

/// Allows for defining impl methods that are dependent on hashspace
/// info to be wrapped.
pub trait HashspaceDependent {
  fn fmt(
    &self,
    f: &mut fmt::Formatter<'_>,
    hashspace: &Hashspace,
  ) -> fmt::Result;
}

pub struct HashspaceImplWrapper<T: HashspaceDependent> {
  hashspace: Hashspace,
  value: T,
}

impl<T: Clone + HashspaceDependent> HashspaceImplWrapper<T> {
  pub fn wrap(hashspace: &Hashspace, value: &T) -> Self {
    Self { hashspace: (*hashspace).clone(), value: (*value).clone() }
  }
}

impl<T> fmt::Display for HashspaceImplWrapper<T>
where T: HashspaceDependent
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.value.fmt(f, &self.hashspace)
  }
}

// TODO: Add custom directory option
/// Returns the hashspace directory. This function panics if the directory
/// cannot be created, read from or written to.
#[cfg(not(target_arch = "wasm32"))]
#[must_use]
pub fn hashspace_directory() -> PathBuf {
  let proj_dir =
    ProjectDirs::from("io", "yatima", "hashspace")
      .expect(
        "Error: No valid $HOME directory could be retrieved from the \
        operating system. Please open an issue at \
        \"https://github.com/yatima-inc/yatima/issues\" \
        if you see this message.");
  let path = proj_dir.cache_dir();
  if let Err(..) = fs::read_dir(&path) {
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
  PathBuf::from(path)
}

#[cfg(target_arch = "wasm32")]
#[must_use]
pub fn hashspace_directory() -> PathBuf {
  let path = Path::new("/hashspace");
  if let Err(..) = fs::read_dir(&path) {
    let path_name = path.to_str().expect(&format!(
          "Error: hashspace path {} contains invalid Unicode. \
           Please open an issue at \
          \"https://github.com/yatima-inc/yatima/issues\" \
          if you see this message", path.to_string_lossy()));
    println!("Creating new hashspace at {}", path_name);
    fs::create_dir_all(path).expect(&format!(
        "Error: cannot create hashspace path {}, likely due to lacking \
         sufficient filesystem permissions. \
         Please contact your system administrator or open an issue at \
         \"https://github.com/yatima-inc/yatima/issues\"", path_name));
    let mut perms = fs::metadata(path)
      .expect(&format!(
          "Error: cannot read metadata on hashspace path {}. \
            Please contact your system administrator or open an issue at \
            \"https://github.com/yatima-inc/yatima/issues\"", path_name))
      .permissions();
    perms.set_readonly(false);
    fs::set_permissions(path, perms).expect(&format!(
        "Error: cannot set hashspace path {} as writeable. \
            Please contact your system administrator or open an issue at \
            \"https://github.com/yatima-inc/yatima/issues\"", path_name))
  }
  PathBuf::from(path)
}

// Contains the configuration of hashspace
#[derive(Debug, Clone)]
pub struct Hashspace {
  dir: Option<PathBuf>,
  hosts: Vec<String>,
}

impl Hashspace {
  #[must_use]
  pub fn local() -> Self {
    Self { dir: Some(hashspace_directory()), hosts: Vec::new() }
  }

  #[must_use]
  pub fn with_hosts(hosts: Vec<String>) -> Self { Self { dir: None, hosts } }

  #[must_use]
  pub fn get(&self, link: Link) -> Option<Expr> {
    let cid = link.to_string();
    let data = match &self.dir {
      Some(dir) => {
        let path = dir.as_path().join(Path::new(&cid));
        fs::read(path).ok()?
      }
      None => {
        let result = self.get_local_storage(&cid);
        log(format!("Before remote_get({})", &cid).as_str());
        // let result = self.remote_get(cid);
        // result
        //   .clone()
        //   .map(|s| log(format!("Ok: {}", s).as_str()))
        //   .map_err(|s| log(format!("Error: {}", s).as_str()));
        match &result {
          Ok(d) => log(format!("After remote_get = {}", d).as_str()),
          Err(s) => log(format!("After error remote_get = {}", s).as_str()),
        };
        base_x::decode(Base::_64.base_digits(), result.ok()?.as_ref()).unwrap()
      }
    };
    match Expr::deserialize(&data) {
      Ok((_, x)) => Some(x),
      Err(e) => {
        log(format!("deserialization error: {}", e).as_str());
        panic!("deserialization error: {}", e)
      }
    }
  }

  #[must_use]
  pub fn put(&self, expr: &Expr) -> Link {
    let link = expr.link();
    let data = expr.serialize();
    match &self.dir {
      Some(dir) => {
        let path = dir.as_path().join(Path::new(&link.to_string()));
        fs::write(path, data).unwrap_or_else(|_| {
          panic!(
            "Error: cannot write to hashspace path {}. \
             Please open an issue at \
             \"https://github.com/yatima-inc/yatima/issues\" \
             if you see this message",
             link)
        });
      }
      None => {
        let data64 = base_x::encode(Base::_64.base_digits(), data.as_ref());
        self.put_local_storage(link.to_string(), data64.as_str()).unwrap();
        log("Before remote_put");
        let result = self.remote_put(&data);
        result
          .map(|s| log(format!("Ok: {}", s).as_str()))
          .map_err(|s| log(format!("Error: {}", s).as_str()))
          .ok();
        log("After remote_put");
      }
    }

    link
  }

  #[cfg(target_arch = "wasm32")]
  pub fn get_local_storage(&self, cid: &String) -> Result<String, String> {
    let window = web_sys::window().unwrap();
    let storage = window.local_storage().unwrap().unwrap();
    Ok(String::from(storage.get_item(cid.as_str()).unwrap().unwrap()))
  }

  #[cfg(target_arch = "wasm32")]
  pub fn put_local_storage(
    &self,
    cid: String,
    data: &str,
  ) -> Result<(), String> {
    let window = web_sys::window().unwrap();
    let storage = window.local_storage().ok().unwrap().unwrap();
    let res = storage.set_item(cid.as_str(), data);
    res.map_err(|e| e.as_string().unwrap())
  }

  #[cfg(not(target_arch = "wasm32"))]
  pub fn put_local_storage(
    &self,
    _cid: String,
    _data: &str,
  ) -> Result<(), String> {
    Err("Not implemented".to_string())
  }

  #[cfg(not(target_arch = "wasm32"))]
  pub fn get_local_storage(&self, _cid: &String) -> Result<String, String> {
    Err("Not implemented".to_string())
  }

  #[cfg(target_arch = "wasm32")]
  pub fn remote_get(&self, cid: String) -> Result<String, String> {
    let result_rc = Arc::new(Mutex::new(Err("Async not complete".to_string())));
    let r2 = Arc::clone(&result_rc);
    spawn_local(async move {
      let js_result = wasm_binds::hashspace_get(cid).await;
      *r2.lock().unwrap() = match js_result {
        Ok(js_value) => Ok(js_value.as_string().unwrap()),
        Err(js_value) => {
          let error = js_value.as_string().unwrap();
          Err(error)
        }
      };
    });

    let x = result_rc.lock().unwrap().clone();
    x
  }

  #[cfg(target_arch = "wasm32")]
  pub fn remote_put(&self, data: Vec<u8>) -> Result<String, String> {
    let result_rc = Arc::new(Mutex::new(Err("Async not complete".to_string())));
    let r2 = Arc::clone(&result_rc);
    spawn_local(async move {
      let js_result = wasm_binds::hashspace_put(data).await;
      *r2.lock().unwrap() = match js_result {
        Ok(js_value) => Ok(js_value.as_string().unwrap()),
        Err(js_value) => Err(js_value.as_string().unwrap()),
      };
    });

    result_rc.lock().unwrap()
  }

  #[cfg(not(target_arch = "wasm32"))]
  pub fn remote_get(&self, _cid: String) -> Result<String, String> {
    // TODO native impl
    Err("Not implemented".to_string())
  }

  #[cfg(not(target_arch = "wasm32"))]
  pub fn remote_put(&self, _data: &[u8]) -> Result<String, String> {
    // TODO native impl
    Err("Not implemented".to_string())
  }
}
