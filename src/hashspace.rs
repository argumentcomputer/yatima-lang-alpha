#[cfg(not(target_arch = "wasm32"))]
use directories_next::ProjectDirs;
use hashexpr::{
  link::Link,
  Expr,
};
use futures::executor;
use std::{
  rc::Rc,
  cell::RefCell,
  sync::{Arc, Mutex},
  fs,
  fmt,
  path::{
    Path,
    PathBuf,
  },
};
use crate::{
  utils::log
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
  fn fmt(&self, f: &mut fmt::Formatter<'_>, hashspace: &Hashspace) -> fmt::Result;
}

pub struct HashspaceImplWrapper<T: HashspaceDependent> {
  hashspace: Hashspace,
  value: T
}

impl<T: Clone + HashspaceDependent> HashspaceImplWrapper<T> {
  pub fn wrap(hashspace: &Hashspace, value: &T) -> Self {
    Self {
      hashspace: (*hashspace).clone(),
      value: (*value).clone()
    }
  }
}

impl<T> fmt::Display for HashspaceImplWrapper<T>
where T: HashspaceDependent {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.value.fmt(f, &self.hashspace)
  }
}

// TODO: Add custom directory option
/// Returns the hashspace directory. This function panics if the directory
/// cannot be created, read from or written to.
#[cfg(not(target_arch = "wasm32"))]
pub fn hashspace_directory() -> PathBuf {
  let proj_dir =
    ProjectDirs::from("io", "yatima", "hashspace")
      .expect(
        "Error: No valid $HOME directory could be retrieved from the \
        operating system. Please open an issue at \
        \"https://github.com/yatima-inc/yatima/issues\" \
        if you see this message.");
  let path = proj_dir.cache_dir();
  match fs::read_dir(&path) {
    Ok(_) => (),
    Err(_) => {
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
  }
  PathBuf::from(path)
}

#[cfg(target_arch = "wasm32")]
pub fn hashspace_directory() -> PathBuf {
  let path = Path::new("/hashspace");
  match fs::read_dir(&path) {
    Ok(_) => (),
    Err(_) => {
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
  }
  PathBuf::from(path)
}

// Contains the configuration of hashspace
#[derive(Debug,Clone)]
pub struct Hashspace {
  dir: Option<PathBuf>,
  hosts: Vec<String>
}

impl Hashspace {
  pub fn local() -> Self {
    Self {
      dir: Some(hashspace_directory()),
      hosts: Vec::new()
    }
  }

  pub fn with_hosts(hosts: Vec<String>) -> Self {
    Self {
      dir: None,
      hosts: hosts,
    }
  }

  pub fn get(&self, link: Link) -> Option<Expr> {
    let cid = link.to_string();
    let data = 
      match &self.dir {
        Some(dir) => {
          let path = dir.as_path().join(Path::new(&cid));
          fs::read(path).ok()?
        }
        None => {
          log("Before remote_get");
          let result = self.remote_get(cid);
          log("After remote_get");
          result.ok()?.as_bytes().to_vec()
        }
      };
    match Expr::deserialize(&data) {
      Ok((_, x)) => Some(x),
      Err(e) => panic!("deserialization error: {}", e),
    }
  }

  pub fn put(&self, expr: Expr) -> Link {
    let link = expr.link();
    let data = expr.serialize();
    match &self.dir {
      Some(dir) => {
        let path = dir.as_path().join(Path::new(&link.to_string()));
        fs::write(path, data).expect(&format!(
            "Error: cannot write to hashspace path {}. \
             Please open an issue at \
             \"https://github.com/yatima-inc/yatima/issues\" \
             if you see this message",
             link));
      }
      None => {
        log("Before remote_put");
        let result = self.remote_put(data.to_vec());
        result
          .map(|s| log(format!("Ok: {}", s).as_str()))
          .map_err(|s| log(format!("Error: {}", s).as_str()));
        log("After remote_put");
      }
    }

    link
  }

  #[cfg(target_arch = "wasm32")]
  pub fn remote_get(&self, cid: String) -> Result<String,String> {
    let result_rc = Arc::new(Mutex::new(Err("Async not complete".to_string())));
    let r2 = Arc::clone(&result_rc);
    spawn_local(async move {
      let js_result = wasm_binds::hashspace_get(cid).await;
      *r2.lock().unwrap() = match js_result {
        Ok(js_value) => {
          Ok(js_value.as_string().unwrap())
        }
        Err(js_value) => {
          let error = js_value.as_string().unwrap();
          Err(error)
        }
      };
    });

    let x = result_rc.lock().unwrap().clone(); x
  }

  #[cfg(target_arch = "wasm32")]
  pub fn remote_put(&self, data: Vec<u8>) -> Result<String, String> {
    let result_rc = Arc::new(Mutex::new(Err("Async not complete".to_string())));
    let r2 = Arc::clone(&result_rc);
    spawn_local(async move {
      let js_result = wasm_binds::hashspace_put(data).await;
      *r2.lock().unwrap() = match js_result {
        Ok(js_value) => Ok(js_value.as_string().unwrap()),
        Err(js_value) => Err(js_value.as_string().unwrap())
      };
    });
    
    let x = result_rc.lock().unwrap().clone(); x
  }

  #[cfg(not(target_arch = "wasm32"))]
  pub fn remote_get(&self, cid: String) -> Result<String,String> {
    // TODO native impl
    Err("Not implemented".to_string())
  }

  #[cfg(not(target_arch = "wasm32"))]
  pub fn remote_put(&self, data: Vec<u8>) -> Result<String, String> {
    // TODO native impl
    Err("Not implemented".to_string())
  }
}
