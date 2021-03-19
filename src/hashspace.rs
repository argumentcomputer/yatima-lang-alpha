#[cfg(not(target_arch = "wasm32"))]
use directories_next::ProjectDirs;
use hashexpr::{
  link::Link,
  Expr,
};
use std::{
  fs,
  path::{
    Path,
    PathBuf,
  },
};

pub mod cache;
#[cfg(not(target_arch = "wasm32"))]
pub mod server;

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

// Rust native runtime

#[cfg(not(target_arch = "wasm32"))]
pub fn get(link: Link) -> Option<Expr> {
  let dir = hashspace_directory();
  let path = dir.as_path().join(Path::new(&link.to_string()));
  let file = fs::read(path).ok()?;
  // println!("file {:?}", file);
  match Expr::deserialize(&file) {
    Ok((_, x)) => Some(x),
    Err(e) => panic!("deserialization error: {}", e),
  }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn put(expr: Expr) -> Link {
  let dir = hashspace_directory();
  let link = expr.link();
  let path = dir.as_path().join(Path::new(&link.to_string()));
  fs::write(path, expr.serialize()).expect(&format!(
    "Error: cannot write to hashspace path {}. \
     Please open an issue at \
     \"https://github.com/yatima-inc/yatima/issues\" \
     if you see this message",
    link));
  link
}

// Rust wasm runtime

#[cfg(target_arch = "wasm32")]
pub fn get(link: Link) -> Option<Expr> {
  let dir = hashspace_directory();
  let path = dir.as_path().join(Path::new(&link.to_string()));
  let file = fs::read(path).ok()?;
  // println!("file {:?}", file);
  match Expr::deserialize(&file) {
    Ok((_, x)) => Some(x),
    Err(e) => panic!("deserialization error: {}", e),
  }
}

#[cfg(target_arch = "wasm32")]
pub fn put(expr: Expr) -> Link {
  let dir = hashspace_directory();
  let link = expr.link();
  let path = dir.as_path().join(Path::new(&link.to_string()));
  fs::write(path, expr.serialize()).expect(&format!(
    "Error: cannot write to hashspace path {}. \
     Please open an issue at \
     \"https://github.com/yatima-inc/yatima/issues\" \
     if you see this message",
    link));
  link
}
