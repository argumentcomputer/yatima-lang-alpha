use directories_next::ProjectDirs;

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
  fs,
  path::{
    Path,
    PathBuf,
  },
};
use yatima_utils::store::Store;
use bytecursor::ByteCursor;

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

pub fn get(link: Cid) -> Option<Ipld> {
  let dir = hashspace_directory();
  let path = dir.as_path().join(Path::new(&link.to_string()));
  let file: Vec<u8> = fs::read(path).ok()?;
  // println!("file {:?}", file);
  let res: Ipld =
    DagCborCodec.decode(ByteCursor::new(file)).expect("valid cbor bytes");
  Some(res)
}

pub fn put(expr: Ipld) -> Cid {
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
pub struct FileStore {}

impl FileStore {
  pub fn new() -> Self { FileStore {} }
}

impl Store for FileStore {
  fn get(&self, link: Cid) -> Option<Ipld> { get(link) }

  fn put(&self, expr: Ipld) -> Cid { put(expr) }
}
