use directories_next::ProjectDirs;

use cid::Cid;
use sp_ipld::{
  dag_cbor::{
    cid,
    DagCborCodec,
  },
  ByteCursor,
  Codec,
  Ipld,
};
use crate::ipfs;
use yatima_utils::{
  file::parse,
};
use multiaddr::Multiaddr;
use std::{
  fs,
  path::{
    Path,
    PathBuf,
  },
  rc::Rc,
};
use tokio::{
  runtime::Handle,
  task,
};
use yatima_utils::store::Store;

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
pub struct FileStore {
  use_ipfs_daemon: bool,
}

impl FileStore {
  pub fn new() -> Self { FileStore { use_ipfs_daemon: true } }
}

impl Store for FileStore {
  fn get_by_multiaddr(&self, _addr: Multiaddr) -> Result<Ipld, String> {
    // ipfs::get(addr);
    // TODO implement
    Err("Not implemented".to_owned())
  }

  fn load_by_name(&self, path: Vec<&str>) -> Result<Ipld, String> {
    let root = std::env::current_dir().unwrap();
    let mut fs_path = root.clone();
    for n in path {
      fs_path.push(n);
    }
    fs_path.set_extension("ya");
    let env = parse::PackageEnv::new(root, fs_path, Rc::new(self.clone()));
    let (_cid, p, _ds) = parse::parse_file(env)?;
    let ipld = p.to_ipld();
    Ok(ipld)
  }

  fn get(&self, link: Cid) -> Option<Ipld> {
    fs_get(link).or_else(||
      if self.use_ipfs_daemon { 
        task::block_in_place(move || {
          Handle::current()
            .block_on(async move { ipfs::dag_get(link.to_string()).await.ok() })
        })
      } else {
        None
      })
  }

  fn put(&self, expr: Ipld) -> Cid { 
      if self.use_ipfs_daemon {
        let expr = expr.clone();
        task::block_in_place(move || {
          Handle::current()
            .block_on(async move { ipfs::dag_put(expr).await.unwrap(); })
        });   
      }
      fs_put(expr)
  }
}
