#[cfg(test)]
mod tests {
  use crate::file::store::{
    FileStore,
    FileStoreOpts,
  };
  use std::{
    path::PathBuf,
    rc::Rc,
  };
  use yatima_utils::{
    file::parse::{
      parse_text,
      PackageEnv,
    },
    ipfs::IpfsApi,
  };
  #[ignore]
  #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
  async fn test_get_boolya() {
    let src = "
    package bool where

    def Bool: Type = #Bool

    def Bool.True: Bool = #Bool.true
    def Bool.False: Bool = #Bool.false

    def Bool.eql: ∀ (x y: Bool) -> Bool = #Bool.eql
    def Bool.lte: ∀ (x y: Bool) -> Bool = #Bool.lte
    def Bool.lth: ∀ (x y: Bool) -> Bool = #Bool.lth
    def Bool.gte: ∀ (x y: Bool) -> Bool = #Bool.gte
    def Bool.gth: ∀ (x y: Bool) -> Bool = #Bool.gth


    def Bool.and: ∀ (x y: Bool) -> Bool = #Bool.and
    def Bool.or:  ∀ (x y: Bool) -> Bool = #Bool.or
    def Bool.xor: ∀ (x y: Bool) -> Bool = #Bool.xor

    def Bool.not: ∀ (x: Bool) -> Bool = #Bool.not

    def Bool.neq (x y: Bool): Bool = Bool.not (Bool.eql x y)

    def Bool.if (A: Type) (bool : Bool) (t f: A): A = 
        (case bool) (λ _ => A) t f
    ";
    let root = std::env::current_dir().unwrap();
    let path = PathBuf::from("bool.ya");
    let api = IpfsApi::new("localhost:5001".to_string());
    let store = Rc::new(
      FileStore::new(FileStoreOpts { use_file_store: true, root: root.clone() },
      Some(api.clone()),
    ));
    let env = PackageEnv::new(root, path, store);
    let (cid, p, _defs) = parse_text(src, env).unwrap();

    let ipld = api.dag_get(cid.to_string()).await.unwrap();
    assert_eq!(ipld, p.to_ipld());
  }
}
