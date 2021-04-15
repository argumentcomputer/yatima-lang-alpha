use nom::AsBytes;

// need this because nom doesn't implement AsBytes for Vec<u8>
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ByteVec(Vec<u8>);

impl From<Vec<u8>> for ByteVec {
    fn from(x: Vec<u8>) -> Self {
        Self(x)
    }
}

impl AsBytes for ByteVec {
    fn as_bytes(&self) -> &[u8] {
        match self {
            Self(x) => x.as_ref(),
        }
    }
}
