pub use crate::base::{
    alphabet::Alphabet,
    decoder::DecodeError
};

/// Encode an input vector using the given alphabet.
pub fn encode<A: Alphabet>(alphabet: A, input: &[u8]) -> String {
    alphabet.encode(input)
}

/// Decode an input vector using the given alphabet.
pub fn decode<A: Alphabet>(alphabet: A, input: &str) -> Result<Vec<u8>, DecodeError> {
    alphabet.decode(input)
}
