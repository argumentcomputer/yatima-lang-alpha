# hash-expressions: A self-describing, typed, canonical data format
An interchange format for hash-linked data which is well-typed, self-describing
and efficiently serializing.

- #base: hash-base: Self-describing numerical base print formats
- #atom: hash-atom: Typed data with efficient serializations and print formats
- #expr: hash-expr: Trees of #atoms

## hash-expr base: A self-describing textual print format for binary data.

On its own, the string "10" is ambiguous. Is this decimal ten, binary two, octal
eight, or hexadecimal sixteen? We have to establish our choice of base by
convention. The hash-expr base specification extends the standard `'b'`, `'o'`,
`'d'`, `'x'` numeric prefixes for bases 2, 8, 10 and 16 with the prefixes `'v'`,
`'I'`, and `'~'` for specific base 32, 58, and 64 encodings. These prefixes have
been chosen to be url-safe and to not occur in the digit-set of the encoding
they specify. That is, the character `'v'` is not a valid base32z digit, nor is
`'I'` a valid base58btc digit.

| encoding   | code | description               |
|------------|------|---------------------------|
| base2,     | 'b', | binary (01010101),        |
| base8,     | 'o', | octal,                    |
| base10,    | 'd', | decimal,                  |
| base16,    | 'x', | hexadecimal,              |
| base32z,   | 'v', | z-base-32                 |
| base58btc, | 'I', | base58 bitcoin,           |
| base64url, | '~', | rfc4648 no padding,       |
| ...        | ...  | This table is extensible! |

## hash-expr atoms: Type codes and Syntax sugars for data literals

`#atom` is a tables of prefix codes which describe the type of the encoded
bytes, as well as a textual syn

| type     | code   | description                            | atom syntax                                          |
| -------- | ------ | -------------------------------------- | ---------------------------------------------------- |
| link     | x00    | a blake3 hash digest                   | `IVdLcYQdSL7W32L6CY3VJTbyodRnN5pBvo5gMbaSa3nk6ENQ21` |
| bits     | x01    | bitstrings                             | `~"AAKioq"`                                          |
| text     | x02    | utf8 encoded text                      | `"foobar"`                                           |
| char     | x03    | a unicode code point                   | `'f'`                                                |
| int      | x04    | arbitrary precision signed integer     | `+1`, `-1`                                           |
| nat      | x05    | arbitrary precision unsigned integer   | `1`                                                  |
| ...      | ...    | This is extensible!                    | ...                                                  |

Note that the code length is meaningful, so `x00` and `x0000` represent
different codes.

