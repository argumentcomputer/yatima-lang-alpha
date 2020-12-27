# Hashspace - Content addressed data storage

> A data storage indexed by hashes

Run server with:
```
# If using Nix
nix-shell -p cargo

# Then compile and run
cargo run

# And test
curl -X PUT http://localhost:8000/store -T Cargo.toml

# And verify
curl -X GET http://localhost:8000/store/13196f03c8782bbbf88930d13a15957133f2d862b9a1915aa44c298286777d1d | b3sum
```
