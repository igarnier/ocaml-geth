A library to interact with Ethereum blockchains.

For now, the library contains:
1) bindings to the Geth RPC interface (via http),
2) scripts to deploy geth nodes via ssh,
3) facilities to deploy Solidity contracts and call them from `OCaml`.

Depends on ssh-client (not in OPAM) among other things.

* INSTALL

We assume that a working `OCaml` environment has been installed through `opam`.

1) Clone, build and install the `ssh-client` library available at https://github.com/igarnier/ssh-client
2) Install the `ocamlnet`, `cryptokit`, `zarith`, `hex`, `bitstring` packages using `opam`
3) Clone this repository, then run `jbuilder build && jbuilder install` in the cloned directory.