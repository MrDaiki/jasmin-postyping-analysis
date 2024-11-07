# Jasmin Postyping analysis

This repo objective is to provide a set of semantic analysis comming after Jasmin typing step.
Currently, it contains: 
- Reaching definitions analysis


# install 

This repo use nix as its package manager. To install dependencies, just run:
```
nix-shell
``` 

For more informations, check [nix](https://nixos.org/)

# Backlog

- [ ] Dead code 
  - [ ] Variable liveness
  - [x] Never called functions 
  - [ ] Function with no effect or return values 
  - [ ] Empty domain loops
- [x] Uninitialised variables 
- [x] Inline (static) variables depending on dynamic values 
- [x] Mutability of non `mut` array pointers 

