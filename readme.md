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
  - [ ] Never called functions 
  - [ ] Function with no effect or return values 
  - [ ] Empty domains loops 
- [x] Uninitialised variables 
- [x] Inline (static) variables depending on dynamic values 
- [x] Mutability of non `mut` array pointers 

Pour détecter des étourderies (warning)
- Fonctions sans effet ni valeur de retour
- Fonctions jamais appelées
- Code mort
- variables non-initialisées
- constantes tronquées
- domaine vide des boucles =for=
Pour garantir que la compilation réussisse (erreur)
- Valeurs =inline= qui dépendent de valeurs =reg= / =stack=
- https://github.com/jasmin-lang/jasmin/issues/828
Après la vérification de type
- https://github.com/jasmin-lang/jasmin/issues/727
- https://github.com/jasmin-lang/jasmin/issues/854

