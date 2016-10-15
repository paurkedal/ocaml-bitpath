Synopsis
========

This OCaml library implements compact strings over booleans along with
related containers.  In particular,

  - `Bitpath` is the string structure.
  - `Bitpath_cover` implements sets of infinite strings sharing a finite set
    of prefixes.
  - `Bitpath_cover_map` implements maps with domain corresponding to
    `Bitpath_cover`.

The `Bitpath` structure is partly written in C to utilize bitwise
operations on the native integer type, while keeping the underlying array
compact.

As an example application, let `Bitpath.t` represent IPv4 and IPv6
addresses, and corresponding CIDR networks.  The `Bitpath_cover` structure
then represent sets of networks, and provides standard set operations on
these.


Build and Install
=================

The library has not been released yet.

Apart from OCaml 3.12 or later and a C compiler, you need [oUnit][ounit] if
tests are enabled and [OASIS][oasis] to build from the Git repository.

To build and install the library from the Git repository, run

    oasis setup
    ocaml setup.ml -configure [...]
    ocaml setup.ml -build
    ocaml setup.ml -install

The installed package is named `bitpath` and can be used with `ocamlfind`.


[ounit]: http://ounit.forge.ocamlcore.org/
[oasis]: http://oasis.forge.ocamlcore.org/
