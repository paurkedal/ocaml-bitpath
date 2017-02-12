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

To install from the author's OPAM repository:

    opam repo add paurkedal https://github.com/paurkedal/opam-repo-paurkedal.git
    opam install bitpath

For manual installs, use the `pkg/pkg.ml` script.
