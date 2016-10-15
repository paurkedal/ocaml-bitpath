#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let () = Pkg.describe ~licenses "bitpath" @@ fun c ->
  Ok [
    Pkg.mllib "src/bitpath.mllib";
    Pkg.clib "src/libbitpath_stubs.clib";
    Pkg.lib "src/bitpath_prereq.h";
    Pkg.lib "src/bitpath.h";
  ]
