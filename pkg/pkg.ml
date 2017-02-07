#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let () = Pkg.describe ~licenses "bitpath" @@ fun c ->
  Ok [
    Pkg.mllib "lib/bitpath.mllib";
    Pkg.clib "lib/libbitpath_stubs.clib";
    Pkg.lib "lib/bitpath_prereq.h";
    Pkg.lib "lib/bitpath.h";
    Pkg.test "tests/testsuite";
  ]
