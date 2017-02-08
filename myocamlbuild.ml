open Ocamlbuild_plugin

let clib ~dir name =
  flag ["ocaml"; "library"; "link"; "byte"; "use_"^name]
       (S [A "-dllib"; A("-l"^name)]);
  flag ["ocaml"; "library"; "link"; "native"; "use_"^name]
       (S [A "-cclib"; A("-l"^name)]);
  flag ["link"; "ocaml"; "link_"^name] (A(dir^"/lib"^name^".a"));
  dep ["link"; "ocaml"; "use_"^name^""] [dir^"/lib"^name^".a"]

let () = dispatch @@ function
 | After_rules ->
    pdep ["c"; "compile"] "cdep" (fun dep -> [dep]);
    pdep ["ocaml"; "link"] "linkdep" (fun dep -> [dep]);
    clib ~dir:"lib" "bitpath_stubs"

 | _ -> ()
