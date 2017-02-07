open Ocamlbuild_plugin

let () = dispatch @@ function
 | After_rules ->
    pdep ["c"; "compile"] "cdep" (fun dep -> [dep]);
    pdep ["ocaml"; "link"] "linkdep" (fun dep -> [dep])
 | _ -> ()
