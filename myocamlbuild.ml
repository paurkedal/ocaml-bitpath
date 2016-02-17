(* OASIS_START *)
(* OASIS_STOP *)

let () = mark_tag_used "tests"

let () = dispatch begin function
  | After_rules as e ->
    flag ["doc"; "ocaml"] & S[A"-charset"; A"utf8"];
    dispatch_default e
  | e -> dispatch_default e
end
