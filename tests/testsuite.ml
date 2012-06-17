open OUnit

let suite = "Bitlib Test Suite" >::: [
    "test_bitpath" >:: Test_bitpath.test;
    "test_prefixset" >:: Test_prefixset.test;
    "test_prefixmap" >:: Test_prefixmap.test;
]

let _ = run_test_tt_main suite
