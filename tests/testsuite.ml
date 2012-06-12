open OUnit

let suite = "Bitlib Test Suite" >::: [
    "test_bitstring" >:: Test_bitstring.test;
    "test_prefixset" >:: Test_prefixset.test;
    "test_prefixmap" >:: Test_prefixmap.test;
]

let _ = run_test_tt_main suite
