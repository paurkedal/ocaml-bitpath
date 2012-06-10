open OUnit

let suite = "Bitlib Test Suite" >::: [
    "test_bitstring" >:: Test_bitstring.test;
]

let _ = run_test_tt_main suite
