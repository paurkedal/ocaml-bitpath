open OUnit

let suite = "Bitpath Test Suite" >::: [
  "test_bitpath" >:: Test_bitpath.test;
  "test_bitpath_cover" >:: Test_bitpath_cover.test;
  "test_bitpath_cover_map" >:: Test_bitpath_cover_map.test;
]

let _ = run_test_tt_main suite
