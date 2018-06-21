open Lgo
open OUnit2

(* Helpers *)

let assert_inequal v1 v2 =
        assert_bool "should be inequal" (v1 != v2)

(* Tests *)

let test_dim_vector_basic test_ctxt =
        assert_equal pascal (newton /! (metre*!metre) )

let test_prefix_km test_ctxt =
        assert_equal (kilo metre) (1000.0 *@ metre)

let test_newton test_ctxt =
        assert_equal newton { metre = 1.; second = -2.; kilogram = 1.;
        ampere = 0.; kelvin = 0.; mole = 0.; candela = 0.; factor = 1. }

let test_hertz test_ctxt =
        assert_equal hertz { metre = 0.; second = -1.; kilogram = 0.;
        ampere = 0.; kelvin = 0.; mole = 0.; candela = 0.; factor = 1. }

let test_not_equal_vectors test_ctxt =
        assert_inequal tesla volt


let suite =
"units">:::
 [
        "dim vector basic"      >:: test_dim_vector_basic;
        "prefix kilo"           >:: test_prefix_km;
        "derived units 1"       >:: test_newton;
        "derived units 2"       >:: test_hertz;
        "derived units 3"       >:: test_not_equal_vectors;
 ]

let () =
  run_test_tt_main suite
