open Lgo
open OUnit2
open Helpers
open Core

(* Tests *)

let test_unit_to_dim_vector test_ctxt =
        assert_equal (unit_to_dim_vector "km" 2.) (Some ((kilo metre) **! 2.))
        
let test_str_to_dim_vector test_ctxt =
        assert_equal (str_to_dim_vector "MHz^3.") (Some ((mega hertz) **! 3.));
        assert_equal (str_to_dim_vector "nothing") (None)

let test_units_to_dim_vectors_exn test_ctxt =
        assert_equal (units_to_dim_vectors_exn "km*h^-1") ((kilo metre) *! (hour **! -1.))
        
let test_quantity test_ctxt =
        assert_equal (q 1. "km") (q 100000. "cm");
        let dist = (q 10. "ly") in
        assert_bool "not equal" ((dist.value *@ dist.unit) = (10. *@ (c_light *! year)))

let test_stringify test_ctxt =
        assert_equal (stringify (q 9. "N")) "9 m*s^-2.*kg"

let convert_units test_ctxt =
        assert_equal (convert (q 10. "N*m^-2") ~to_unit:"Pa") "10 Pa"

let suite =
"str_units">:::
 [
         "unit to dim_vector"    >:: test_unit_to_dim_vector;
         "str to dim_vector"     >:: test_str_to_dim_vector;
         "units to dim_vectors"  >:: test_units_to_dim_vectors_exn;
         "test quantity"         >:: test_quantity;
         "test stringify"        >:: test_stringify;
         "convert units"         >:: convert_units;
 ]

let () =
  run_test_tt_main suite
