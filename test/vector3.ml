open Lgo
open OUnit2
open Helpers

(* Tests *)

let v = (1., 2., 3.14)
let v_alt = (21., 22., 23.)

let test_string_representation test_ctxt =
        let str_v = (string_of_vector v) in
        assert_equal str_v "(1, 2, 3.14)"

let test_scalar_multiplication test_ctxt =
        assert_equal (scalar_multi 2. v) (2., 4., 6.28)

let test_vector_magnitude test_ctxt =
        assert_float_near (magnitude v) 3.8548 0.0001

let test_vector_equality test_ctxt =
        assert_bool "not equal" (equal v (1., 2., 3.14))

let test_dot_product test_ctxt =
        assert_float_near (dot v v_alt) 137.22 0.0001

let test_cross_product test_ctxt =
        assert_bool "not equal" (equal (cross v v_alt) (-23.08, 42.94, -20.))

let suite =
"vector3">:::
 [
        "string representation" >:: test_string_representation;
        "scalar multiplication" >:: test_scalar_multiplication;
        "vector magnitude"      >:: test_vector_magnitude;
        "vector equality"       >:: test_vector_equality;
        "vector dot product"    >:: test_dot_product;
        "vector cross product"  >:: test_cross_product;
 ]

let () =
  run_test_tt_main suite
