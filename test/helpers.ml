open OUnit2

(* Helpers *)

let assert_inequal v1 v2 =
        assert_bool "should be inequal" (v1 != v2)

let assert_float_near v1 v2 epsilon =
        assert_bool "the difference is bigger then the given epsilon" ((v1 -. v2) < epsilon)

