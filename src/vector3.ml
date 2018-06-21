let string_of_vector (x, y, z) =
        Printf.sprintf "(%g, %g, %g)" x y z

let scalar_multi scalar (x, y, z) =
        (scalar *. x, scalar *. y, scalar *. z)

let dot (a1, a2, a3) (b1, b2, b3) =
        (a1 *. b1) +. (a2 *. b2) +. (a3 *. b3)

let magnitude (x, y, z) =
        sqrt (dot (x, y, z) (x, y, z))

let unit_vector (x, y, z) =
        let v = (x, y, z) in
        scalar_multi (1.0 /. (magnitude v)) v

let equal (a1, a2, a3) (b1, b2, b3) =
        a1 = b1 && a2 = b2 && a3 = b3

let cross (a1, a2, a3) (b1, b2, b3) =
        (a2 *. b3 -. a3 *. b2,
         a3 *. b1 -. a1 *. b3,
         a1 *. b2 -. a2 *. b1)
 
let scalar_triple a b c =
        dot a (cross b c)
 
let vector_triple a b c =
        cross a (cross b c)
