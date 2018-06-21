open Units
open Lgo_string
open Core

let apply_prefixes (str, f) =
        Printf.printf "%s" str;
        let make_unit_with_prefix (prefix_name, prefix) =
                (prefix_name ^ str, (prefix f)) in
        let prefixes_SI_list = [
                "y", yocto;
                "z", zepto;
                "a", atto;
                "f", femto;
                "p", pico;
                "n", nano;
                "mu", micro;
                "m", mili;
                "c", centi;
                "d", deci;
                "",  (fun x -> x);
                "da", deca;
                "h", hecto;
                "k", kilo;
                "M", mega;
                "G", giga;
                "T", tera;
                "P", peta;
                "E", exa;
                "Z", zetta;
                "Y", yotta;
        ] in
        List.map ~f:make_unit_with_prefix prefixes_SI_list

let unit_str_list = [
        "m", metre;
        "K", kelvin;
        "A", ampere;
        "N", newton;
        "Hz", hertz
]        

let unit_str_list_special = [
        "g", gram;
        "kg", kilogram;
        "t", tonne;
        "s", second;
        "min", minute;
        "h", hour;
        "day", day;
        "week", week;
        "yr", year;
        "ly", lightyear;
]

let units_with_prefixes =
        List.concat (List.map ~f:apply_prefixes unit_str_list)

let unit_mapping = String.Map.of_alist_exn (units_with_prefixes @ unit_str_list_special)

let unit_to_dim_vector unit exp =
        match (Map.find unit_mapping unit) with
        | None -> None
        | Some u -> Some (u **! exp) 

let str_to_dim_vector str =
        match String.split_on_chars ~on:[ '^' ] str with
                | base :: exp :: [] ->  unit_to_dim_vector base (float_of_string exp)
                | base :: [] -> unit_to_dim_vector base 1.0
                | _ -> None

let correct_division_exn str =
        match split_on_first_string ~on:"/" str with
        | numerator :: denominator :: [] -> numerator ^ "*" ^ denominator ^ "^-1"
        | numerator :: [] -> numerator
        | [] -> ""
        | _ -> failwith "Multiple slashes are not (yet) implemented."

let units_to_dim_vectors_exn str =
        String.split_on_chars ~on:[ '*' ] str |>
        (* List.map ~f:correct_division |> *)
        List.map ~f:str_to_dim_vector |>
        List.map ~f:(function | Some x -> x | None -> failwith ("Unkown unit:" ^ str)) |>
        List.fold_left ~init:null_dim_vector ~f:( *! )  

let q value str =
        let dim_vector =
                units_to_dim_vectors_exn str in 
        {
                value = value *. dim_vector.factor;
                unit  = { dim_vector with factor = 1.0 }
        }

let dim_vector_to_unit_str vec =
        let format_unit_power pow unit =
               if pow = 0.0 then "" else
               if pow = 1.0 then unit
               else unit ^ "^" ^ (string_of_float pow)
        in
        Printf.sprintf "%s%s%s%s%s%s%s"
                (format_unit_power vec.metre "m")
                (format_unit_power vec.second "s")
                (format_unit_power vec.kilogram "kg")
                (format_unit_power vec.ampere "A")
                (format_unit_power vec.kelvin "K")
                (format_unit_power vec.mole "mol")
                (format_unit_power vec.candela "cd")

let stringify a =
        let print value unit =
                Printf.sprintf "%f %s" value unit in
        print a.value (dim_vector_to_unit_str a.unit) 


let operation_on_quantities_exn ~op a b =
        match a.unit = b.unit with
        | true -> { value = (op a.value b.value); unit = a.unit }
        | false -> failwith ("Incomparable units: "
                                 ^ (stringify a)
                                 ^ "!=" ^ (stringify b))

let ( +$ ) a b =
        operation_on_quantities_exn ~op:( +. ) a b

let ( -$ ) a b =
        operation_on_quantities_exn ~op:( -. ) a b
