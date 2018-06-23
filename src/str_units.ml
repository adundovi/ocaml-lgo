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
        "J", joule;
        "eV", electronvolt;
        "Hz", hertz;
        "Pa", pascal;
        "V", volt;
        "W", watt;
        "C", coulomb;
        "pc", parsec;
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
        let rec print_unit = function
                | [] -> ""
                | hd::rest -> (Printf.sprintf "%s" hd) ^ (print_unit rest) in 
        let base_units = ["m"; "s"; "kg"; "A"; "K"; "mol"; "cd"] in
        let make_unit (unit, power) =
                match power with
                | 0.0 -> None
                | 1.0 -> Some unit
                | x -> Some (unit ^ "^" ^ (string_of_float x))
        in
        List.map ~f:make_unit (List.zip_exn base_units (dim_vector_to_list vec))
        |> List.filter_opt
        |> List.intersperse ~sep:"*"
        |> print_unit

let stringify a =
        let print value unit =
                Printf.sprintf "%.8g %s" value unit in
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

let ( *$ ) a b =
        { value = (a.value *. b.value); unit = (a.unit *! b.unit) }

let ( /$ ) a b =
        { value = (a.value /. b.value); unit =(a.unit /! b.unit) }

let convert a ~to_unit:str =
        let quotient = (operation_on_quantities_exn ~op:( /. ) a (q 1. str)).value in
        Printf.sprintf "%.8g %s" quotient str
