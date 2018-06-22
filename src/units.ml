(*
 @file
 @brief Definition of SI base units and constants

 Definition of SI base units and constants used elsewhere in the code.
 Partly inherited from CRPropa3.
 Based on:
 - CODATA recommended values of the fundamental physical constants: 2006
        doi:10.1103/RevModPhys.80.633
 - IAU 2012 Resolution B2, IAU 2015 Resolution B2
        https://www.iau.org/administration/resolutions/
*)

type dim_vector = {
        metre   : float;
        second  : float;
        kilogram: float;
        ampere  : float;
        kelvin  : float;
        mole    : float;
        candela : float;
        factor  : float;
}

type quantity = {
        value   : float;
        unit    : dim_vector
}

type q_variant =
        | Dim_vector of dim_vector
        | Quantity of quantity

let dim_vector_to_list a = [
        a.metre; a.second; a.kilogram; a.ampere; a.kelvin; a.mole; a.candela; 
]

(* operation on units *)

let null_dim_vector = { metre = 0.; second = 0.; kilogram = 0.;
        ampere = 0.; kelvin = 0.; mole = 0.; candela = 0.; factor = 1. }

let ( *! ) a b = {
        metre   = a.metre +. b.metre;
        second  = a.second +. b.second;
        kilogram= a.kilogram +. b.kilogram;
        ampere  = a.ampere +. b.ampere;
        kelvin  = a.kelvin +. b.kelvin;
        mole    = a.mole +. b.mole;
        candela = a.candela +. b.candela;
        factor  = a.factor *. b.factor;
}

let ( /! ) a b = {
        metre   = a.metre -. b.metre;
        second  = a.second -. b.second;
        kilogram= a.kilogram -. b.kilogram;
        ampere  = a.ampere -. b.ampere;
        kelvin  = a.kelvin -. b.kelvin;
        mole    = a.mole -. b.mole;
        candela = a.candela -. b.candela;
        factor  = a.factor /. a.factor;
}

let ( **! ) a pow = {
        metre   = a.metre *. pow;
        second  = a.second *. pow;
        kilogram= a.kilogram *. pow;
        ampere  = a.ampere *. pow;
        kelvin  = a.kelvin *. pow;
        mole    = a.mole *. pow;
        candela = a.candela *.pow;
        factor  = a.factor ** pow;
}

let ( *@ ) scalar a = {
        metre   = a.metre;
        second  = a.second;
        kilogram= a.kilogram;
        ampere  = a.ampere;
        kelvin  = a.kelvin;
        mole    = a.mole;
        candela = a.candela;
        factor  = a.factor *. scalar;
}

let ( /@ ) scalar a = {
        metre   = a.metre;
        second  = a.second;
        kilogram= a.kilogram;
        ampere  = a.ampere;
        kelvin  = a.kelvin;
        mole    = a.mole;
        candela = a.candela;
        factor  = scalar /. a.factor;
}


(* base SI units *)

let metre = { metre = 1.; second = 0.; kilogram = 0.;
        ampere = 0.; kelvin = 0.; mole = 0.; candela = 0.; factor = 1. }

let second = { metre = 0.; second = 1.; kilogram = 0.;
        ampere = 0.; kelvin = 0.; mole = 0.; candela = 0.; factor = 1. }

let kilogram = { metre = 0.; second = 0.; kilogram = 1.;
        ampere = 0.; kelvin = 0.; mole = 0.; candela = 0.; factor = 1. }

let ampere = { metre = 0.; second = 0.; kilogram = 0.;
        ampere = 1.; kelvin = 0.; mole = 0.; candela = 0.; factor = 1. }

let kelvin = { metre = 0.; second = 0.; kilogram = 0.;
        ampere = 0.; kelvin = 1.; mole = 0.; candela = 0.; factor = 1. }

let mole = { metre = 0.; second = 0.; kilogram = 0.;
        ampere = 0.; kelvin = 0.; mole = 1.; candela = 0.; factor = 1. }

let candela = { metre = 0.; second = 0.; kilogram = 0.;
        ampere = 0.; kelvin = 0.; mole = 0.; candela = 1.; factor = 1. }

(* derived SI units *)

let newton      = kilogram *! metre /! (second **! 2.0)
let pascal      = newton /! (metre **! 2.0)
let joule       = newton *! metre
let tesla       = newton /! ampere /! metre
let volt        = kilogram *! (metre **! 2.0) /! ampere /! (second **! 3.0)
let coulomb     = ampere *! second
let hertz       = second **! (-1.0)
let watt        = ampere *! volt
let ohm         = volt /! ampere

(* physical constants *)

let eplus       = 1.602176487e-19 *@ ampere *! second
let c_light     = 2.99792458e8 *@ metre /! second
let c_squared   = c_light **! 2.0
let amu         = 1.660538921e-27 *@ kilogram
let m_proton    = 1.67262158e-27 *@ kilogram
let m_neutron   = 1.67492735e-27 *@ kilogram
let m_electron  = 9.10938291e-31 *@ kilogram
let h_planck    = 6.62606957e-34 *@ joule *! second
let k_boltzmann = 1.3806488e-23 *@ joule /! kelvin
let mu0         = (4. *. Lgo_math.pi *. 1e-7) *@ newton /! (ampere **! 2.0)
let epsilon0    = 1.0 /@ mu0 /! c_squared *! ampere *! second /! volt /! metre

(* time and mass *)

let minute      = 60. *@ second
let hour        = 60. *@ minute
let day         = 24. *@ hour
let week        = 7.  *@ day
let year        = 365.25 *@ day

let gram        = 0.001 *@ kilogram
let tonne       = 1000. *@ kilogram
let pound       = 0.45359237 *@ kilogram

(* other units *)

let gauss       = 1e-4 *@ tesla
let electronvolt= eplus *! joule

(* astronomical distances *)
let au          = 149597870700.0 *@ metre
let lightyear   = year *! c_light
let parsec      = 648000.0 /. Lgo_math.pi *@ au

(* SI prefixes *)

let yocto x = 1e-24 *@ x
let zepto x = 1e-21 *@ x
let atto  x = 1e-18 *@ x
let femto x = 1e-15 *@ x
let pico  x = 1e-12 *@ x
let nano  x = 1e-9  *@ x
let micro x = 1e-6  *@ x
let mili  x = 1e-3  *@ x
let centi x = 1e-2  *@ x
let deci  x = 1e-1  *@ x
let deca  x = 1e1   *@ x
let hecto x = 1e2   *@ x 
let kilo  x = 1e3   *@ x
let mega  x = 1e6   *@ x
let giga  x = 1e9   *@ x
let tera  x = 1e12  *@ x
let peta  x = 1e15  *@ x
let exa   x = 1e18  *@ x
let zetta x = 1e21  *@ x
let yotta x = 1e24  *@ x
