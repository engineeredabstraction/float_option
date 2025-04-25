(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)
open Sexplib0
open Sexplib0.Sexp_conv

type t = float

let none = Float.nan

let some x =
  assert (not (Float.is_nan x));
  x

let of_option = function
  | None -> none
  | Some x -> some x

let zero = 0.0
let one = 1.0
let minus_one = -1.0

let pi = Float.pi

let epsilon_float = Float.epsilon

let max_finite_value = Float.max_float

module Infix = struct
  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( * ) = ( *. )
  let ( / ) = ( /. )
  let ( % ) = ( mod_float )
  let ( ** ) = ( ** )
  let ( ~- ) = ( ~-. )
end

include Infix

module Optional_syntax = struct
  module Optional_syntax = struct
    let is_none = Float.is_nan
    let unsafe_value t = t
  end
end

open Optional_syntax

let value ~default t =
  match%optional t with
  | None -> default
  | Some x -> x

let sexp_of_t (t : t) =
  match%optional t with
  | None -> Sexp.Atom "None"
  | Some x -> [%sexp_of: float] x

let t_of_sexp (s : Sexp.t) =
  match s with
  | Atom "None" -> none
  | _ -> some ([%of_sexp: float] s)

module Array = struct
  module A = Stdlib.Float.Array
  type t = floatarray

  let sexp_of_t (t : t) =
    A.map_to_array sexp_of_t t
    |> [%sexp_of: Sexp.t array]

  let t_of_sexp (s : Sexp.t) =
    [%of_sexp: Sexp.t array] s
    |> A.map_from_array t_of_sexp

  let of_option_list (l : float option list) =
    List.map of_option l
    |> A.of_list

  let of_option_array_map (a : 'a option array) ~f =
    A.map_from_array
      (function
        | None -> none
        | Some x -> some (f x))
      a
end
