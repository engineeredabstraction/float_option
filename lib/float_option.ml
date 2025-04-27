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

let equal a b =
  match%optional a, b with
  | None, None -> true
  | Some x, Some y -> Float.equal x y
  | _ -> false

let compare a b =
  match%optional a, b with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x, Some y -> Float.compare x y

let sexp_of_t (t : t) =
  match%optional t with
  | None -> Sexp.Atom "None"
  | Some x -> [%sexp_of: float] x

let t_of_sexp (s : Sexp.t) =
  match s with
  | Atom "None" -> none
  | _ -> some ([%of_sexp: float] s)

module Array = struct
  module A = Stdlib.Float.ArrayLabels
  type t = floatarray

  let all_some = A.for_all ~f:(fun x -> not (Optional_syntax.is_none x))
  let all_none = A.for_all ~f:Optional_syntax.is_none
  let unsafe_to_floatarray (a : t) = a

  let to_floatarray (a : t) =
    if all_some a
    then Some (unsafe_to_floatarray a)
    else None

  let length = A.length
  let get = A.get
  let set = A.set
  let unsafe_get = A.unsafe_get
  let unsafe_set = A.unsafe_set
  let make = A.make
  let create = A.create
  let init = A.init

  let append = A.append
  let concat = A.concat
  let sub = A.sub
  let copy = A.copy
  let fill = A.fill
  let blit = A.blit

  let iter = A.iter
  let iteri = A.iteri
  let map = A.map
  let map_inplace = A.map_inplace
  let mapi = A.mapi
  let mapi_inplace = A.mapi_inplace
  let fold_left = A.fold_left
  let fold_right = A.fold_right

  let iter2 = A.iter2
  let map2 = A.map2

  let for_all = A.for_all
  let exists = A.exists
  let mem v t = A.exists ~f:(fun x -> equal x v) t 

  let to_list = A.to_list
  let of_list = A.of_list

  let sexp_of_t (t : t) =
    A.map_to_array ~f:sexp_of_t t
    |> [%sexp_of: Sexp.t array]

  let t_of_sexp (s : Sexp.t) =
    [%of_sexp: Sexp.t array] s
    |> A.map_from_array ~f:t_of_sexp

  let of_option_list (l : float option list) =
    List.map of_option l
    |> A.of_list

  let of_option_array_map (a : 'a option array) ~f =
    A.map_from_array
      ~f:(function
        | None -> none
        | Some x -> some (f x))
      a
end
