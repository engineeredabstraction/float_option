(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)
type t = private float

val of_option : float option -> t

val none : t
val some : float -> t

val equal : t -> t -> bool
val compare : t -> t -> int

val value : default:float -> t -> float

val zero : t
val one : t
val minus_one : t

val pi : t

val epsilon_float : t

val max_finite_value : t

module Infix : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( % ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( ~- ) : t -> t
end

include module type of Infix

module Optional_syntax : sig
  module Optional_syntax : sig
    type value := float
    val is_none : t -> bool
    val unsafe_value : t -> value
  end
end

module Array : sig
  type value := t
  type t = private Float.Array.t [@@deriving sexp]

  val length : t -> int
  val get : t -> int -> value
  val set : t -> int -> value -> unit
  val make : int -> value -> t
  val create : int -> t
  val init : int -> f:(int -> value) -> t

  val append : t -> t -> t
  val concat : t list -> t
  val sub : t -> pos:int -> len:int -> t
  val copy : t -> t
  val fill : t -> pos:int -> len:int -> value -> unit
  val blit : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit

  val to_list : t -> value list
  val of_list : value list -> t

  val iter : f:(value -> unit) -> t -> unit
  val iteri : f:(int -> value -> unit) -> t -> unit
  val map : f:(value -> value) -> t -> t
  val map_inplace : f:(value -> value) -> t -> unit
  val mapi : f:(int -> value -> value) -> t -> t
  val mapi_inplace : f:(int -> value -> value) -> t -> unit
  val fold_left : f:('acc -> value -> 'acc) -> init:'acc -> t -> 'acc
  val fold_right : f:(value -> 'acc -> 'acc) -> t -> init:'acc -> 'acc

  val iter2 : f:(value -> value -> unit) -> t -> t -> unit
  val map2 : f:(value -> value -> value) -> t -> t -> t

  val for_all : f:(value -> bool) -> t -> bool
  val exists : f:(value -> bool) -> t -> bool
  val mem : value -> t -> bool

  val of_option_list : float option list -> t
  val of_option_array_map : 'a option array -> f:('a -> float) -> t
end
