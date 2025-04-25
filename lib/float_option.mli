(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)
type t = private float

val of_option : float option -> t

val none : t
val some : float -> t

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
  type t = private Float.Array.t [@@deriving sexp]

  val of_option_list : float option list -> t
  val of_option_array_map : 'a option array -> f:('a -> float) -> t
end
