(* Copyright (C) 2012  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

module type Equatable = sig
    type t
    val equal : t -> t -> bool
end

module Poly : sig

    type prefix = Bitstring.t
    type 'a t

    val empty : 'a t
    val is_empty : 'a t -> bool

    val const : 'a -> 'a t
    val is_const : 'a t -> bool
    val to_const : 'a t -> 'a

    val lower_half : 'a t -> 'a t
    val upper_half : 'a t -> 'a t

    val unzoom : prefix -> 'a t -> 'a t
    val zoom : prefix -> 'a t -> 'a t

    val remove : prefix -> 'a t -> 'a t
    val intersect : prefix -> 'a t -> 'a t

    val card : 'a t -> int

    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val foldi : (prefix -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val iter : ('a -> unit) -> 'a t -> unit
    val iteri : (prefix -> 'a -> unit) -> 'a t -> unit
end

module Make (C : Equatable) : sig

    type prefix = Bitstring.t
    type codomain = C.t
    type t = codomain Poly.t

    val equal : t -> t -> bool
    val disjoint : t -> t -> bool

    val empty : t
    val is_empty : t -> bool

    val const : codomain -> t
    val is_const : t -> bool
    val to_const : t -> codomain

    val appose : t -> t -> t
    val lower_half : t -> t
    val upper_half : t -> t

    val unzoom : prefix -> t -> t
    val zoom : prefix -> t -> t

    val add : prefix -> codomain -> t -> t
    val remove : prefix -> t -> t
    val intersect : prefix -> t -> t
    val modify : prefix -> (t -> t) -> t -> t

    val card : t -> int

    val fold : (codomain -> 'a -> 'a) -> t -> 'a -> 'a
    val foldi : (prefix -> codomain -> 'a -> 'a) -> t -> 'a -> 'a

    val iter : (codomain -> unit) -> t -> unit
    val iteri : (prefix -> codomain -> unit) -> t -> unit

    val map : ('a -> codomain) -> 'a Poly.t -> t
    val mapi : (prefix -> 'a -> codomain) -> 'a Poly.t -> t

    val right_isecn : t -> t -> t
    val right_union : t -> t -> t

    val valid : t -> bool
end
