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

type prefix = Bitstring.t
type t

val equal : t -> t -> bool

val empty : t
val is_empty : t -> bool

val universe : t
val is_universe : t -> bool

val of_prefix : prefix -> t
val is_prefix : t -> bool
val to_prefix : t -> prefix

val appose : t -> t -> t
val lower_half : t -> t
val upper_half : t -> t

val unzoom : prefix -> t -> t
val zoom : prefix -> t -> t

val add : prefix -> t -> t
val remove : prefix -> t -> t
val intersect : prefix -> t -> t
val modify : prefix -> (t -> t) -> t -> t

val prefix_fold : (prefix -> 'a -> 'a) -> t -> 'a -> 'a
val prefix_iter : (prefix -> unit) -> t -> unit
val prefix_card : t -> int

val isecn : t -> t -> t
val union : t -> t -> t
val rel_compl : t -> t -> t
val abs_compl : t -> t

val valid : t -> bool
val dump : out_channel -> t -> unit

val compl_decomp : t -> t * t
