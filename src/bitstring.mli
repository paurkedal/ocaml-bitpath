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

type t

val empty : t

external init : int -> (int -> bool) -> t = "bitlib_bitstring_init"
external init8 : int -> (int -> int) -> t = "bitlib_bitstring_init8"
external init16 : int -> (int -> int) -> t = "bitlib_bitstring_init16"

external const : int -> bool -> t = "bitlib_bitstring_const"

external length : t -> int = "bitlib_bitstring_length"
val length8 : t -> int
val length16 : t -> int

external get : int -> t -> bool = "bitlib_bitstring_get"
external get8 : int -> t -> int = "bitlib_bitstring_get8"
external get16 : int -> t -> int = "bitlib_bitstring_get16"

external compare : t -> t -> int = "bitlib_bitstring_compare"
val equal : t -> t -> bool

external slice : int -> int -> t -> t = "bitlib_bitstring_slice"
val prefix : int -> t -> t
val suffix : int -> t -> t

val has_prefix : t -> t -> bool

external coprefix_length : t -> t -> int = "bitlib_bitstring_coprefix_length"
val coprefix : t -> t -> t
external coslice_length : int -> t -> int -> t -> int
    = "bitlib_bitstring_coslice_length"

external cat : t -> t -> t = "bitlib_bitstring_cat"

val map : (bool -> bool) -> t -> t

val mapi : (int -> bool -> bool) -> t -> t

val fold : (bool -> 'a -> 'a) -> t -> 'a -> 'a

val foldi : (int -> bool -> 'a -> 'a) -> t -> 'a -> 'a
val foldi8 : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a
val foldi16 : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a

val iter : (bool -> unit) -> t -> unit

val iteri : (int -> bool -> unit) -> t -> unit

val of_string : string -> t
val to_string : t -> string
