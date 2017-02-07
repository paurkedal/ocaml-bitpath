(* Copyright (C) 2012--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Utilities and overlays for standard library structures used by the Bitpath
    library itself.

    This is not meant to provide a complete supplement to the standard library
    as it is mainly for internal needs.  Use at your own discretion. *)

val ident : 'a -> 'a
val konst : 'a -> 'b -> 'a
val ( |< ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

module Option : sig
  type 'a t = 'a option

  val default : 'a -> 'a option -> 'a
end

module Char : sig
  include module type of Char

  val hexdigit_of_int : int -> char
  val hexdigit_to_int : char -> int
end
