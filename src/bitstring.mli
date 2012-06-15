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

(** The [Bitstring] module implements strings of booleans.  The bits are
    compactly represented packed into native words.  Core functions are
    written in C to operate on full words. *)

(** The type of a bit-string, which represents zero or more consecutive
    boolean values, indexed from 0. *)
type t


(** [length s] is the number of bits contained in [s]. *)
external length : t -> int = "camlbitlib_bitstring_length"

(** [length8 s = ⌈length s / 8⌉]. *)
val length8 : t -> int

(** [length16 s = ⌈length s / 16⌉]. *)
val length16 : t -> int

(** [get i s] is true iff the [i]th bit of [s] is set. *)
external get : int -> t -> bool = "camlbitlib_bitstring_get"

(** [get8 i s] is the integer with bit number [7 - j] equal to bit number
    [8*i + j] of [s]. *)
external get8 : int -> t -> int = "camlbitlib_bitstring_get8"

(** [get16 i s] is the integer with bit number [15 - j] equal to bit number
    [16*i + j] of [s]. *)
external get16 : int -> t -> int = "camlbitlib_bitstring_get16"

(** [compare sA sB] returns [-1], [0], or [1] when [sA] is less, equal, or
    greater than [sB], respectively, according to lexicographical ordering of
    bits. *)
external compare : t -> t -> int = "camlbitlib_bitstring_compare"

(** [equal sA sB] is true iff [sA] and [sB] are bitwise equal. *)
val equal : t -> t -> bool


(** The empty bit-string. *)
val empty : t

(** [init n f] turns the predicate [f] into a bit-string [s] of length [n]
    such that [f i = get f i] for 0 ≤ [i] < [n]. *)
external init : int -> (int -> bool) -> t = "camlbitlib_bitstring_init"

(** [init8 n f] is the [(8 * n)]-bit string [s] such that [get (8*i + j) s] is
    [true] iff bit number [7 - j] of [f i] is set. *)
external init8 : int -> (int -> int) -> t = "camlbitlib_bitstring_init8"

(** [init16 n f] is the [(16 * n)]-bit string [s] such that [get (16*i + j) s]
    is [true] iff bit number [15 - j] of [i i] is set. *)
external init16 : int -> (int -> int) -> t = "camlbitlib_bitstring_init16"

(** [const n x] is the [n]-bit string of [x]. *)
external const : int -> bool -> t = "camlbitlib_bitstring_const"


(** [bitnot s] is the string [s'] such that [get i s = not (get i s')]. *)
external bitnot : t -> t = "camlbitlib_bitstring_not"

(** [slice i j s] is the sub-string of bit [i] to [j - 1] of [s]. *)
external slice : int -> int -> t -> t = "camlbitlib_bitstring_slice"

(** [prefix i s] is the substring containing the [i] first bits of [s]. *)
val prefix : int -> t -> t

(** [suffix i s] is the substring containing all but the [i] first bits
    of [s]. *)
val suffix : int -> t -> t

(** [has_slice s0 i s] is true iff [s] contains a substring equal to [s0]
    starting at bit number [i]. *)
val has_slice : t -> int -> t -> bool

(** [has_prefix s0 s] is true iff [s] starts with [s0]. *)
val has_prefix : t -> t -> bool

(** [has_suffix s0 s] is true iff [s] ends with [s0]. *)
val has_suffix : t -> t -> bool

(** [coprefix_length sA sB] is the largest [n] such that
    [get i sA = get i sB] for [i < n]. *)
external coprefix_length : t -> t -> int
    = "camlbitlib_bitstring_coprefix_length"

val coprefix : t -> t -> t

(** [coslice_length iA sA iB sB] is the largest [n] such that
    [get (iA + i) sA = get (iB + i) sB] for [i < n]. *)
external coslice_length : int -> t -> int -> t -> int
    = "camlbitlib_bitstring_coslice_length"

(** [cat s0 s1] is the concatenation of [s0] and [s1]. *)
external cat : t -> t -> t = "camlbitlib_bitstring_cat"

(** Given [s] of length [n], then [map f s] is the [n]-bit string [s'] such
    that [get i s' = f (get i s)] for [0 ≤ i < n]. *)
val map : (bool -> bool) -> t -> t

(** Given [s] of length [n], then [mapi f s] is the [n]-bit string [s'] such
    that [get i s' = f i (get i s)] for [0 ≤ i < n]. *)
val mapi : (int -> bool -> bool) -> t -> t

(** Given a string [s] of length [n], [fold f s] returns
    [f (get (n - 1) s) ∘ ⋯ ∘ f (get 0 s)]. *)
val fold : (bool -> 'a -> 'a) -> t -> 'a -> 'a

(** Given a string [s] of length [n], [foldi f s] is the function
    [f (n - 1) (get (n - 1) s) ∘ ⋯ ∘ f 0 (get 0 s)]. *)
val foldi : (int -> bool -> 'a -> 'a) -> t -> 'a -> 'a

(** Given a string [s] of length [n], [foldi8 f s] is the function
    [f (⌈n/8⌉ - 1) (get (⌈n/8⌉ - 1) s) ∘ ⋯ ∘ f 0 (get 0 s)]. *)
val foldi8 : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a

(** Given a string [s] of length [n], [foldi16 f s] is the function
    [f (⌈n/16⌉ - 1) (get (⌈n/16⌉ - 1) s) ∘ ⋯ ∘ f 0 (get 0 s)]. *)
val foldi16 : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a

(** [iter f s] calls [f] on each element of [s] in order. *)
val iter : (bool -> unit) -> t -> unit

(** [iteri f s] calls [f i (get i s)] for [0 ≤ i < length s] in order. *)
val iteri : (int -> bool -> unit) -> t -> unit

(** [of_string s] interprets a string [s] of ['0'] and ['1'] characters as a
    bit string. *)
val of_string : string -> t

(** [to_string s] turns [s] to a textual representation of ['0'] and ['1']
    characters, starting with the lowest index. *)
val to_string : t -> string
