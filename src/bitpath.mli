(* Copyright (C) 2012--2013  Petter Urkedal <paurkedal@gmail.com>
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

(** The [Bitpath] module implements strings of booleans.  The bits are
    compactly represented packed into native words.  Core functions are
    written in C to operate on full words. *)

(** The type of a bit-string, which represents zero or more consecutive
    boolean values, indexed from 0. *)
type t


external length : t -> int = "camlbitpath_length"
(** [length s] is the number of bits contained in [s]. *)

val length8 : t -> int
(** [length8 s = ⌈length s / 8⌉]. *)

val length16 : t -> int
(** [length16 s = ⌈length s / 16⌉]. *)

external get : int -> t -> bool = "camlbitpath_get"
(** [get i s] is true iff the [i]th bit of [s] is set. *)

external get8 : int -> t -> int = "camlbitpath_get8"
(** [get8 i s] is the integer with bit number [7 - j] equal to bit number
    [8*i + j] of [s]. *)

external get16 : int -> t -> int = "camlbitpath_get16"
(** [get16 i s] is the integer with bit number [15 - j] equal to bit number
    [16*i + j] of [s]. *)

external compare : t -> t -> int = "camlbitpath_compare"
(** [compare sA sB] returns [-1], [0], or [1] when [sA] is less, equal, or
    greater than [sB], respectively, according to lexicographical ordering of
    bits. *)

val equal : t -> t -> bool
(** [equal sA sB] is true iff [sA] and [sB] are bitwise equal. *)


val empty : t
(** The empty bit-string. *)

val is_empty : t -> bool
(** True on the empty path, elsewhere false. *)

external init : int -> (int -> bool) -> t = "camlbitpath_init"
(** [init n f] turns the predicate [f] into a bit-string [s] of length [n]
    such that [f i = get f i] for 0 ≤ [i] < [n]. *)

external init8 : int -> (int -> int) -> t = "camlbitpath_init8"
(** [init8 n f] is the [n]-bit string [s] such that [get (8*i + j) s] is
    [true] iff bit number [7 - j] of [f i] is set.  If [n] is not a multiple
    of [8], then the [n mod 8] most significant bits of [f (n / 8)] are used;
    the rest are ignored.  *)

external init16 : int -> (int -> int) -> t = "camlbitpath_init16"
(** [init16 n f] is the [n]-bit string [s] such that [get (16*i + j) s] is
    [true] iff bit number [15 - j] of [f i] is set.  If [n] is not a multiple
    of [16], then the [n mod 16] most significant bits of [f (n / 16)] are
    used; the rest are ignored. *)

external const : int -> bool -> t = "camlbitpath_const"
(** [const n x] is the [n]-bit string of [x]. *)

val c0 : t
(** The constant [const 1 false]. *)

val c1 : t
(** The constant [const 1 true]. *)


external bitnot : t -> t = "camlbitpath_not"
(** [bitnot s] is the string [s'] such that [get i s = not (get i s')]. *)

external slice : int -> int -> t -> t = "camlbitpath_slice"
(** [slice i j s] is the sub-string of bit [i] to [j - 1] of [s]. *)

val prefix : int -> t -> t
(** [prefix i s] is the substring containing the [i] first bits of [s]. *)

val suffix : int -> t -> t
(** [suffix i s] is the substring containing all but the [i] first bits
    of [s]. *)

val has_slice : t -> int -> t -> bool
(** [has_slice s0 i s] is true iff [s] contains a substring equal to [s0]
    starting at bit number [i]. *)

val has_prefix : t -> t -> bool
(** [has_prefix s0 s] is true iff [s] starts with [s0]. *)

val has_suffix : t -> t -> bool
(** [has_suffix s0 s] is true iff [s] ends with [s0]. *)

external coprefix_length : t -> t -> int
    = "camlbitpath_coprefix_length"
(** [coprefix_length sA sB] is the largest [n] such that
    [get i sA = get i sB] for [i < n]. *)

val coprefix : t -> t -> t

external coslice_length : int -> t -> int -> t -> int
    = "camlbitpath_coslice_length"
(** [coslice_length iA sA iB sB] is the largest [n] such that
    [get (iA + i) sA = get (iB + i) sB] for [i < n]. *)

external cat : t -> t -> t = "camlbitpath_cat"
(** [cat s0 s1] is the concatenation of [s0] and [s1]. *)

val map : (bool -> bool) -> t -> t
(** Given [s] of length [n], then [map f s] is the [n]-bit string [s'] such
    that [get i s' = f (get i s)] for [0 ≤ i < n]. *)

val mapi : (int -> bool -> bool) -> t -> t
(** Given [s] of length [n], then [mapi f s] is the [n]-bit string [s'] such
    that [get i s' = f i (get i s)] for [0 ≤ i < n]. *)

val fold : (bool -> 'a -> 'a) -> t -> 'a -> 'a
(** Given a string [s] of length [n], [fold f s] returns
    [f (get (n - 1) s) ∘ ⋯ ∘ f (get 0 s)]. *)

val foldi : (int -> bool -> 'a -> 'a) -> t -> 'a -> 'a
(** Given a string [s] of length [n], [foldi f s] is the function
    [f (n - 1) (get (n - 1) s) ∘ ⋯ ∘ f 0 (get 0 s)]. *)

val foldi8 : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a
(** Given a string [s] of length [n], [foldi8 f s] is the function
    [f (⌈n/8⌉ - 1) (get (⌈n/8⌉ - 1) s) ∘ ⋯ ∘ f 0 (get 0 s)]. *)

val foldi16 : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a
(** Given a string [s] of length [n], [foldi16 f s] is the function
    [f (⌈n/16⌉ - 1) (get (⌈n/16⌉ - 1) s) ∘ ⋯ ∘ f 0 (get 0 s)]. *)

val iter : (bool -> unit) -> t -> unit
(** [iter f s] calls [f] on each element of [s] in order. *)

val iteri : (int -> bool -> unit) -> t -> unit
(** [iteri f s] calls [f i (get i s)] for [0 ≤ i < length s] in order. *)

val iteri8 : (int -> int -> unit) -> t -> unit
(** [iteri8 f s] calls [f i (get8 i s)] for [0 ≤ i < length8 s] in order. *)

val iteri16 : (int -> int -> unit) -> t -> unit
(** [iteri16 f s] calls [f i (get16 i s)] for [0 ≤ i < length16 s] in order. *)

val of_array8 : int array -> t
(** [of_array8 xa] forms a bitpath by composing the octets from [xa], starting
    with the most significant. *)

val of_array16 : int array -> t
(** [of_array16 xa] forms a bitpath by composing hexadecatets of [xa], staring
    with the most significant. *)

val of_string : string -> t
(** [of_string s] interprets a string [s] of ['0'] and ['1'] characters as a
    bit string. *)

val to_string : t -> string
(** [to_string s] turns [s] to a textual representation of ['0'] and ['1']
    characters, starting with the lowest index. *)

(** [of_hex_string ~n str] returns the bitstring described by the hexadecimal
    string [str], clipped to [n] bits if specified. *)
val of_hex_string : ?n : int -> string -> t

(** [to_hex_string ~n s] is the hexadecimal encoding of [s] padded with zeros
    to the next quartet, or clipped or padded to [n] bits if specified. *)
val to_hex_string : ?n : int -> t -> string

(** [of_bin_string ~n s8] converts an octet string [s8] to a bit string [s1],
    where [get (8*i + j) s1] equals bit number [7 - j] of [String.get i s8].
    The result is clipped or padded with zeros to [n] bits. *)
val of_bin_string : ?n : int -> string -> t

(** [to_bin_string ~n s] converts a bit string to an octet string using the
    same representation as [of_bit_string].  The result it clipped or padded
    with zeros to [(n + 7) / 8] octets. *)
val to_bin_string : ?n : int -> t -> string
