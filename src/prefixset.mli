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

(** [Prefixset.t] models sets of infinitely long bit-strings which can be
    described by a finite set of prefixes common to the members.  These sets
    can also be considered subsets of unit interval \[0, 1\] in which case
    prefixes are the leading decimals of the binary representation of
    members.

    In the following we use [·] to represent concatenation of bit-strings
    along with regular set operations to describe modelled sets. *)

(** A prefix is represented by a [Bitstring]. *)
type prefix = Bitstring.t

(** The set type. *)
type t

(** [equal sA sB] is true iff [sA] and [sB] contain the same members. *)
val equal : t -> t -> bool

(** [disjoint sA sB] is true iff [sA ∩ sB] is the empty set. *)
val disjoint : t -> t -> bool

(** The empty set. *)
val empty : t

(** True on the empty set, false elsewhere. *)
val is_empty : t -> bool

(** The set containing all infinite bit-strings. *)
val universe : t

(** [is_universe s] is true iff [equal s universe]. *)
val is_universe : t -> bool

(** [of_prefix p] is the set of all strings starting with [p]. *)
val of_prefix : prefix -> t

(** True for sets [s] such that [equal s (of_prefix p)] for some [p]. *)
val is_prefix : t -> bool

(** If [is_prefix s], then [to_prefix s] returns the [p] such that [equal s
    (of_prefix p)], otherwise it raises [Invalid_argument]. *)
val to_prefix : t -> prefix

(** [appose sA sB] is the set whose lower and upper halves are [sA] and [sB]
    shrunk to half their size by prefixing their elements by [0] and [1],
    respectively. *)
val appose : t -> t -> t

(** [lower_half s] is the set [{p | 0·p ∈ s}]. *)
val lower_half : t -> t

(** [upper_half s] is the set [{p | 1·p ∈ s}]. *)
val upper_half : t -> t

(** [unzoom p s] is the set [{p·p' | p' ∈ s}]. *)
val unzoom : prefix -> t -> t

(** [zoom p s] is the set [{p' | p·p' ∈ s}]. *)
val zoom : prefix -> t -> t

(** [add p s] is the union [s ∪ of_prefix p]. *)
val add : prefix -> t -> t

(** [remove p s] is the relative complement [s ∖ of_prefix p]. *)
val remove : prefix -> t -> t

(** [intersect p s] is the intersection [s ∩ of_prefix p]. *)
val intersect : prefix -> t -> t

(** [modify f p s] is the set [s ∖ of_prefix p ∪ unzoom (f (zoom s))]. *)
val modify : prefix -> (t -> t) -> t -> t

(** [prefix_fold f s] is the function [f p_(n-1) ∘ ⋯ ∘ f p_0] where [{p_i}]
    is the minimal set of prefixes which cover [s]. *)
val prefix_fold : (prefix -> 'a -> 'a) -> t -> 'a -> 'a

(** [prefix_iter f s] calls [f] for each prefix in the minimal cover for
    [s]. *)
val prefix_iter : (prefix -> unit) -> t -> unit

(** [prefix_card s] is the cardinality of the minimal set of prefixes which
    cover of [s]. *)
val prefix_card : t -> int

(** [isecn sA sB] is the intersection [sA ∩ sB]. *)
val isecn : t -> t -> t

(** [union sA sB] is the union [sA ∪ sB]. *)
val union : t -> t -> t

(** [rel_compl sC sA] is the relative complement [sA ∖ sC]. *)
val rel_compl : t -> t -> t

(** [abs_compl s] is the absolute complement [∁ s]. *)
val abs_compl : t -> t

(** [compl_decomp s] returns a pair [(sC, sA)] such that [s] equals [sA ∖ sC].
    The two returned sets will generally have a simler representation than the
    original set. *)
val compl_decomp : t -> t * t

(**/**)
val valid : t -> bool
val dump : out_channel -> t -> unit
