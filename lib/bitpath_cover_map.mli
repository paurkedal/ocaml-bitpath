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

(** The [Bitpath_cover_map] implements a mappings from sets of infinite
    bit-strings, representable by a finite set of common prefixes.  In other
    words, the domain has the same structure as the sets modelled by
    [Bitpath_cover].  Both polymorphic and a functor variants are provided to
    abstract over the codomain, though the former only contains the part of
    the API which do not depend on equality. *)

(** The signature for types with an equality predicate. *)
module type Equatable = sig
  type t
  val equal : t -> t -> bool
end

(** The polymorphic API to prefix-maps contains the subset of the operations
    which do not depend on equality on the codomain.  Note that the API is
    really incomplete, and must be combined with the functor API.  This is
    made possible by the type equality [Make(C).t = C.t Poly.t].  The purpose
    of this module is to allow writing polymorphic functions acting on
    pre-constructed maps.

    See {!Make} for documentation. *)
module Poly : sig

  type prefix = Bitpath.t
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool

  val const : 'a -> 'a t
  val is_const : 'a t -> bool
  val value : 'a t -> 'a option
  val picki_first : 'a t -> prefix * 'a
  val picki_random : 'a t -> prefix * 'a

  val lower_half : 'a t -> 'a t
  val upper_half : 'a t -> 'a t

  val unzoom : prefix -> 'a t -> 'a t
  val zoom : prefix -> 'a t -> 'a t
  val cover_find : prefix -> 'a t -> prefix

  val remove : prefix -> 'a t -> 'a t
  val intersect : prefix -> 'a t -> 'a t

  val cover_card : 'a t -> int

  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val foldi : (prefix -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (prefix -> 'a -> unit) -> 'a t -> unit

  (**/**)
  val to_const : 'a t -> 'a [@@deprecated "Use value."]
end

(** A functor which implements the full API of prefix-maps, given an equatable
    type for the codomain. *)
module Make (C : Equatable) : sig

  type prefix = Bitpath.t
  type codomain = C.t
  type t = codomain Poly.t

  val equal : t -> t -> bool
  (** [equal mA mB] is true iff for any prefix [p] such that [zoom p sA] and
      [zoom p sB] are both constant, [C.equal (to_const p mA)
      (to_const p mB)]. *)

  val disjoint : t -> t -> bool
  (** [disjoint mA mB] is true iff [mA] and [mB] have no overlapping keys. *)

  val empty : t
  (** [empty] is the unique map fulfilling [is_empty empty]. Also, [equal (zoom
      p empty) empty] for any prefix [p]. *)

  val is_empty : t -> bool
  (** [is_empty m] is true iff [m] is the unique empty map. *)

  val const : codomain -> t
  (** [const v] is the unique map [m] such that [is_const m] and [to_const m =
      v]. Also, [equal (zoom p m) m] for any prefix [p]. *)

  val is_const : t -> bool
  (** [is_const m] iff [equal m (const v)] for some [v]. *)

  val value : t -> codomain option
  (** [value v] is [Some v] if [equal m (const v)] for some [v], otherwise
      [None]. *)

  val picki_first : t -> prefix * codomain
  val picki_random : t -> prefix * codomain

  val appose : t -> t -> t
  (** [appose mA mB] is [{0·p ↦ x | p ↦ x ∈ mA} ∪ {1·p ↦ x | p ↦ x ∈ mB}] *)

  val lower_half : t -> t
  (** [lower_half m] is [{p ↦ x | 0·p ↦ x ∈ m}]. *)

  val upper_half : t -> t
  (** [upper_half m] is [{p ↦ x | 1·p ↦ x ∈ m}]. *)

  val unzoom : prefix -> t -> t
  (** [unzoom p m] is [{p·p' ↦ x | p' ↦ x ∈ m}]. *)

  val zoom : prefix -> t -> t
  (** [zoom p m] is [{p' ↦ x | p·p' ↦ x ∈ m}]. *)

  val cover_find : prefix -> t -> prefix

  val add : prefix -> codomain -> t -> t
  (** [add p x m] is the map [m] with keys starting with [p] replaced by a
      uniform mapping to [x]. *)

  val remove : prefix -> t -> t
  (** [remove p m] is the restriction of [m] to keys not starting with [p]. *)

  val intersect : prefix -> t -> t
  (** [intersect p m] is the restriction of [m] to keys starting with [p]. *)

  val modify : prefix -> (t -> t) -> t -> t
  (** [modify p f m] agrees with [m] everywhere except that the submap [m']
      under [p] is replaced by [f m']. *)

  val cover_card : t -> int
  (** [cover_card m] is the mimimum number of pairs [(pi, xi)] such that [m =
      {pi ↦ xi | i}]. *)

  val fold : (codomain -> 'a -> 'a) -> t -> 'a -> 'a
  val foldi : (prefix -> codomain -> 'a -> 'a) -> t -> 'a -> 'a

  val iter : (codomain -> unit) -> t -> unit
  val iteri : (prefix -> codomain -> unit) -> t -> unit

  val map : ('a -> codomain) -> 'a Poly.t -> t
  val mapi : (prefix -> 'a -> codomain) -> 'a Poly.t -> t

  val right_isecn : t -> t -> t
  val right_union : t -> t -> t

  (**/**)
  val valid : t -> bool
  val to_const : t -> codomain [@@deprecated "Use value."]
end
