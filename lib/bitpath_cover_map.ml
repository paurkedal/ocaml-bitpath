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

open Bitpath_prereq

module type Equatable = sig
  type t
  val equal : t -> t -> bool
end

module Poly = struct

  type prefix = Bitpath.t

  type 'a t =
    | E
    | U of 'a
    | Y of 'a t * 'a t
    | P of prefix * 'a t

  let rec unzoom p m =
    if Bitpath.length p = 0 then m else
    (match m with
     | E -> E
     | P (p', m') -> P (Bitpath.cat p p', m')
     | _ -> P (p, m))

  let empty = E
  let is_empty = function E -> true | _ -> false

  let const x = U x
  let is_const = function U _ -> true | _ -> false
  let to_const = function
   | U x -> x
   | _ -> invalid_arg "Bitpath_cover_map.to_const"
  let value = function
   | U x -> Some x
   | _ -> None

  let picki_first m =
    let rec dive p = function
     | E -> raise Not_found
     | U c -> (p, c)
     | Y (m, _) -> dive (Bitpath.cat p Bitpath.c0) m
     | P (p', m') -> dive (Bitpath.cat p p') m' in
    dive Bitpath.empty m

  let picki_random m =
    let rec dive p = function
     | E -> raise Not_found
     | U c -> (p, c)
     | Y (m0, m1) ->
        if Random.bool ()
        then dive (Bitpath.cat p Bitpath.c1) m1
        else dive (Bitpath.cat p Bitpath.c0) m0
     | P (p', m') -> dive (Bitpath.cat p p') m' in
    dive Bitpath.empty m

  let lower_half = function
   | E -> E
   | U c -> U c
   | Y (m, _) -> m
   | P (p, m) ->
      if Bitpath.get p 0 then E else
      unzoom (Bitpath.suffix 1 p) m

  let upper_half = function
   | E -> E
   | U c -> U c
   | Y (_, m) -> m
   | P (p, m) ->
      if not (Bitpath.get p 0) then E else
      unzoom (Bitpath.suffix 1 p) m

  let rec zoom pG m =
    let nG = Bitpath.length pG in
    let rec loop iG m =
      if iG = nG then m else
      (match m with
       | E -> E
       | U x -> U x
       | Y (mL, mR) ->
          loop (iG + 1) (if Bitpath.get pG iG then mR else mL)
       | P (p', m') ->
          let n' = Bitpath.length p' in
          let n = Bitpath.coslice_length iG pG 0 p' in
          if n = n' then loop (iG + n) m' else
          if n = nG - iG then P (Bitpath.slice n n' p', m') else
          E) in
    loop 0 m

  let cover_find pG m =
    let nG = Bitpath.length pG in
    let rec loop iG = function
     | E -> raise Not_found
     | U x -> Bitpath.prefix iG pG
     | Y (m0, m1) ->
        if iG = nG then raise Not_found else
        loop (iG + 1) (if Bitpath.get pG iG then m1 else m0)
     | P (p, mI) ->
        if not (Bitpath.has_slice p iG pG) then raise Not_found else
        loop (iG + Bitpath.length p) mI in
    loop 0 m

  let rec disjoint mA mB = match mA, mB with
   | E, _ | _, E -> true
   | U _, _ | _, U _ -> false
   | Y (mA0, mA1), Y (mB0, mB1) -> disjoint mA0 mB0 && disjoint mA1 mB1
   | P (pA, mAI), _ -> disjoint mAI (zoom pA mB)
   | _, P (pB, mBI) -> disjoint (zoom pB mA) mBI

  let remove pG m =
    let nG = Bitpath.length pG in
    let rec remove_uniform m iG =
      if iG = nG then E else
      if Bitpath.get pG iG then Y (m, remove_uniform m (iG + 1))
                           else Y (remove_uniform m (iG + 1), m) in
    let rec loop iG m =
      if iG = nG then m else
      (match m with
       | E -> E
       | U c -> remove_uniform m iG
       | Y (mL, mR) ->
          if Bitpath.get pG iG then Y (mL, loop (iG + 1) mR)
                               else Y (loop (iG + 1) mL, mR)
       | P (p', m') ->
          if Bitpath.has_slice p' iG pG
          then unzoom p' (loop (iG + Bitpath.length p') m')
          else m) in
    loop 0 m

  let intersect p s = unzoom p (zoom p s)

  let rec fold f = function
   | E -> ident
   | U c -> f c
   | Y (m0, m1) -> fold f m1 |< fold f m0
   | P (_, m) -> fold f m

  let foldi f =
    let rec loop p = function
     | E -> ident
     | U c -> f p c
     | Y (m0, m1) -> loop (Bitpath.cat p Bitpath.c1) m1
                  |< loop (Bitpath.cat p Bitpath.c0) m0
     | P (pI, mI) -> loop (Bitpath.cat p pI) mI in
    loop Bitpath.empty

  let rec iter f = function
   | E -> ()
   | U c -> f c
   | Y (m0, m1) -> iter f m0; iter f m1
   | P (_, m) -> iter f m

  let iteri f =
    let rec loop p = function
     | E -> ()
     | U c -> f p c
     | Y (m0, m1) -> loop (Bitpath.cat p Bitpath.c0) m0;
                     loop (Bitpath.cat p Bitpath.c1) m1
     | P (pI, mI) -> loop (Bitpath.cat p pI) mI in
    loop Bitpath.empty

  let cover_card m = fold (fun _ -> (+) 1) m 0
end

module Make (C : Equatable) = struct

  open Poly

  type prefix = Poly.prefix
  type codomain = C.t
  type t = codomain Poly.t

  let disjoint = disjoint
  let empty = empty
  let is_empty = is_empty
  let const = const
  let is_const = is_const
  let to_const = to_const
  let value = value
  let picki_first = picki_first
  let picki_random = picki_random
  let lower_half = lower_half
  let upper_half = upper_half
  let unzoom = unzoom
  let zoom = zoom
  let cover_find = cover_find
  let remove = remove
  let intersect = intersect

  let rec valid = function
   | E -> true
   | U _ -> true
   | Y (E, _) | Y (_, E) -> false
   | Y (U cL, U cR) -> not (C.equal cL cR)
   | Y (m0, m1) -> valid m0 && valid m1
   | P (p, E) | P (p, P _) -> false
   | P (p, m) -> Bitpath.length p > 0 && valid m

  let rec equal ma mb = match ma, mb with
   | E, E -> true
   | U ca, U cb -> C.equal ca cb
   | Y (maL, maR), Y (mbL, mbR) -> equal maL mbL && equal maR mbR
   | P (pa, ma'), P (pb, mb') -> Bitpath.equal pa pb && equal ma' mb'
   | E, _ | U _, _ | Y _, _ | P _, _ -> false

  let appose mL mR = match mL, mR with
   | E, E -> E
   | U cL, U cR -> if C.equal cL cR then U cL else Y (mL, mR)
   | _, E -> unzoom (Bitpath.init 1 (konst false)) mL
   | E, _ -> unzoom (Bitpath.init 1 (konst true)) mR
   | _ -> Y (mL, mR)

  let rec modify pG f m =
    let nG = Bitpath.length pG in
    let rec loop iG m =
      if iG = nG then f m else
      (match m with
       | E | U _ ->
          if Bitpath.get pG iG then appose m (loop (iG + 1) m)
                               else appose (loop (iG + 1) m) m
       | Y (mL, mR) ->
          if Bitpath.get pG iG then appose mL (loop (iG + 1) mR)
                               else appose (loop (iG + 1) mL) mR
       | P (p', m') ->
          let n' = Bitpath.length p' in
          let n = Bitpath.coslice_length iG pG 0 p' in
          if n = n' then unzoom p' (loop (iG + n) m') else
          let m_new =
            if iG + n = nG then f (P (Bitpath.slice n n' p', m')) else
            let m_unm = unzoom (Bitpath.slice (n + 1) n' p') m' in
            let m_mod = unzoom (Bitpath.slice (iG+n+1) nG pG) (f E) in
            if Bitpath.get p' n then appose m_mod m_unm
                                else appose m_unm m_mod in
          unzoom (Bitpath.prefix n p') m_new) in
    loop 0 m

  let add p x = modify p (konst (U x))
  let remove p = modify p (konst E)

  let cover_card = cover_card
  let fold = fold
  let foldi = foldi
  let iter = iter
  let iteri = iteri

  let rec map f = function
   | E -> E
   | U c -> U (f c)
   | Y (m0, m1) -> appose (map f m0) (map f m1)
   | P (pI, mI) -> P (pI, map f mI)

  let mapi f =
    let rec loop p = function
     | E -> E
     | U c -> U (f p c)
     | Y (m0, m1) -> appose (loop (Bitpath.cat p Bitpath.c0) m0)
                            (loop (Bitpath.cat p Bitpath.c1) m1)
     | P (pI, mI) -> P (pI, loop (Bitpath.cat p pI) mI) in
    loop Bitpath.empty

  let rec right_isecn mA mB =
    (match mA, mB with
     | _, E | E, _ -> E
     | _, U c -> map (konst c) mA
     | U c, _ -> mB
     | Y (mA0, mA1), Y (mB0, mB1) ->
        appose (right_isecn mA0 mB0) (right_isecn mA1 mB1)
     | P (pI, mI), _ -> unzoom pI (right_isecn mI (zoom pI mB))
     | _, P (pI, mI) -> unzoom pI (right_isecn (zoom pI mA) mI))

  let rec right_union mA mB =
    (match mA, mB with
     | _, E -> mA
     | E, _ -> mB
     | _, U c -> U c
     | Y (mA0, mA1), Y (mB0, mB1) ->
        appose (right_union mA0 mB0) (right_union mA1 mB1)
     | U c, Y (m0, m1) ->
        appose (right_union (U c) m0) (right_union (U c) m1)
     | _, P (pI, mI) -> modify pI (fun m' -> right_union m' mI) mA
     | P (pI, mI), _ -> modify pI (fun m' -> right_union mI m') mB)
end
