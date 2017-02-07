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

(* TODO:  Some operations using Bitpath.cat below and in Bitpath_cover_map
 * could be optimized using a new Bitbuffer module, though it's probably minor
 * in most cases, as bitpaths used here will typically be of limited length. *)

open Bitpath_prereq

type prefix = Bitpath.t

let get_bit n x = (x lsr n) land 1 = 1

let lowmasked n x = x land ((1 lsl n) - 1)

type t =
   | Bot | Top
   | Y of t * t
   | P of Bitpath.t * t

(* Canonicalizing variant of P. *)
let rec unzoom p s =
    if Bitpath.length p = 0 then s else
    match s with
      | Bot -> Bot
      | P (p', s') -> P (Bitpath.cat p p', s')
      | _ -> P (p, s)

(* Canonicalizing variant of Y. *)
let appose sL sR = match sL, sR with
  | Top, Top -> Top
  | Bot, Bot -> Bot
  | _, Bot -> unzoom (Bitpath.init 1 (konst false)) sL
  | Bot, _ -> unzoom (Bitpath.init 1 (konst true)) sR
  | _ -> Y (sL, sR)

let rec valid = function
  | Top -> true
  | Bot -> true
  | Y (Bot, _) | Y (_, Bot) -> false
  | Y (Top, Top) -> false
  | Y (s0, s1) -> valid s0 && valid s1
  | P (p, Bot) -> false
  | P (p, P _) -> false
  | P (p, s) -> Bitpath.length p > 0 && valid s

let rec equal sL sR = match sL, sR with
  | Top, Top -> true
  | Bot, Bot -> true
  | Y (sL0, sL1), Y (sR0, sR1) -> equal sL0 sR0 && equal sL1 sR1
  | P (pL, sL0), P (pR, sR0) -> Bitpath.equal pL pR && equal sL0 sR0
  | Top, _ | Bot, _ | Y _, _ | P _, _ -> false

let empty = Bot
let is_empty = function Bot -> true | _ -> false

let universe = Top
let is_universe = function Top -> true | _ -> false

let of_prefix p =
    if Bitpath.length p = 0 then Top else
    P (p, Top)
let is_prefix = function
  | Top -> true
  | P (p, Top) -> true
  | _ -> false
let to_prefix = function
  | Top -> Bitpath.empty
  | P (p, Top) -> p
  | _ -> invalid_arg "Bitpath.to_prefix: Not a basis set."

let pick_first =
    let rec dive p = function
      | Bot -> raise Not_found
      | Top -> p
      | Y (s0, s1) -> dive (Bitpath.cat p Bitpath.c0) s0
      | P (p', s') -> dive (Bitpath.cat p p') s' in
    dive Bitpath.empty

let pick_random =
    let rec dive p = function
      | Bot -> raise Not_found
      | Top -> Bitpath.empty
      | Y (s0, s1) ->
        if Random.bool ()
        then dive (Bitpath.cat p Bitpath.c1) s1
        else dive (Bitpath.cat p Bitpath.c0) s0
      | P (p', s') -> dive (Bitpath.cat p p') s' in
    dive Bitpath.empty

let lower_half = function
  | Bot -> Bot | Top -> Top
  | Y (s, _) -> s
  | P (p, s) -> if Bitpath.get p 0 then Bot else
                unzoom (Bitpath.suffix 1 p) s

let upper_half = function
  | Bot -> Bot | Top -> Top
  | Y (_, s) -> s
  | P (p, s) -> if not (Bitpath.get p 0) then Bot else
                unzoom (Bitpath.suffix 1 p) s

let zoom pG s =
    let nG = Bitpath.length pG in
    let rec loop iG s =
        if iG = nG then s else
        match s with
          | Bot -> Bot
          | Top -> Top
          | Y (s0, s1) ->
            loop (iG + 1) (if Bitpath.get pG iG then s1 else s0)
          | P (p0, s0) ->
            let n0 = Bitpath.length p0 in
            let n = Bitpath.coslice_length iG pG 0 p0 in
            if n = n0 then loop (iG + n) s0 else
            if n = nG - iG then P (Bitpath.slice n n0 p0, s0) else
            Bot in
    loop 0 s

let cover_find pG s =
    let nG = Bitpath.length pG in
    let rec loop iG = function
      | Bot -> raise Not_found
      | Top -> Bitpath.prefix iG pG
      | Y (s0, s1) ->
        if iG = nG then raise Not_found else
        loop (iG + 1) (if Bitpath.get pG iG then s1 else s0)
      | P (p, sI) ->
        if not (Bitpath.has_slice p iG pG) then raise Not_found else
        loop (iG + Bitpath.length p) sI in
    loop 0 s

let rec disjoint sA sB = match sA, sB with
  | Bot, _ | _, Bot -> true
  | Top, _ | _, Top -> false
  | Y (sA0, sA1), Y (sB0, sB1) -> disjoint sA0 sB0 && disjoint sA1 sB1
  | P (pA, sAI), _ -> disjoint sAI (zoom pA sB)
  | _, P (pB, sBI) -> disjoint (zoom pB sA) sBI

let rec modify pG f s =
    let nG = Bitpath.length pG in
    let rec loop iG s =
        if iG = nG then f s else
        match s with
          | Bot | Top ->
            if Bitpath.get pG iG then appose s (loop (iG + 1) s)
                                 else appose (loop (iG + 1) s) s
          | Y (s0, s1) ->
            if Bitpath.get pG iG then appose s0 (loop (iG + 1) s1)
                                 else appose (loop (iG + 1) s0) s1
          | P (p0, s0) ->
            let n0 = Bitpath.length p0 in
            let n = Bitpath.coslice_length iG pG 0 p0 in
            if n = n0 then unzoom p0 (loop (iG + n) s0) else
            let s_new =
                if iG + n = nG then f (P (Bitpath.slice n n0 p0, s0)) else
                let s_unm = unzoom (Bitpath.slice (n + 1) n0 p0) s0 in
                let s_mod = unzoom (Bitpath.slice (iG+n+1) nG pG) (f Bot) in
                if Bitpath.get p0 n then (appose s_mod s_unm)
                                    else (appose s_unm s_mod) in
            unzoom (Bitpath.prefix n p0) s_new in
    loop 0 s

let add p = modify p (konst Top)
let remove p = modify p (konst Bot)
let intersect p s = unzoom p (zoom p s)

let fold f =
    let rec loop p = function
      | Bot -> ident
      | Top -> f p
      | Y (s0, s1) -> loop (Bitpath.cat p Bitpath.c1) s1
                   |< loop (Bitpath.cat p Bitpath.c0) s0
      | P (p0, s0) -> loop (Bitpath.cat p p0) s0 in
    loop Bitpath.empty

let iter f s = fold (fun x () -> f x) s ()

let rec cover_card = function
  | Bot -> 0
  | Top -> 1
  | Y (s0, s1) -> cover_card s0 + cover_card s1
  | P (p, s) -> cover_card s

let rec isecn sL sR = match sL, sR with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> sR
  | _, Top -> sL
  | Y (sL0, sL1), Y (sR0, sR1) -> appose (isecn sL0 sR0) (isecn sL1 sR1)
  | P (pL, sL), _ ->
    begin match zoom pL sR with
    | Bot -> Bot
    | sR' -> unzoom pL (isecn sL sR')
    end
  | _, P (pR, sR) ->
    begin match zoom pR sL with
    | Bot -> Bot
    | sL' -> unzoom pR (isecn sL' sR)
    end

let rec union sL sR = match sL, sR with
  | Bot, _ -> sR
  | _, Bot -> sL
  | Top, _ -> Top
  | _, Top -> Top
  | Y (sL0, sL1), Y (sR0, sR1) -> appose (union sL0 sR0) (union sL1 sR1)
  | P (pL, sL), _ ->
    modify pL (fun sR0 -> union sL sR0) sR
  | _, P (pR, sR) ->
    modify pR (fun sL0 -> union sL0 sR) sL

let rec abs_compl = function
  | Bot -> Top
  | Top -> Bot
  | Y (s0, s1) -> appose (abs_compl s0) (abs_compl s1)
  | P (p0, s0) ->
    let n0 = Bitpath.length p0 in
    let rec loop i0 =
        if i0 = n0 then abs_compl s0 else
        if Bitpath.get p0 i0 then appose Top (loop (i0 + 1))
                             else appose (loop (i0 + 1)) Top in
    loop 0

let rec rel_compl sC sR = match sC, sR with
  | _, Bot | Top, _ -> Bot
  | Bot, _ -> sR
  | _, Top -> abs_compl sC
  | Y (sC0, sC1), Y (sR0, sR1) ->
    appose (rel_compl sC0 sR0) (rel_compl sC1 sR1)
  | _, P (pR, sR) -> unzoom pR (rel_compl (zoom pR sC) sR)
  | P (pC0, sC0), _ -> modify pC0 (fun sR0 -> rel_compl sC0 sR0) sR

let rec compl_decomp = function
  | Bot -> Bot, Bot
  | Top -> Bot, Top
  | Y (s0, s1) ->
    let sC0, sR0 = compl_decomp s0 in
    let sC1, sR1 = compl_decomp s1 in
    let sR = appose sR0 sR1 in
    let sC = appose sC0 sC1 in
    let sA = appose (abs_compl s0) (abs_compl s1) in
    if cover_card sR + cover_card sC <= 1 + cover_card sA
        then (sC, sR)
        else (sA, Top)
  | P (p0, s0) ->
    let sC0, sR0 = compl_decomp s0 in
    (unzoom p0 sC0, unzoom p0 sR0)

let rec dump chan = function
  | Bot -> output_string chan "∅"
  | Top -> output_string chan "*"
  | Y (s0, s1) ->
    output_char chan '(';
    dump chan s0;
    output_string chan ", ";
    dump chan s1;
    output_char chan ')'
  | P (p, s) -> output_string chan (Bitpath.to_string p); dump chan s
