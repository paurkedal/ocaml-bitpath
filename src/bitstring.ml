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

open Bitlib_prereq

type t

external make_empty : unit -> t = "camlbitlib_bitstring_make_empty"
let empty = make_empty ()

external init : int -> (int -> bool) -> t = "camlbitlib_bitstring_init"
external init8 : int -> (int -> int) -> t = "camlbitlib_bitstring_init8"
external init16 : int -> (int -> int) -> t = "camlbitlib_bitstring_init16"

external const : int -> bool -> t = "camlbitlib_bitstring_const"

external length : t -> int = "camlbitlib_bitstring_length"
let length8 s = let n = length s in (n + 7) / 8
let length16 s = let n = length s in (n + 15) / 16

external get : int -> t -> bool = "camlbitlib_bitstring_get"
external get8 : int -> t -> int = "camlbitlib_bitstring_get8"
external get16 : int -> t -> int = "camlbitlib_bitstring_get16"

external compare : t -> t -> int = "camlbitlib_bitstring_compare"
let equal sA sB = compare sA sB = 0

external slice : int -> int -> t -> t = "camlbitlib_bitstring_slice"
let prefix = slice 0
let suffix i s = slice i (length s) s

external coprefix_length : t -> t -> int
    = "camlbitlib_bitstring_coprefix_length"
external coslice_length : int -> t -> int -> t -> int
    = "camlbitlib_bitstring_coslice_length"

external cat : t -> t -> t = "camlbitlib_bitstring_cat"

let coprefix sA sB = slice 0 (coprefix_length sA sB) sA

let has_prefix sP s = coprefix_length sP s = length sP

let has_suffix sS s =
    let nS, n = length sS, length s in
    n >= nS && coslice_length 0 sS (length s - nS) s = nS

let has_slice sS i s = coslice_length 0 sS i s = length sS

external bitnot : t -> t = "camlbitlib_bitstring_not"

let map f s =
    match f false, f true with
    | false, true -> s
    | false, false -> const (length s) false
    | true, true -> const (length s) true
    | true, false -> bitnot s

let mapi f s = init (length s) (fun i -> f i (get i s))

let fold f s accu =
    let n = length s in
    let rec loop i accu =
	if i = n then accu else
	loop (i + 1) (f (get i s) accu) in
    loop 0 accu

let foldi f s accu =
    let n = length s in
    let rec loop i accu =
	if i = n then accu else
	loop (i + 1) (f i (get i s) accu) in
    loop 0 accu

let foldi8 f s accu =
    let n = length8 s in
    let rec loop i accu =
	if i = n then accu else
	loop (i + 1) (f i (get8 i s) accu) in
    loop 0 accu

let foldi16 f s accu =
    let n = length16 s in
    let rec loop i accu =
	if i = n then accu else
	loop (i + 1) (f i (get16 i s) accu) in
    loop 0 accu

let iter f s = for i = 0 to length s - 1 do f (get i s) done

let iteri f s = for i = 0 to length s - 1 do f i (get i s) done

let of_string str =
    let get_bit i =
	match str.[i] with
	  | '0' -> false
	  | '1' -> true
	  | _ -> invalid_arg "Bitstring.of_string" in
    init (String.length str) get_bit

let to_string s =
    if length s = 0 then "ε" else
    String.init (length s) (fun i -> if get i s then '1' else '0')
