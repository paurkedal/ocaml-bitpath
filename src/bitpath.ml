(* Copyright (C) 2012--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

type t

external make_empty : unit -> t = "camlbitpath_make_empty"
let empty = make_empty ()

external init : int -> (int -> bool) -> t = "camlbitpath_init"
external init8 : int -> (int -> int) -> t = "camlbitpath_init8"
external init16 : int -> (int -> int) -> t = "camlbitpath_init16"

external const : int -> bool -> t = "camlbitpath_const"

let c0 = const 1 false
let c1 = const 1 true

external length : t -> int = "camlbitpath_length"
let length8 s = let n = length s in (n + 7) / 8
let length16 s = let n = length s in (n + 15) / 16

external get : int -> t -> bool = "camlbitpath_get"
external get8 : int -> t -> int = "camlbitpath_get8"
external get16 : int -> t -> int = "camlbitpath_get16"

let is_empty p = length p = 0

external compare : t -> t -> int = "camlbitpath_compare"
let equal sA sB = compare sA sB = 0

external slice : int -> int -> t -> t = "camlbitpath_slice"
let prefix = slice 0
let suffix i s = slice i (length s) s

external coprefix_length : t -> t -> int
    = "camlbitpath_coprefix_length"
external coslice_length : int -> t -> int -> t -> int
    = "camlbitpath_coslice_length"

external cat : t -> t -> t = "camlbitpath_cat"

let coprefix sA sB = slice 0 (coprefix_length sA sB) sA

let has_prefix sP s = coprefix_length sP s = length sP

let has_suffix sS s =
    let nS, n = length sS, length s in
    n >= nS && coslice_length 0 sS (length s - nS) s = nS

let has_slice sS i s = coslice_length 0 sS i s = length sS

external bitnot : t -> t = "camlbitpath_not"

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
let iteri8 f s = for i = 0 to length8 s - 1 do f i (get8 i s) done
let iteri16 f s = for i = 0 to length16 s - 1 do f i (get16 i s) done

let of_array8 xa = init8 (Array.length xa * 8) (fun i -> xa.(i))
let of_array16 xa = init16 (Array.length xa * 16) (fun i -> xa.(i))

let of_string str =
    let get_bit i =
        match str.[i] with
          | '0' -> false
          | '1' -> true
          | _ -> invalid_arg "Bitpath.of_string" in
    if str = "ε" then empty else
    init (String.length str) get_bit

let to_string s =
    if length s = 0 then "ε" else
    String.init (length s) (fun i -> if get i s then '1' else '0')

let of_bin_string ?n str =
    let m = String.length str in
    let n = Option.default (m * 8) n in
    init8 n (fun i -> if i < m then Char.code (String.get str i) else 0)

let to_bin_string ?n s =
    let ns = length s in
    let put_octet i = Char.chr (if 8 * i < ns then get8 i s else 0) in
    String.init ((Option.default ns n + 7) / 8) put_octet

let of_hex_string ?n str =
    let m = String.length str in
    let n = Option.default (m * 4) n in
    let get_octet i =
        if 2*i + 1 < m then
            16 * Char.hexdigit_to_int str.[2*i]
               + Char.hexdigit_to_int str.[2*i + 1] else
        if 2*i < m then
            16 * Char.hexdigit_to_int str.[2*i] else
        0 in
    init8 n get_octet

let to_hex_string ?n s =
    let ns = length s in
    let put_quartet i =
        let x = if (4 * i) >= ns then 0 else
                get8 (i / 2) s lsr (4*(1 - i mod 2)) land 15 in
        Char.hexdigit_of_int x in
    String.init ((Option.default ns n + 3) / 4) put_quartet
