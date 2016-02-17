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

open Printf
open Bitpath_prereq
open OUnit

let assert_streq = assert_equal ~printer:Bitpath.to_string

let bool_array_get8 i data =
    let n = Array.length data in
    let rec loop j x =
        if j < 0 then x else
        let k = i * 8 + 7 - j in
        let bit = if k >= n then 0 else
                  if Array.get data k then 1 else 0 in
        loop (j - 1) (2 * x + bit) in
    loop 7 0

let bool_array_get16 i data =
    let n = Array.length data in
    let rec loop j x =
        if j < 0 then x else
        let k = i * 16 + 15 - j in
        let bit = if k >= n then 0 else
                  if Array.get data k then 1 else 0 in
        loop (j - 1) (2 * x + bit) in
    loop 15 0

let test_one () =
    let n = Random.int 256 in
    let data = Array.init n (fun i -> Random.bool ()) in
    let bs = Bitpath.init n (fun i -> Array.get data i) in
    let bs8 = Bitpath.init8 n (fun i -> bool_array_get8 i data) in
    let bs16 = Bitpath.init16 n (fun i -> bool_array_get16 i data) in

    assert (Bitpath.length bs8 = n);
    assert (Bitpath.length bs16 = n);

    assert (Bitpath.bitnot bs = Bitpath.map not bs);
    assert (Bitpath.bitnot (Bitpath.bitnot bs) = bs);

    for i = 0 to n - 1 do
        assert (Bitpath.get i bs = Array.get data i);
        assert (Bitpath.get i bs8 = Array.get data i);
        assert (Bitpath.get i bs16 = Array.get data i)
    done;
    for i = 0 to (n + 7) / 8 - 1 do
        assert (Bitpath.get8 i bs = bool_array_get8 i data)
    done;
    for i = 0 to (n + 15) / 16 - 1 do
        assert (Bitpath.get16 i bs = bool_array_get16 i data)
    done;

    let i_r = ref 0 in
    let n' = Bitpath.foldi
        (fun i x ->
            assert (i = !i_r);
            assert (x = Array.get data i);
            i_r := !i_r + 1;
            fun x -> x + 1)
        bs 0 in
    assert (!i_r = n);
    assert (n' = n);

    let i_r = ref 0 in
    let n' = Bitpath.foldi8
        (fun i x ->
            assert (i = !i_r);
            assert (x = bool_array_get8 i data);
            i_r := !i_r + 1;
            fun x -> x + 1)
        bs8 0 in
    assert (!i_r = (n + 7) / 8);
    assert (n' = !i_r);
    let i_r = ref 0 in
    let n' = Bitpath.foldi16
        (fun i x ->
            assert (i = !i_r);
            assert (x = bool_array_get16 i data);
            i_r := !i_r + 1;
            fun x -> x + 1)
        bs16 0 in
    assert (!i_r = (n + 15) / 16);
    assert (n' = !i_r);

    assert (n = Bitpath.coprefix_length bs bs);
    assert (n = Bitpath.coprefix_length bs bs8);
    assert (n = Bitpath.coprefix_length bs bs16);

    let nA = Random.int (n + 1) in
    let nB = n - nA in
    let bsA = Bitpath.init nA (fun i -> Array.get data i) in
    let bsB = Bitpath.init nB (fun i -> Array.get data (nA + i)) in
    let bsAB = Bitpath.cat bsA bsB in
    assert (n = Bitpath.length bsAB);
    assert (n = Bitpath.coprefix_length bs bsAB);
    assert (Bitpath.equal bs bsAB);

    let _ = Bitpath.slice 0 (Bitpath.length bsA) bsA in
    let bsP = Bitpath.coprefix bsA bsB in
    assert (Bitpath.has_prefix bsP bsA);
    assert (Bitpath.has_prefix bsP bsB);
    let nP = Bitpath.length bsP in
    if nP < Bitpath.length bsA then
        assert (not (Bitpath.has_prefix (Bitpath.prefix (nP + 1) bsA) bsB));

    let bsA' = Bitpath.prefix nA bs in
    assert (Bitpath.equal bsA bsA');
    let bsB' = Bitpath.slice nA n bs in
    assert (Bitpath.equal bsB bsB');
    let nC = Random.int (nB + 1) in
    let bsC = Bitpath.slice nA (nA + nC) bs in
    let bsC' = Bitpath.prefix nC bsB in
    assert (Bitpath.equal bsC bsC');

    assert (bs8 = Bitpath.of_string (Bitpath.to_string bs8));
    if Bitpath.length bs mod 4 = 0 then
        assert (bs = Bitpath.of_hex_string (Bitpath.to_hex_string bs));
    if Bitpath.length bs mod 8 = 0 then
        assert (bs = Bitpath.of_bin_string (Bitpath.to_bin_string bs));
    let n' = n + Random.int 65 in
    assert (bs = Bitpath.of_hex_string ~n (Bitpath.to_hex_string ~n:n' bs));
    assert (bs = Bitpath.of_bin_string ~n (Bitpath.to_bin_string ~n:n' bs))

let test () =
    for n = 0 to 500 do
        assert_streq (Bitpath.init n (konst true)) (Bitpath.const n true);
        assert_streq (Bitpath.init n (konst false)) (Bitpath.const n false);
        assert_streq (Bitpath.bitnot (Bitpath.const n true))
                     (Bitpath.const n false);
        assert_streq (Bitpath.bitnot (Bitpath.const n false))
                     (Bitpath.const n true)
    done;
    for i = 0 to 1999 do test_one () done
