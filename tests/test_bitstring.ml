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

open Printf
open Bitlib_prereq
open OUnit

let assert_streq = assert_equal ~printer:Bitstring.to_string

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
    let bs = Bitstring.init n (fun i -> Array.get data i) in
    let bs8 = Bitstring.init8 n (fun i -> bool_array_get8 i data) in
    let bs16 = Bitstring.init16 n (fun i -> bool_array_get16 i data) in

    assert (Bitstring.length bs8 = n);
    assert (Bitstring.length bs16 = n);

    assert (Bitstring.bitnot bs = Bitstring.map not bs);
    assert (Bitstring.bitnot (Bitstring.bitnot bs) = bs);

    for i = 0 to n - 1 do
	assert (Bitstring.get i bs = Array.get data i);
	assert (Bitstring.get i bs8 = Array.get data i);
	assert (Bitstring.get i bs16 = Array.get data i)
    done;
    for i = 0 to (n + 7) / 8 - 1 do
	assert (Bitstring.get8 i bs = bool_array_get8 i data)
    done;
    for i = 0 to (n + 15) / 16 - 1 do
	assert (Bitstring.get16 i bs = bool_array_get16 i data)
    done;

    let i_r = ref 0 in
    let n' = Bitstring.foldi
	(fun i x ->
	    assert (i = !i_r);
	    assert (x = Array.get data i);
	    i_r := !i_r + 1;
	    fun x -> x + 1)
	bs 0 in
    assert (!i_r = n);
    assert (n' = n);

    let i_r = ref 0 in
    let n' = Bitstring.foldi8
	(fun i x ->
	    assert (i = !i_r);
	    assert (x = bool_array_get8 i data);
	    i_r := !i_r + 1;
	    fun x -> x + 1)
	bs8 0 in
    assert (!i_r = (n + 7) / 8);
    assert (n' = !i_r);
    let i_r = ref 0 in
    let n' = Bitstring.foldi16
	(fun i x ->
	    assert (i = !i_r);
	    assert (x = bool_array_get16 i data);
	    i_r := !i_r + 1;
	    fun x -> x + 1)
	bs16 0 in
    assert (!i_r = (n + 15) / 16);
    assert (n' = !i_r);

    assert (n = Bitstring.coprefix_length bs bs);
    assert (n = Bitstring.coprefix_length bs bs8);
    assert (n = Bitstring.coprefix_length bs bs16);

    let nA = Random.int (n + 1) in
    let nB = n - nA in
    let bsA = Bitstring.init nA (fun i -> Array.get data i) in
    let bsB = Bitstring.init nB (fun i -> Array.get data (nA + i)) in
    let bsAB = Bitstring.cat bsA bsB in
    assert (n = Bitstring.length bsAB);
    assert (n = Bitstring.coprefix_length bs bsAB);
    assert (Bitstring.equal bs bsAB);

    let _ = Bitstring.slice 0 (Bitstring.length bsA) bsA in
    let bsP = Bitstring.coprefix bsA bsB in
    assert (Bitstring.has_prefix bsP bsA);
    assert (Bitstring.has_prefix bsP bsB);
    let nP = Bitstring.length bsP in
    if nP < Bitstring.length bsA then
	assert (not (Bitstring.has_prefix (Bitstring.prefix (nP + 1) bsA) bsB));

    let bsA' = Bitstring.prefix nA bs in
    assert (Bitstring.equal bsA bsA');
    let bsB' = Bitstring.slice nA n bs in
    assert (Bitstring.equal bsB bsB');
    let nC = Random.int (nB + 1) in
    let bsC = Bitstring.slice nA (nA + nC) bs in
    let bsC' = Bitstring.prefix nC bsB in
    assert (Bitstring.equal bsC bsC');

    assert (bs8 = Bitstring.of_string (Bitstring.to_string bs8));
    if Bitstring.length bs mod 4 = 0 then
	assert (bs = Bitstring.of_hex_string (Bitstring.to_hex_string bs));
    if Bitstring.length bs mod 8 = 0 then
	assert (bs = Bitstring.of_bin_string (Bitstring.to_bin_string bs));
    let n' = n + Random.int 65 in
    assert (bs = Bitstring.of_hex_string ~n (Bitstring.to_hex_string ~n:n' bs));
    assert (bs = Bitstring.of_bin_string ~n (Bitstring.to_bin_string ~n:n' bs))

let test () =
    for n = 0 to 500 do
	assert_streq (Bitstring.init n (konst true)) (Bitstring.const n true);
	assert_streq (Bitstring.init n (konst false)) (Bitstring.const n false);
	assert_streq (Bitstring.bitnot (Bitstring.const n true))
		     (Bitstring.const n false);
	assert_streq (Bitstring.bitnot (Bitstring.const n false))
		     (Bitstring.const n true)
    done;
    for i = 0 to 1999 do test_one () done
