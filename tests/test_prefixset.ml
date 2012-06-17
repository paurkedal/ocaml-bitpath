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

let (===) = Prefixset.equal

let random_bitstring n = Bitpath.init n (fun _ -> Random.bool ())

let random_prefixset max_width =
    if max_width = 0 then
	if Random.bool () then Prefixset.universe else Prefixset.empty else
    let width = Random.int max_width in
    let rec loop n_add s =
	if n_add = 0 then s else
	let p = random_bitstring width in
	loop (n_add - 1) (Prefixset.add p s) in
    let n_add = 1 lsl width in
    loop n_add Prefixset.empty

let verbose = ref false

let show_prefixset sv s =
    if not !verbose then () else
    if true then begin
	printf "%s = {" sv;
	let count = ref 0 in
	Prefixset.prefix_iter (fun x ->
	    if !count > 0 then print_string ", ";
	    count := !count + 1;
	    if Bitpath.length x > 0 then print_string (Bitpath.to_string x);
	    print_char '*') s;
	printf "}\n"
    end else begin
	printf "%s = " sv; Prefixset.dump stdout s; output_char stdout '\n'
    end;
    flush stdout

let test_one max_width =
    let sA = random_prefixset max_width in
    let sB = random_prefixset max_width in
    show_prefixset "A" sA;
    show_prefixset "B" sB;
    assert (Prefixset.valid sA);
    assert (Prefixset.valid sB);

    assert (Prefixset.prefix_card sA =
	    Prefixset.prefix_fold (fun _ -> (+) 1) sA 0);

    let sAuB = Prefixset.union sA sB in
    let sAnB = Prefixset.isecn sA sB in
    show_prefixset "A ∪ B" sAuB;
    show_prefixset "A ∩ B" sAnB;
    assert (Prefixset.valid sAuB);
    assert (Prefixset.valid sAnB);
    assert (sAuB === Prefixset.union sB sA); (* A ∪ B = B ∪ A *)
    assert (sAnB === Prefixset.isecn sB sA); (* A ∩ B = B ∩ A *)

    let scA = Prefixset.abs_compl sA in
    show_prefixset "∁A" scA;

    assert (Prefixset.union sA scA === Prefixset.universe); (* A ∪ ∁A = U *)
    assert (Prefixset.isecn sA scA === Prefixset.empty);    (* A ∩ ∁A = ∅ *)

    let sAcB = Prefixset.rel_compl sB sA in
    let sBcA = Prefixset.rel_compl sA sB in
    show_prefixset "A ∖ B" sAcB;
    show_prefixset "B ∖ A" sBcA;
    assert (Prefixset.valid sAcB);
    assert (Prefixset.valid sBcA);

    let sBcA' = Prefixset.isecn scA sB in
    show_prefixset "∁A ∩ B" sBcA';
    assert (Prefixset.valid sBcA');
    assert (sBcA' === sBcA);
    assert (Prefixset.isecn sAcB sBcA === Prefixset.empty);

    let sA' = Prefixset.rel_compl sBcA sAuB in
    let sB' = Prefixset.rel_compl sAcB sAuB in
    show_prefixset "A ∪ B ∖ (B ∖ A)" sA';
    show_prefixset "A ∪ B ∖ (A ∖ B)" sB';
    assert (Prefixset.valid sA');
    assert (Prefixset.valid sB');
    assert (sB' === sB);
    assert (sA' === sA);
    assert (Prefixset.rel_compl sAnB sAuB === Prefixset.union sAcB sBcA);

    let sN, sP = Prefixset.compl_decomp sA in
    show_prefixset "P" sP;
    show_prefixset "N" sN;
    let sPcN = Prefixset.rel_compl sN sP in
    show_prefixset "P ∖ N" sPcN;
    assert (sPcN === sA);
    assert (Prefixset.prefix_card sP + Prefixset.prefix_card sN <=
	    Prefixset.prefix_card sA)

let test () =
    Arg.parse [("-v", Arg.Set verbose, "Dump computed values.")] (konst ()) "";
    for testnum = 0 to 999 do
	test_one (Random.int 12)
    done
