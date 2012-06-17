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
open Bitpath_prereq

module Ipm = Bitpath_cover_map.Make
    (struct  type t = int  let equal : t -> t -> bool = (=)  end)

let (===) = Ipm.equal
let (---) mA mB = Ipm.map (konst 0) mA === Ipm.map (konst 0) mB

let random_bitstring n = Bitpath.init n (fun _ -> Random.bool ())

let random_bitpath_cover_map max_width =
    if max_width = 0 then
	if Random.bool () then Ipm.const 0 else Ipm.empty else
    let width = Random.int max_width in
    let n_add = 1 lsl width in
    let n_val = 3 * n_add / 2 + 1 in
    let rec loop n_add s =
	if n_add = 0 then s else
	let p = random_bitstring width in
	loop (n_add - 1) (Ipm.add p (Random.int n_val) s) in
    loop n_add Ipm.empty

let verbose = ref false

let show_map sv s =
    if not !verbose then () else
    begin
	printf "%s = {" sv;
	let count = ref 0 in
	Ipm.iteri (fun p x ->
	    if !count > 0 then print_string ", ";
	    count := !count + 1;
	    if Bitpath.length p > 0 then print_string (Bitpath.to_string p);
	    print_string "* ↦ ";
	    print_int x) s;
	printf "}\n"
    end;
    flush stdout

let test_one max_width =
    let sA = random_bitpath_cover_map max_width in
    let sB = random_bitpath_cover_map max_width in
    show_map "A" sA;
    show_map "B" sB;
    assert (Ipm.valid sA);
    assert (Ipm.valid sB);

    assert (Ipm.card sA = Ipm.fold (fun _ -> (+) 1) sA 0);

    assert ((sA === sB) == (sB === sA));

    assert (Ipm.disjoint sA Ipm.empty);
    assert (Ipm.disjoint sA sB == Ipm.disjoint sB sA);
    assert (Ipm.disjoint sA sB == Ipm.is_empty (Ipm.right_isecn sA sB));

    let p = random_bitstring 65 in
    assert (Ipm.appose (Ipm.lower_half sA) (Ipm.upper_half sA) === sA);
    assert (Ipm.lower_half (Ipm.appose sA sB) === sA);
    assert (Ipm.upper_half (Ipm.appose sA sB) === sB);
    assert (Ipm.zoom p (Ipm.unzoom p sA) === sA);
    assert (Ipm.unzoom p (Ipm.zoom p sA) === Ipm.intersect p sA);

    assert (Ipm.intersect p sA ===
	    Ipm.right_isecn (Ipm.unzoom p (Ipm.const 0)) sA);
    assert (Ipm.disjoint (Ipm.remove p sA) (Ipm.intersect p sA));

    let sAuB = Ipm.right_union sA sB in
    let sAnB = Ipm.right_isecn sA sB in
    show_map "A ∪ B" sAuB;
    show_map "A ∩ B" sAnB;
    assert (Ipm.valid sAuB);
    assert (Ipm.valid sAnB);
    assert (sAuB --- Ipm.right_union sB sA); (* A ∪ B = B ∪ A *)
    assert (sAnB --- Ipm.right_isecn sB sA); (* A ∩ B = B ∩ A *)
    ()

let test () =
    Arg.parse [("-v", Arg.Set verbose, "Dump computed values.")] (konst ()) "";

    let const0 = Ipm.const 0 in
    let const1 = Ipm.const 1 in
    let const2 = Ipm.const 2 in
    assert (Ipm.empty === Ipm.empty);
    assert (Ipm.disjoint Ipm.empty Ipm.empty);
    assert (Ipm.disjoint Ipm.empty const0);
    assert (not (Ipm.disjoint const0 const1));
    assert (not (Ipm.disjoint (Ipm.appose Ipm.empty const2) const2));
    assert (not (Ipm.disjoint (Ipm.appose const2 Ipm.empty) const2));
    assert (Ipm.is_empty Ipm.empty);
    assert (not (Ipm.is_empty const0));
    assert (Ipm.empty === Ipm.empty);
    assert (not (Ipm.empty === const0));
    assert (const1 === const1);
    assert (not (const0 === const1));
    assert (not (Ipm.is_const Ipm.empty));
    assert (Ipm.is_const const1);
    assert (Ipm.to_const const1 == 1);
    assert (Ipm.appose const2 const2 === const2);
    assert (Ipm.appose Ipm.empty const1 ===
	    Ipm.unzoom (Bitpath.const 1 true) const1);
    assert (Ipm.card Ipm.empty == 0);
    assert (Ipm.card const2 == 1);

    for testnum = 0 to 999 do
	test_one (Random.int 12)
    done
