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

let (===) = Bitpath_cover.equal

let random_bitpath n = Bitpath.init n (fun _ -> Random.bool ())

let random_bitpath_cover max_width =
    if max_width = 0 then
        if Random.bool () then
            Bitpath_cover.universe else Bitpath_cover.empty else
    let width = Random.int max_width in
    let rec loop n_add s =
        if n_add = 0 then s else
        let p = random_bitpath width in
        loop (n_add - 1) (Bitpath_cover.add p s) in
    let n_add = 1 lsl width in
    loop n_add Bitpath_cover.empty

let verbose = ref false

let show_set sv s =
    if not !verbose then () else
    if true then begin
        printf "%s = {" sv;
        let count = ref 0 in
        Bitpath_cover.iter (fun x ->
            if !count > 0 then print_string ", ";
            count := !count + 1;
            if Bitpath.length x > 0 then print_string (Bitpath.to_string x);
            print_char '*') s;
        printf "}\n"
    end else begin
        printf "%s = " sv; Bitpath_cover.dump stdout s; output_char stdout '\n'
    end;
    flush stdout

let test_one max_width =
    let sA = random_bitpath_cover max_width in
    let sB = random_bitpath_cover max_width in
    show_set "A" sA;
    show_set "B" sB;
    assert (Bitpath_cover.valid sA);
    assert (Bitpath_cover.valid sB);

    assert (Bitpath_cover.cover_card sA =
            Bitpath_cover.fold (fun _ -> (+) 1) sA 0);

    Bitpath_cover.iter
        (fun p ->
            let p' = random_bitpath 4 in
            assert (Bitpath_cover.cover_find (Bitpath.cat p p') sA = p))
        sA;

    let sAuB = Bitpath_cover.union sA sB in
    let sAnB = Bitpath_cover.isecn sA sB in
    show_set "A ∪ B" sAuB;
    show_set "A ∩ B" sAnB;
    assert (Bitpath_cover.valid sAuB);
    assert (Bitpath_cover.valid sAnB);
    assert (sAuB === Bitpath_cover.union sB sA); (* A ∪ B = B ∪ A *)
    assert (sAnB === Bitpath_cover.isecn sB sA); (* A ∩ B = B ∩ A *)

    let scA = Bitpath_cover.abs_compl sA in
    show_set "∁A" scA;

    (* A ∪ ∁A = U and A ∩ ∁A = ∅ *)
    assert (Bitpath_cover.union sA scA === Bitpath_cover.universe);
    assert (Bitpath_cover.isecn sA scA === Bitpath_cover.empty);

    let sAcB = Bitpath_cover.rel_compl sB sA in
    let sBcA = Bitpath_cover.rel_compl sA sB in
    show_set "A ∖ B" sAcB;
    show_set "B ∖ A" sBcA;
    assert (Bitpath_cover.valid sAcB);
    assert (Bitpath_cover.valid sBcA);

    let sBcA' = Bitpath_cover.isecn scA sB in
    show_set "∁A ∩ B" sBcA';
    assert (Bitpath_cover.valid sBcA');
    assert (sBcA' === sBcA);
    assert (Bitpath_cover.isecn sAcB sBcA === Bitpath_cover.empty);

    let sA' = Bitpath_cover.rel_compl sBcA sAuB in
    let sB' = Bitpath_cover.rel_compl sAcB sAuB in
    show_set "A ∪ B ∖ (B ∖ A)" sA';
    show_set "A ∪ B ∖ (A ∖ B)" sB';
    assert (Bitpath_cover.valid sA');
    assert (Bitpath_cover.valid sB');
    assert (sB' === sB);
    assert (sA' === sA);
    assert (Bitpath_cover.rel_compl sAnB sAuB ===
            Bitpath_cover.union sAcB sBcA);

    let sN, sP = Bitpath_cover.compl_decomp sA in
    show_set "P" sP;
    show_set "N" sN;
    let sPcN = Bitpath_cover.rel_compl sN sP in
    show_set "P ∖ N" sPcN;
    assert (sPcN === sA);
    assert (Bitpath_cover.cover_card sP + Bitpath_cover.cover_card sN <=
            Bitpath_cover.cover_card sA)

let test () =
    Arg.parse [("-v", Arg.Set verbose, "Dump computed values.")] (konst ()) "";
    for testnum = 0 to 999 do
        test_one (Random.int 12)
    done
