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

let ident x = x
let konst x y = x
let ( |< ) f g x = f (g x)

module Option = struct
    type 'a t = 'a option

    let default x0 = function
      | None -> x0
      | Some x -> x
end

module Char = struct
    include Char

    let hexdigit_of_int i =
        if i < 0 then invalid_arg"Char.hexdigit_of_int: Negative argument." else
        if i < 10 then chr (i + 0x30) else
        if i < 16 then chr (i + (0x61 - 10)) else
        invalid_arg "Char.hexdigit_of_int: Argument greater that 16."

    let hexdigit_to_int ch =
        if '0' <= ch && ch <= '9' then code ch - 0x30 else
        if 'a' <= ch && ch <= 'f' then code ch - (0x61 - 10) else
        if 'A' <= ch && ch <= 'F' then code ch - (0x41 - 10) else
        invalid_arg "Char.hexdigit_to_int: Not a hexadecimal digit."
end
