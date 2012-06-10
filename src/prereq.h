/* Copyright (C) 2012  Petter Urkedal <paurkedal@gmail.com>
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
 */

#ifndef BITLIB_PREREQ_H
#define BITLIB_PREREQ_H

#ifdef __cplusplus
#  define BITLIB_BEGIN_DECLS extern "C" {
#  define BITLIB_END_DECLS }
#else
#  define BITLIB_BEGIN_DECLS
#  define BITLIB_END_DECLS
#endif

#endif
