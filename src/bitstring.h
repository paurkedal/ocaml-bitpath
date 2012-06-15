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

#ifndef BITLIB_BITSTRING_H
#define BITLIB_BITSTRING_H

#include "prereq.h"
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>

BITLIB_BEGIN_DECLS

#define BITLIB_BITSTRING_INLINED 1

#ifndef BITLIB_WORD_WIDTH
#  if ULONG_MAX >= UINT64_MAX
#    define BITLIB_WORD_WIDTH 64
#  else
#    define BITLIB_WORD_WIDTH 32
#  endif
#endif
#define BITLIB_WORD_SIZE (BITLIB_WORD_WIDTH / 8)

#if BITLIB_WORD_WIDTH == 64
typedef uint64_t bitlib_word_t;
#  define BITLIB_WORD_C UINT64_C
#else
typedef uint32_t bitlib_word_t;
#  define BITLIB_WORD_C UINT32_C
#endif

typedef struct bitlib_bitstring *bitlib_bitstring_t;

struct bitlib_bitstring
{
    size_t len;
#ifdef BITLIB_BITSTRING_INLINED
    bitlib_word_t arr[1];
#else
    bitlib_word_t *arr;
#endif
};

#define BITLIB_BITSTRING(v) ((bitlib_bitstring_t)Data_custom_val(v))

BITLIB_SINLINE size_t bitlib_bitstring_length(bitlib_bitstring_t s)
{ return s->len; }

BITLIB_SINLINE bitlib_word_t *bitlib_bitstring_array(bitlib_bitstring_t s)
{ return s->arr; }

BITLIB_SINLINE size_t bitlib_bitstring_array_length(bitlib_bitstring_t s)
{ return (s->len + BITLIB_WORD_WIDTH - 1) / BITLIB_WORD_WIDTH; }

BITLIB_END_DECLS

#endif
