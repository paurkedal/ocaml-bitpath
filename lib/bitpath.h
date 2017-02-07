/* Copyright (C) 2012--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

#ifndef BITPATH_H
#define BITPATH_H

#include "bitpath_prereq.h"
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>

BITPATH_BEGIN_DECLS

#define BITPATH_INLINED 1

#ifndef BITPATH_WORD_WIDTH
#  if ULONG_MAX >= UINT64_MAX
#    define BITPATH_WORD_WIDTH 64
#  else
#    define BITPATH_WORD_WIDTH 32
#  endif
#endif
#define BITPATH_WORD_SIZE (BITPATH_WORD_WIDTH / 8)

#if BITPATH_WORD_WIDTH == 64
typedef uint64_t bitpath_word_t;
#  define BITPATH_WORD_C UINT64_C
#else
typedef uint32_t bitpath_word_t;
#  define BITPATH_WORD_C UINT32_C
#endif

typedef struct bitpath *bitpath_t;

struct bitpath
{
    size_t len;
#ifdef BITPATH_INLINED
    bitpath_word_t arr[1];
#else
    bitpath_word_t *arr;
#endif
};

#define BITPATH(v) ((bitpath_t)Data_custom_val(v))

BITPATH_SINLINE size_t bitpath_length(bitpath_t s)
{ return s->len; }

BITPATH_SINLINE bitpath_word_t *bitpath_array(bitpath_t s)
{ return s->arr; }

BITPATH_SINLINE size_t bitpath_array_length(bitpath_t s)
{ return (s->len + BITPATH_WORD_WIDTH - 1) / BITPATH_WORD_WIDTH; }

BITPATH_END_DECLS

#endif
