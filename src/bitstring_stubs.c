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

#include "bitstring.h"

#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <string.h>
#include <assert.h>

#ifndef MIN
#  define MIN(x, y) ((x) <= (y)? (x) : (y))
#endif
#define WORD_ROT(i, k) ((i << k) | (i >> (8*sizeof(bitlib_word_t) - i)))
#define WORD_WIDTH BITLIB_WORD_WIDTH
#define WORD_SIZE (BITLIB_WORD_WIDTH / 8)
#define WORD_SZ16 (BITLIB_WORD_WIDTH / 16)
#define WORD_CNT(n) (((n) + WORD_WIDTH - 1) / WORD_WIDTH)
#define WORD_MOD(i) (WORD_WIDTH - 1 - (i) % WORD_WIDTH)
#define WORD_MOD8(i) ((WORD_SIZE - 1 - (i) % WORD_SIZE) * 8)
#define WORD_MOD16(i) ((WORD_SZ16 - 1 - (i) % WORD_SZ16) * 16)

#define BITSTRING(v) ((bitlib_bitstring_t)Data_custom_val(v))
#define BITSTRING_GET(i, s)  ((s->arr[(i) / WORD_WIDTH] >> WORD_MOD(i)) & 1)
#define BITSTRING_GET8(i, s) ((s->arr[(i) / WORD_SIZE] >> WORD_MOD8(i)) & 0xff)
#define BITSTRING_GET16(i, s) \
    ((s->arr[(i) / WORD_SZ16] >> WORD_MOD16(i)) & 0xffff)

#ifndef BITLIB_BITSTRING_INLINED
static void
_bitstring_finalize(value s_v)
{
    free(BITSTRING(s_v)->arr);
}
#endif

static int
_bitstring_compare(value sA_v, value sB_v)
{
    size_t i;
    size_t mA = WORD_CNT(BITSTRING(sA_v)->len);
    size_t mB = WORD_CNT(BITSTRING(sB_v)->len);
    for (i = 0; i < MIN(mA, mB); ++i) {
	bitlib_word_t wA = BITSTRING(sA_v)->arr[i];
	bitlib_word_t wB = BITSTRING(sB_v)->arr[i];
	if (wA < wB) return -1;
	if (wA > wB) return 1;
    }
    if (mA < mB) return -1;
    if (mA > mB) return 1;
    return 0;
}

value
bitlib_bitstring_compare(value sA_v, value sB_v)
{
    return Val_int(_bitstring_compare(sA_v, sB_v));
}

/* Based on Bob Jenkins' hash functions. */
#if BITLIB_WORD_WIDTH >= 64
#  define MIX(a, b, c) \
    do { \
        a -= b + c;  a ^= (c >> 43); \
        b -= c + a;  b ^= (a <<  9); \
        c -= a + b;  c ^= (b >>  8); \
        a -= b + c;  a ^= (c >> 38); \
        b -= c + a;  b ^= (a << 23); \
        c -= a + b;  c ^= (b >>  5); \
        a -= b + c;  a ^= (c >> 35); \
        b -= c + a;  b ^= (a << 49); \
        c -= a + b;  c ^= (b >> 11); \
        a -= b + c;  a ^= (c >> 12); \
        b -= c + a;  b ^= (a << 18); \
        c -= a + b;  c ^= (b >> 22); \
    } while (0)
#else
#  define MIX(a, b, c) \
    do { \
        a -= c;  a ^= WORD_ROT(c,  4);  c += b; \
        b -= a;  b ^= WORD_ROT(a,  6);  a += c; \
        c -= b;  c ^= WORD_ROT(b,  8);  b += a; \
        a -= c;  a ^= WORD_ROT(c, 16);  c += b; \
        b -= a;  b ^= WORD_ROT(a, 19);  a += c; \
        c -= b;  c ^= WORD_ROT(b,  4);  b += a; \
    } while (0)
#endif

static long
_bitstring_hash(value s_v)
{
    size_t i, m = WORD_CNT(BITSTRING(s_v)->len);
    long a, b, c;
    a = 0xba5ebee7;
    b = 0xcafebabe;
    c = BITSTRING(s_v)->len;
    for (i = 0; i < m - 2; i += 3) {
	a += BITSTRING(s_v)->arr[i];
	b += BITSTRING(s_v)->arr[i + 1];
	c += BITSTRING(s_v)->arr[i + 2];
	MIX(a, b, c);
    }
    if (i < m) {
	a += BITSTRING(s_v)->arr[i];
	if (i + 1 < m) b += BITSTRING(s_v)->arr[i + 1];
	MIX(a, b, c);
    }
    return c;
}

static struct custom_operations _bitstring_ops = {
    "org.eideticdew.bitlib.bitstring",
#ifndef BITLIB_BITSTRING_INLINED
    _bitstring_finalize,
#else
    custom_finalize_default,
#endif
    _bitstring_compare,
    _bitstring_hash,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
};

static value
_bitstring_alloc(size_t m)
{
#ifdef BITLIB_BITSTRING_INLINED
    return caml_alloc_custom(&_bitstring_ops,
	    sizeof(struct bitlib_bitstring) + (m - 1)*sizeof(bitlib_word_t),
	    0, 1);
#else
    value s = caml_alloc_custom(&_bitstring_ops,
				sizeof(struct bitlib_bitstring), 0, 1);
    BITSTRING(s)->arr = malloc(m * WORD_SIZE);
    return s;
#endif
}

static value _bitstring_empty = Val_unit;

value
bitlib_bitstring_make_empty(value _)
{
    assert(_bitstring_empty == Val_unit);
    caml_register_global_root(&_bitstring_empty);
    _bitstring_empty = _bitstring_alloc(0);
    BITSTRING(_bitstring_empty)->len = 0;
    return _bitstring_empty;
}

value
bitlib_bitstring_init(value n_v, value f_v)
{
    CAMLparam2 (n_v, f_v);
    CAMLlocal1 (s_v);
    size_t i, j, n = Long_val(n_v);
    size_t m0 = n / WORD_WIDTH;
    size_t m = WORD_CNT(n);

    s_v = _bitstring_alloc(m);
    BITSTRING(s_v)->len = n;
    if (!BITSTRING(s_v)->arr)
	caml_raise_out_of_memory();
    for (i = 0; i < m0; ++i) {
	bitlib_word_t w = 0;
	for (j = 0; j < WORD_WIDTH; ++j) {
	    int x = Bool_val(caml_callback(f_v, Val_long(i * WORD_WIDTH + j)));
	    w <<= 1;
	    w |= x;
	}
	BITSTRING(s_v)->arr[i] = w;
    }
    if (m0 < m) {
	bitlib_word_t w = 0;
	for (j = 0; j < n % WORD_WIDTH; ++j) {
	    int x = Bool_val(caml_callback(f_v, Val_long(m0 * WORD_WIDTH + j)));
	    w <<= 1;
	    w |= x;
	}
	BITSTRING(s_v)->arr[m0] = w << (WORD_WIDTH - n % WORD_WIDTH);
    }
    CAMLreturn (s_v);
}

value
bitlib_bitstring_init8(value n_v, value f_v)
{
    CAMLparam2 (n_v, f_v);
    CAMLlocal1 (s_v);
    size_t i, j, n = Long_val(n_v);
    size_t m0 = n / WORD_SIZE;
    size_t m = WORD_CNT(n * 8);

    s_v = _bitstring_alloc(m);
    BITSTRING(s_v)->len = n * 8;
    if (!BITSTRING(s_v)->arr)
	caml_raise_out_of_memory();
    for (i = 0; i < m0; ++i) {
	bitlib_word_t w = 0;
	for (j = 0; j < WORD_SIZE; ++j) {
	    int x = Int_val(caml_callback(f_v, Val_long(i * WORD_SIZE + j)));
	    w <<= 8;
	    w |= x;
	}
	BITSTRING(s_v)->arr[i] = w;
    }
    if (m0 < m) {
	bitlib_word_t w = 0;
	for (j = 0; j < n % WORD_SIZE; ++j) {
	    int x = Int_val(caml_callback(f_v, Val_long(m0 * WORD_SIZE + j)));
	    w <<= 8;
	    w |= x;
	}
	BITSTRING(s_v)->arr[m0] = w << (WORD_SIZE - n % WORD_SIZE)*8;
    }
    CAMLreturn (s_v);
}

value
bitlib_bitstring_init16(value n_v, value f_v)
{
    CAMLparam2 (n_v, f_v);
    CAMLlocal1 (s_v);
    size_t i, j, n = Long_val(n_v);
    size_t m0 = n / WORD_SZ16;
    size_t m = WORD_CNT(n * 16);

    s_v = _bitstring_alloc(m);
    BITSTRING(s_v)->len = n * 16;
    if (!BITSTRING(s_v)->arr)
	caml_raise_out_of_memory();
    for (i = 0; i < m0; ++i) {
	bitlib_word_t w = 0;
	for (j = 0; j < WORD_SZ16; ++j) {
	    int x = Int_val(caml_callback(f_v, Val_long(i*WORD_SZ16 + j)));
	    w <<= 16;
	    w |= x;
	}
	BITSTRING(s_v)->arr[i] = w;
    }
    if (m0 < m) {
	bitlib_word_t w = 0;
	for (j = 0; j < n % WORD_SZ16; ++j) {
	    int x = Int_val(caml_callback(f_v, Val_long(i*WORD_SZ16 + j)));
	    w <<= 16;
	    w |= x;
	}
	BITSTRING(s_v)->arr[m0] = w << (WORD_SZ16 - n % WORD_SZ16)*16;
    }
    CAMLreturn (s_v);
}

#include <stdio.h>
value
bitlib_bitstring_const(value n_v, value x_v)
{
    CAMLparam2 (n_v, x_v);
    CAMLlocal1 (s_v);
    size_t i, n, m;
    bitlib_word_t x;

    n = Long_val(n_v);
    if (n == 0) {
	assert(_bitstring_empty != Val_unit);
	CAMLreturn (_bitstring_empty);
    }
    m = WORD_CNT(n);
    x = Bool_val(x_v)? ~BITLIB_WORD_C(0) : 0;
    s_v = _bitstring_alloc(m);
    BITSTRING(s_v)->len = n;
    if (!BITSTRING(s_v)->arr)
	caml_raise_out_of_memory();
    for (i = 0; i < m - 1; ++i)
	BITSTRING(s_v)->arr[i] = x;
    BITSTRING(s_v)->arr[m - 1] = x << (WORD_WIDTH - 1 - (n - 1) % WORD_WIDTH);
    CAMLreturn (s_v);
}

value
bitlib_bitstring_not(value sA_v)
{
    CAMLparam1 (sA_v);
    CAMLlocal1 (sN_v);
    size_t i, n, m;
    n = BITSTRING(sA_v)->len;
    if (n == 0) {
	assert(_bitstring_empty != Val_unit);
	CAMLreturn (_bitstring_empty);
    }
    m = WORD_CNT(n);
    sN_v = _bitstring_alloc(m);
    BITSTRING(sN_v)->len = n;
    for (i = 0; i < m - 1; ++i)
	BITSTRING(sN_v)->arr[i] = ~BITSTRING(sA_v)->arr[i];
    BITSTRING(sN_v)->arr[m - 1] = ~BITSTRING(sA_v)->arr[m - 1]
	    & (~BITLIB_WORD_C(0) << (WORD_WIDTH - 1 - (n - 1) % WORD_WIDTH));
    CAMLreturn (sN_v);
}

value
bitlib_bitstring_length(value s_v)
{
    return Val_long(BITSTRING(s_v)->len);
}

value
bitlib_bitstring_get(value i_v, value s_v)
{
    size_t i = Long_val(i_v);
    return Val_bool(BITSTRING_GET(i, BITSTRING(s_v)));
}

value
bitlib_bitstring_get8(value i_v, value s_v)
{
    size_t i = Long_val(i_v);
    return Val_int(BITSTRING_GET8(i, BITSTRING(s_v)));
}

value
bitlib_bitstring_get16(value i_v, value s_v)
{
    size_t i = Long_val(i_v);
    return Val_int(BITSTRING_GET16(i, BITSTRING(s_v)));
}

value
bitlib_bitstring_coprefix_length(value sA_v, value sB_v)
{
    size_t i, n = MIN(BITSTRING(sA_v)->len, BITSTRING(sB_v)->len);
    size_t j, m = n / WORD_WIDTH;
    for (j = 0; j < m; ++j)
	if (BITSTRING(sA_v)->arr[j] != BITSTRING(sB_v)->arr[j])
	    break;
    for (i = j * WORD_WIDTH; i < n; ++i)
	if (BITSTRING_GET(i, BITSTRING(sA_v)) !=
	    BITSTRING_GET(i, BITSTRING(sB_v)))
	    break;
    return Val_long(i);
}

value
bitlib_bitstring_coslice_length(value iA_v, value sA_v, value iB_v, value sB_v)
{
    size_t iA = Long_val(iA_v);
    size_t iB = Long_val(iB_v);
    size_t i, n = MIN(BITSTRING(sA_v)->len - iA, BITSTRING(sB_v)->len - iB);
    /* TODO: This can be optimized to operate on words, but it'll take some
     * coding for unaligned cases. */
    for (i = 0; i < n; ++i)
	if (BITSTRING_GET(iA + i, BITSTRING(sA_v)) !=
	    BITSTRING_GET(iB + i, BITSTRING(sB_v)))
	    break;
    return Val_long(i);
}

value
bitlib_bitstring_slice(value iL_v, value iU_v, value sA_v)
{
    CAMLparam3 (sA_v, iL_v, iU_v);
    CAMLlocal1 (s_v);
    size_t iL = Long_val(iL_v);
    size_t iU = Long_val(iU_v);
    size_t nA, n, m;

    if (iL < 0)
	caml_invalid_argument("Bitstring.slice: negative lower bound");
    if (iL > iU)
	caml_invalid_argument("Bitstring.slice: lower bound larger "
			      "than upper bound.");

    if (iL == iU) {
	assert(_bitstring_empty != Val_unit);
	CAMLreturn (_bitstring_empty);
    }
    nA = BITSTRING(sA_v)->len;
    if (iU > nA)
	caml_invalid_argument("Bitstring.slice: Upper bound out of range.");
    if (iL == 0 && iU == nA) CAMLreturn (sA_v);

    n = iU - iL;
    m = WORD_CNT(n);
    assert(m > 0);

    s_v = _bitstring_alloc(m);
    BITSTRING(s_v)->len = n;
    if (!BITSTRING(s_v)->arr)
	caml_raise_out_of_memory();

    if (iL % WORD_WIDTH == 0)
	memcpy(BITSTRING(s_v)->arr, BITSTRING(sA_v)->arr + iL / WORD_WIDTH,
	       m * WORD_SIZE);
    else {
	bitlib_word_t w0, w1;
	int jL = iL % WORD_WIDTH;
	size_t k, kL = iL / WORD_WIDTH;
	w0 = BITSTRING(sA_v)->arr[kL];
	for (k = 0; k < m; ++k) {
	    w1 = BITSTRING(sA_v)->arr[kL + k + 1];
	    BITSTRING(s_v)->arr[k] = (w0 << jL) | (w1 >> (WORD_WIDTH - jL));
	    w0 = w1;
	}
    }
    BITSTRING(s_v)->arr[m - 1] &= ~BITLIB_WORD_C(0)
			       << (WORD_WIDTH - 1 - (n - 1) % WORD_WIDTH);
    CAMLreturn (s_v);
}

value
bitlib_bitstring_cat(value sA_v, value sB_v)
{
    CAMLparam2 (sA_v, sB_v);
    CAMLlocal1 (s_v);
    size_t i, n, m;
    size_t nA = BITSTRING(sA_v)->len;
    size_t nB = BITSTRING(sB_v)->len;

    if (nA == 0) CAMLreturn (sB_v);
    if (nB == 0) CAMLreturn (sA_v);

    n = nA + nB;
    m = WORD_CNT(n);
    s_v = _bitstring_alloc(m);
    BITSTRING(s_v)->len = n;
    if (!BITSTRING(s_v)->arr)
	caml_raise_out_of_memory();
    memcpy(BITSTRING(s_v)->arr, BITSTRING(sA_v)->arr,
	   nA / WORD_WIDTH * WORD_SIZE);
    if (nA % WORD_WIDTH == 0)
	memcpy(BITSTRING(s_v)->arr + nA / WORD_WIDTH, BITSTRING(sB_v)->arr,
	       WORD_CNT(nB) * WORD_SIZE);
    else {
	bitlib_word_t w0, w1;
	int j = nA % WORD_WIDTH;
	size_t m0 = nA / WORD_WIDTH + WORD_CNT(nB);
	w0 = BITSTRING(sA_v)->arr[nA / WORD_WIDTH] >> (WORD_WIDTH - j);
	for (i = nA / WORD_WIDTH; i < m0; ++i) {
	    w1 = BITSTRING(sB_v)->arr[i - nA / WORD_WIDTH];
	    BITSTRING(s_v)->arr[i] = (w0 << (WORD_WIDTH - j)) | (w1 >> j);
	    w0 = w1;
	}
	if (m0 < m)
	    BITSTRING(s_v)->arr[m0] = w0 << (WORD_WIDTH - j);
    }
    CAMLreturn (s_v);
}
