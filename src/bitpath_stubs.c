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

#include "bitpath.h"

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
#define WORD_ROT(i, k) ((i << k) | (i >> (8*sizeof(bitpath_word_t) - i)))
#define WORD_WIDTH BITPATH_WORD_WIDTH
#define WORD_SIZE (BITPATH_WORD_WIDTH / 8)
#define WORD_SZ16 (BITPATH_WORD_WIDTH / 16)
#define WORD_CNT(n) (((n) + WORD_WIDTH - 1) / WORD_WIDTH)
#define WORD_MOD(i) (WORD_WIDTH - 1 - (i) % WORD_WIDTH)
#define WORD_MOD8(i) ((WORD_SIZE - 1 - (i) % WORD_SIZE) * 8)
#define WORD_MOD16(i) ((WORD_SZ16 - 1 - (i) % WORD_SZ16) * 16)

#define BITPATH_GET(i, s)  ((s->arr[(i) / WORD_WIDTH] >> WORD_MOD(i)) & 1)
#define BITPATH_GET8(i, s) ((s->arr[(i) / WORD_SIZE] >> WORD_MOD8(i)) & 0xff)
#define BITPATH_GET16(i, s) \
    ((s->arr[(i) / WORD_SZ16] >> WORD_MOD16(i)) & 0xffff)

#ifndef BITPATH_INLINED
static void
_bitpath_finalize(value s_v)
{
    free(BITPATH(s_v)->arr);
}
#endif

static int
_bitpath_compare(value sA_v, value sB_v)
{
    size_t i;
    size_t mA = WORD_CNT(BITPATH(sA_v)->len);
    size_t mB = WORD_CNT(BITPATH(sB_v)->len);
    for (i = 0; i < MIN(mA, mB); ++i) {
	bitpath_word_t wA = BITPATH(sA_v)->arr[i];
	bitpath_word_t wB = BITPATH(sB_v)->arr[i];
	if (wA < wB) return -1;
	if (wA > wB) return 1;
    }
    if (mA < mB) return -1;
    if (mA > mB) return 1;
    return 0;
}

value
camlbitpath_compare(value sA_v, value sB_v)
{
    return Val_int(_bitpath_compare(sA_v, sB_v));
}

/* Based on Bob Jenkins' hash functions. */
#if BITPATH_WORD_WIDTH >= 64
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
_bitpath_hash(value s_v)
{
    size_t i, m = WORD_CNT(BITPATH(s_v)->len);
    long a, b, c;
    a = 0xba5ebee7;
    b = 0xcafebabe;
    c = BITPATH(s_v)->len;
    for (i = 0; i < m - 2; i += 3) {
	a += BITPATH(s_v)->arr[i];
	b += BITPATH(s_v)->arr[i + 1];
	c += BITPATH(s_v)->arr[i + 2];
	MIX(a, b, c);
    }
    if (i < m) {
	a += BITPATH(s_v)->arr[i];
	if (i + 1 < m) b += BITPATH(s_v)->arr[i + 1];
	MIX(a, b, c);
    }
    return c;
}

static struct custom_operations _bitpath_ops = {
    "org.eideticdew.p.bitpath",
#ifndef BITPATH_INLINED
    _bitpath_finalize,
#else
    custom_finalize_default,
#endif
    _bitpath_compare,
    _bitpath_hash,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
};

static value
_bitpath_alloc(size_t m)
{
#ifdef BITPATH_INLINED
    return caml_alloc_custom(&_bitpath_ops,
	    sizeof(struct bitpath) + (m - 1)*sizeof(bitpath_word_t),
	    0, 1);
#else
    value s = caml_alloc_custom(&_bitpath_ops,
				sizeof(struct bitpath), 0, 1);
    BITPATH(s)->arr = malloc(m * WORD_SIZE);
    return s;
#endif
}

static value _bitpath_empty = Val_unit;

value
camlbitpath_make_empty(value _)
{
    assert(_bitpath_empty == Val_unit);
    caml_register_global_root(&_bitpath_empty);
    _bitpath_empty = _bitpath_alloc(0);
    BITPATH(_bitpath_empty)->len = 0;
    return _bitpath_empty;
}

value
camlbitpath_init(value n_v, value f_v)
{
    CAMLparam2 (n_v, f_v);
    CAMLlocal1 (s_v);
    size_t i, j, n = Long_val(n_v);
    size_t m0 = n / WORD_WIDTH;
    size_t m = WORD_CNT(n);

    s_v = _bitpath_alloc(m);
    BITPATH(s_v)->len = n;
    if (!BITPATH(s_v)->arr)
	caml_raise_out_of_memory();
    for (i = 0; i < m0; ++i) {
	bitpath_word_t w = 0;
	for (j = 0; j < WORD_WIDTH; ++j) {
	    int x = Bool_val(caml_callback(f_v, Val_long(i * WORD_WIDTH + j)));
	    w <<= 1;
	    w |= x;
	}
	BITPATH(s_v)->arr[i] = w;
    }
    if (m0 < m) {
	bitpath_word_t w = 0;
	for (j = 0; j < n % WORD_WIDTH; ++j) {
	    int x = Bool_val(caml_callback(f_v, Val_long(m0 * WORD_WIDTH + j)));
	    w <<= 1;
	    w |= x;
	}
	BITPATH(s_v)->arr[m0] = w << (WORD_WIDTH - n % WORD_WIDTH);
    }
    CAMLreturn (s_v);
}

value
camlbitpath_init8(value n_v, value f_v)
{
    CAMLparam2 (n_v, f_v);
    CAMLlocal1 (s_v);
    size_t i, j, n = Long_val(n_v);
    size_t m0 = n / WORD_WIDTH;
    size_t m = WORD_CNT(n);

    s_v = _bitpath_alloc(m);
    BITPATH(s_v)->len = n;
    if (!BITPATH(s_v)->arr)
	caml_raise_out_of_memory();
    for (i = 0; i < m0; ++i) {
	bitpath_word_t w = 0;
	for (j = 0; j < WORD_SIZE; ++j) {
	    int x = Int_val(caml_callback(f_v, Val_long(i * WORD_SIZE + j)));
	    w <<= 8;
	    w |= x;
	}
	BITPATH(s_v)->arr[i] = w;
    }
    if (m0 < m) {
	bitpath_word_t w = 0;
	int n_oct = (n % WORD_WIDTH + 7) / 8;
	for (j = 0; j < n_oct; ++j) {
	    int x = Int_val(caml_callback(f_v, Val_long(m0 * WORD_SIZE + j)));
	    w <<= 8;
	    w |= x;
	}
	w >>= n_oct * 8 - n % WORD_WIDTH;
	BITPATH(s_v)->arr[m0] = w << (WORD_WIDTH - 1 - (n - 1) % WORD_WIDTH);
    }
    CAMLreturn (s_v);
}

value
camlbitpath_init16(value n_v, value f_v)
{
    CAMLparam2 (n_v, f_v);
    CAMLlocal1 (s_v);
    size_t i, j, n = Long_val(n_v);
    size_t m0 = n / WORD_WIDTH;
    size_t m = WORD_CNT(n);

    s_v = _bitpath_alloc(m);
    BITPATH(s_v)->len = n;
    if (!BITPATH(s_v)->arr)
	caml_raise_out_of_memory();
    for (i = 0; i < m0; ++i) {
	bitpath_word_t w = 0;
	for (j = 0; j < WORD_SZ16; ++j) {
	    int x = Int_val(caml_callback(f_v, Val_long(i*WORD_SZ16 + j)));
	    w <<= 16;
	    w |= x;
	}
	BITPATH(s_v)->arr[i] = w;
    }
    if (m0 < m) {
	bitpath_word_t w = 0;
	int n_hxd = (n % WORD_WIDTH + 15) / 16;
	for (j = 0; j < n_hxd; ++j) {
	    int x = Int_val(caml_callback(f_v, Val_long(m0 * WORD_SZ16 + j)));
	    w <<= 16;
	    w |= x;
	}
	w >>= n_hxd * 16 - n % WORD_WIDTH;
	BITPATH(s_v)->arr[m0] = w << (WORD_WIDTH - 1 - (n - 1) % WORD_WIDTH);
    }
    CAMLreturn (s_v);
}

#include <stdio.h>
value
camlbitpath_const(value n_v, value x_v)
{
    CAMLparam2 (n_v, x_v);
    CAMLlocal1 (s_v);
    size_t i, n, m;
    bitpath_word_t x;

    n = Long_val(n_v);
    if (n == 0) {
	assert(_bitpath_empty != Val_unit);
	CAMLreturn (_bitpath_empty);
    }
    m = WORD_CNT(n);
    x = Bool_val(x_v)? ~BITPATH_WORD_C(0) : 0;
    s_v = _bitpath_alloc(m);
    BITPATH(s_v)->len = n;
    if (!BITPATH(s_v)->arr)
	caml_raise_out_of_memory();
    for (i = 0; i < m - 1; ++i)
	BITPATH(s_v)->arr[i] = x;
    BITPATH(s_v)->arr[m - 1] = x << (WORD_WIDTH - 1 - (n - 1) % WORD_WIDTH);
    CAMLreturn (s_v);
}

value
camlbitpath_not(value sA_v)
{
    CAMLparam1 (sA_v);
    CAMLlocal1 (sN_v);
    size_t i, n, m;
    n = BITPATH(sA_v)->len;
    if (n == 0) {
	assert(_bitpath_empty != Val_unit);
	CAMLreturn (_bitpath_empty);
    }
    m = WORD_CNT(n);
    sN_v = _bitpath_alloc(m);
    BITPATH(sN_v)->len = n;
    for (i = 0; i < m - 1; ++i)
	BITPATH(sN_v)->arr[i] = ~BITPATH(sA_v)->arr[i];
    BITPATH(sN_v)->arr[m - 1] = ~BITPATH(sA_v)->arr[m - 1]
	    & (~BITPATH_WORD_C(0) << (WORD_WIDTH - 1 - (n - 1) % WORD_WIDTH));
    CAMLreturn (sN_v);
}

value
camlbitpath_length(value s_v)
{
    return Val_long(BITPATH(s_v)->len);
}

value
camlbitpath_get(value i_v, value s_v)
{
    size_t i = Long_val(i_v);
    return Val_bool(BITPATH_GET(i, BITPATH(s_v)));
}

value
camlbitpath_get8(value i_v, value s_v)
{
    size_t i = Long_val(i_v);
    return Val_int(BITPATH_GET8(i, BITPATH(s_v)));
}

value
camlbitpath_get16(value i_v, value s_v)
{
    size_t i = Long_val(i_v);
    return Val_int(BITPATH_GET16(i, BITPATH(s_v)));
}

value
camlbitpath_coprefix_length(value sA_v, value sB_v)
{
    size_t i, n = MIN(BITPATH(sA_v)->len, BITPATH(sB_v)->len);
    size_t j, m = n / WORD_WIDTH;
    for (j = 0; j < m; ++j)
	if (BITPATH(sA_v)->arr[j] != BITPATH(sB_v)->arr[j])
	    break;
    for (i = j * WORD_WIDTH; i < n; ++i)
	if (BITPATH_GET(i, BITPATH(sA_v)) !=
	    BITPATH_GET(i, BITPATH(sB_v)))
	    break;
    return Val_long(i);
}

value
camlbitpath_coslice_length(value iA_v, value sA_v, value iB_v, value sB_v)
{
    size_t iA = Long_val(iA_v);
    size_t iB = Long_val(iB_v);
    size_t i, n = MIN(BITPATH(sA_v)->len - iA, BITPATH(sB_v)->len - iB);
    /* TODO: This can be optimized to operate on words, but it'll take some
     * coding for unaligned cases. */
    for (i = 0; i < n; ++i)
	if (BITPATH_GET(iA + i, BITPATH(sA_v)) !=
	    BITPATH_GET(iB + i, BITPATH(sB_v)))
	    break;
    return Val_long(i);
}

value
camlbitpath_slice(value iL_v, value iU_v, value sA_v)
{
    static char errbuf[160 + 3*sizeof(size_t)]; /* NB! Max 2 lines & 2 %zd's */
    CAMLparam3 (sA_v, iL_v, iU_v);
    CAMLlocal1 (s_v);
    size_t iL = Long_val(iL_v);
    size_t iU = Long_val(iU_v);
    size_t nA, n, m;

    if (iL < 0) {
	sprintf(errbuf, "Bitpath.slice: The lower bound %zd is negative.", iL);
	caml_invalid_argument(errbuf);
    }
    if (iL > iU) {
	sprintf(errbuf, "Bitpath.slice: "
		"The lower bound %zd larger than the upper bound %zd.", iL, iU);
	caml_invalid_argument(errbuf);
    }

    if (iL == iU) {
	assert(_bitpath_empty != Val_unit);
	CAMLreturn (_bitpath_empty);
    }
    nA = BITPATH(sA_v)->len;
    if (iU > nA) {
	sprintf(errbuf, "Bitpath.slice: "
		"Upper bound %zd is larger than the length %zd.", iU, nA);
	caml_invalid_argument(errbuf);
    }
    if (iL == 0 && iU == nA) CAMLreturn (sA_v);

    n = iU - iL;
    m = WORD_CNT(n);
    assert(m > 0);

    s_v = _bitpath_alloc(m);
    BITPATH(s_v)->len = n;
    if (!BITPATH(s_v)->arr)
	caml_raise_out_of_memory();

    if (iL % WORD_WIDTH == 0)
	memcpy(BITPATH(s_v)->arr, BITPATH(sA_v)->arr + iL / WORD_WIDTH,
	       m * WORD_SIZE);
    else {
	bitpath_word_t w0, w1;
	int jL = iL % WORD_WIDTH;
	size_t k, kL = iL / WORD_WIDTH;
	w0 = BITPATH(sA_v)->arr[kL];
	for (k = 0; k < m; ++k) {
	    w1 = BITPATH(sA_v)->arr[kL + k + 1];
	    BITPATH(s_v)->arr[k] = (w0 << jL) | (w1 >> (WORD_WIDTH - jL));
	    w0 = w1;
	}
    }
    BITPATH(s_v)->arr[m - 1] &= ~BITPATH_WORD_C(0)
			     << (WORD_WIDTH - 1 - (n - 1) % WORD_WIDTH);
    CAMLreturn (s_v);
}

value
camlbitpath_cat(value sA_v, value sB_v)
{
    CAMLparam2 (sA_v, sB_v);
    CAMLlocal1 (s_v);
    size_t i, n, m;
    size_t nA = BITPATH(sA_v)->len;
    size_t nB = BITPATH(sB_v)->len;

    if (nA == 0) CAMLreturn (sB_v);
    if (nB == 0) CAMLreturn (sA_v);

    n = nA + nB;
    m = WORD_CNT(n);
    s_v = _bitpath_alloc(m);
    BITPATH(s_v)->len = n;
    if (!BITPATH(s_v)->arr)
	caml_raise_out_of_memory();
    memcpy(BITPATH(s_v)->arr, BITPATH(sA_v)->arr,
	   nA / WORD_WIDTH * WORD_SIZE);
    if (nA % WORD_WIDTH == 0)
	memcpy(BITPATH(s_v)->arr + nA / WORD_WIDTH, BITPATH(sB_v)->arr,
	       WORD_CNT(nB) * WORD_SIZE);
    else {
	bitpath_word_t w0, w1;
	int j = nA % WORD_WIDTH;
	size_t m0 = nA / WORD_WIDTH + WORD_CNT(nB);
	w0 = BITPATH(sA_v)->arr[nA / WORD_WIDTH] >> (WORD_WIDTH - j);
	for (i = nA / WORD_WIDTH; i < m0; ++i) {
	    w1 = BITPATH(sB_v)->arr[i - nA / WORD_WIDTH];
	    BITPATH(s_v)->arr[i] = (w0 << (WORD_WIDTH - j)) | (w1 >> j);
	    w0 = w1;
	}
	if (m0 < m)
	    BITPATH(s_v)->arr[m0] = w0 << (WORD_WIDTH - j);
    }
    CAMLreturn (s_v);
}
