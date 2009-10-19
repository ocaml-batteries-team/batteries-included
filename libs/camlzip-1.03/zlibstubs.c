/***********************************************************************/
/*                                                                     */
/*                      The CamlZip library                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file LICENSE.        */
/*                                                                     */
/***********************************************************************/

/* $Id: zlibstubs.c,v 1.3 2006/04/04 08:29:07 xleroy Exp $ */

/* Stub code to interface with Zlib */

#include <zlib.h>

#ifdef USE_BZIP2
#include <bzlib.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

#define ZStream_val(v) ((z_stream *) (v))

static value * camlzip_error_exn = NULL;

static void camlzip_error(char * fn, value vzs)
{
  char * msg;
  value s1 = Val_unit, s2 = Val_unit, bucket = Val_unit;

  msg = ZStream_val(vzs)->msg;
  if (msg == NULL) msg = "";
  if (camlzip_error_exn == NULL) {
    camlzip_error_exn = caml_named_value("Zlib.Error");
    if (camlzip_error_exn == NULL)
      invalid_argument("Exception Zlib.Error not initialized");
  }
  Begin_roots3(s1, s2, bucket);
    s1 = copy_string(fn);
    s2 = copy_string(msg);
    bucket = alloc_small(3, 0);
    Field(bucket, 0) = *camlzip_error_exn;
    Field(bucket, 1) = s1;
    Field(bucket, 2) = s2;
  End_roots();
  mlraise(bucket);
}

static value camlzip_new_stream(void)
{
  value res = alloc((sizeof(z_stream) + sizeof(value) - 1) / sizeof(value),
                    Abstract_tag);
  ZStream_val(res)->zalloc = NULL;
  ZStream_val(res)->zfree = NULL;
  ZStream_val(res)->opaque = NULL;
  ZStream_val(res)->next_in = NULL;
  ZStream_val(res)->next_out = NULL;
  return res;
}

value camlzip_deflateInit(value vlevel, value expect_header)
{
  value vzs = camlzip_new_stream();
  if (deflateInit2(ZStream_val(vzs),
                   Int_val(vlevel),
                   Z_DEFLATED,
                   Bool_val(expect_header) ? MAX_WBITS : -MAX_WBITS,
                   8,
                   Z_DEFAULT_STRATEGY) != Z_OK)
    camlzip_error("Zlib.deflateInit", vzs);
  return vzs;
}

static int camlzip_flush_table[] = 
{ Z_NO_FLUSH, Z_SYNC_FLUSH, Z_FULL_FLUSH, Z_FINISH };

value camlzip_deflate(value vzs, value srcbuf, value srcpos, value srclen,
                      value dstbuf, value dstpos, value dstlen,
                      value vflush)
{
  z_stream * zs = ZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte_u(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte_u(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = deflate(zs, camlzip_flush_table[Int_val(vflush)]);
  if (retcode < 0) camlzip_error("Zlib.deflate", vzs);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;         /* not required, but cleaner */
  zs->next_out = NULL;        /* (avoid dangling pointers into Caml heap) */
  res = alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == Z_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
}

value camlzip_deflate_bytecode(value * arg, int nargs)
{
  return camlzip_deflate(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6], arg[7]);
}

value camlzip_deflateEnd(value vzs)
{
  if (deflateEnd(ZStream_val(vzs)) != Z_OK)
    camlzip_error("Zlib.deflateEnd", vzs);
  return Val_unit;
}

value camlzip_inflateInit(value expect_header)
{
  value vzs = camlzip_new_stream();
  if (inflateInit2(ZStream_val(vzs),
                   Bool_val(expect_header) ? MAX_WBITS : -MAX_WBITS) != Z_OK)
    camlzip_error("Zlib.inflateInit", vzs);
  return vzs;
}

value camlzip_inflate(value vzs, value srcbuf, value srcpos, value srclen,
                      value dstbuf, value dstpos, value dstlen,
                      value vflush)
{
  z_stream * zs = ZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte_u(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte_u(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = inflate(zs, camlzip_flush_table[Int_val(vflush)]);
  if (retcode < 0 || retcode == Z_NEED_DICT)
    camlzip_error("Zlib.inflate", vzs);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;           /* not required, but cleaner */
  zs->next_out = NULL;          /* (avoid dangling pointers into Caml heap) */
  res = alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == Z_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
}

value camlzip_inflate_bytecode(value * arg, int nargs)
{
  return camlzip_inflate(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6], arg[7]);
}

value camlzip_inflateEnd(value vzs)
{
  if (inflateEnd(ZStream_val(vzs)) != Z_OK)
    camlzip_error("Zlib.inflateEnd", vzs);
  return Val_unit;
}

value camlzip_update_crc32(value crc, value buf, value pos, value len)
{
  return copy_int32(crc32((uint32) Int32_val(crc), 
                          &Byte_u(buf, Long_val(pos)),
                          Long_val(len)));
}

/* Bzip2 interface code */

#define BZStream_val(v) ((bz_stream *) (v))

static value * camlzip_bzerror_exn = NULL;

#ifdef USE_BZIP2
static void camlzip_bzerror(char * fn, int err)
{
  char * msg;
  value s1 = Val_unit, s2 = Val_unit, bucket = Val_unit;

  if (camlzip_bzerror_exn == NULL) {
    camlzip_bzerror_exn = caml_named_value("Bzlib.Error");
    if (camlzip_bzerror_exn == NULL)
      invalid_argument("Exception Bzlib.Error not initialized");
  }
  Begin_roots3(s1, s2, bucket);
    s1 = copy_string(fn);
    switch (err) {
    case BZ_CONFIG_ERROR:
      s2 = Val_int(0);
      break;
    case BZ_SEQUENCE_ERROR:
      s2 = Val_int(1);
      break;
    case BZ_PARAM_ERROR:
      s2 = Val_int(2);
      break;
    case BZ_MEM_ERROR:
      s2 = Val_int(3);
      break;
    case BZ_DATA_ERROR:
      s2 = Val_int(4);
      break;
    case BZ_DATA_ERROR_MAGIC:
      s2 = Val_int(5);
      break;
    default:
      s2 = Val_int(6);
    }
    bucket = alloc_small(3, 0);
    Field(bucket, 0) = *camlzip_bzerror_exn;
    Field(bucket, 1) = s1;
    Field(bucket, 2) = s2;
  End_roots();
  mlraise(bucket);
}

static value camlzip_new_bzstream(void)
{
  value res = alloc((sizeof(bz_stream) + sizeof(value) - 1) / sizeof(value),
                    Abstract_tag);
  ZStream_val(res)->zalloc = NULL;
  ZStream_val(res)->zfree = NULL;
  ZStream_val(res)->opaque = NULL;
  ZStream_val(res)->next_in = NULL;
  ZStream_val(res)->next_out = NULL;
  return res;
}

int camlzip_action_table[] = { BZ_RUN, BZ_FLUSH, BZ_FINISH };
#endif


value camlzip_bzCompressInit(value blockSize100k, value verbosity, value workFactor) {
#ifdef USE_BZIP2
  int err;
  value vbzs = camlzip_new_bzstream();
  if ((err = BZ2_bzCompressInit(BZStream_val(vbzs),
			 Int_val(blockSize100k),
			 Int_val(verbosity),
			 Int_val(workFactor))) != BZ_OK)
    camlzip_bzerror("Zlib.deflateInit", err);
  return vbzs;
#else
  failwith("Bzip2 compression not supported.");
#endif
}

value camlzip_bzCompress(value vzs, value srcbuf, value srcpos, value srclen,
                      value dstbuf, value dstpos, value dstlen,
                      value vflush)
{
#ifdef USE_BZIP2
  bz_stream * zs = BZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = BZ2_bzCompress(zs, camlzip_action_table[Int_val(vflush)]);
  if (retcode < 0) camlzip_bzerror("Bzlib.compress", retcode);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;         /* not required, but cleaner */
  zs->next_out = NULL;        /* (avoid dangling pointers into Caml heap) */
  res = alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == BZ_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
#else
  failwith("Bzip2 compression not supported");
#endif
}

value camlzip_bzCompress_bytecode(value * arg, int nargs)
{
  return camlzip_bzCompress(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6], arg[7]);
}

value camlzip_bzCompressEnd(value stream) {
#ifdef USE_BZIP2
  int err;
  if ((err = BZ2_bzCompressEnd(BZStream_val(stream))) != BZ_OK)
    camlzip_bzerror("Bzlib.compress_end", err);
#else
  failwith("Bzip2 compression not supported");
#endif
  return Val_unit;
}

value camlzip_bzDecompressInit(value verbosity, value small)
{
#ifdef USE_BZIP2
  int err;
  value vzs = camlzip_new_bzstream();
  if ((err = BZ2_bzDecompressInit(BZStream_val(vzs), Int_val(verbosity), Bool_val(small))) != BZ_OK)
    camlzip_bzerror("Bzlib.decompress_init", err);
  return vzs;
#else
  failwith("Bzip2 compression not supported");
#endif
}

value camlzip_bzDecompress(value vzs, value srcbuf, value srcpos, value srclen,
			   value dstbuf, value dstpos, value dstlen)
{
#ifdef USE_BZIP2
  bz_stream * zs = BZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = BZ2_bzDecompress(zs);
  if (retcode < 0)
    camlzip_bzerror("Bzlib.decompress", retcode);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;           /* not required, but cleaner */
  zs->next_out = NULL;          /* (avoid dangling pointers into Caml heap) */
  res = alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == BZ_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
#else
  failwith("Bzip2 compression not supported");
#endif
}

value camlzip_bzDecompress_bytecode(value * arg, int nargs)
{
  return camlzip_bzDecompress(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6]);
}

value camlzip_bzDecompressEnd(value stream) {
#ifdef USE_BZIP2
  int err;
  if ((err = BZ2_bzDecompressEnd(BZStream_val(stream))) != BZ_OK)
    camlzip_bzerror("Bzlib.decompressEnd", err);
#else
  failwith("Bzip2 compression not supported");
#endif
  return Val_unit;
}
