(* 
 * CharEncodings - Encoding of characters.
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open BatIO

(**
   Character encodings.

   {topic Unicode}

   When computers were first created, little thought was given to the
   ability of writing text in languages other than English. With the
   years, various notations (or "encodings") emerged, adapted to
   different languages: the notation called Latin-1 for most
   West-European languages, Euc-Kr for Corean, etc. Universal
   conventions were later introduced, which are supposed to be
   sufficient to represent all human languages, with several possible
   encodings, such as UTF-8, UTF-16, UTF-32, ...

   This module deals with conversions between the possible encodings
   of inputs and outputs.

   @author Yamagata Yoriyuki (Camomile module)
   @author David Teller
*)

exception Malformed_code
(** An exception raised when a character is meaningless in the encoding
    in which it appears. This is usually the sign that the encoding used
    was incorrect.*)
    

(**
   The list if known encodings.
*)
type encoding =
[ `utf8
| `utf16
| `utf16be
| `utf16le
| `utf32
| `utf32be
| `utf32le
| `ucs4
| `ansi_x3_110_1983
| `ansi_x3_4_1968
| `ansi_x3_4_1986
| `arabic7
| `arabic
| `armscii_8
| `ascii                (**US ASCII encoding*)
| `asmo_449
| `asmo_708
| `big5_cp950
| `big5_hkscs
| `big5hkscs
| `big5
| `bs_4730
| `bs_viewdata
| `ca
| `charset_1026
| `charset_1047
| `charset_437
| `charset_500
| `charset_500v1
| `charset_850
| `charset_851
| `charset_852
| `charset_855
| `charset_856
| `charset_857
| `charset_860
| `charset_861
| `charset_862
| `charset_863
| `charset_865
| `charset_866
| `charset_866nav
| `charset_869
| `charset_904
| `cn
| `cp037
| `cp038
| `cp10007
| `cp1004
| `cp1026
| `cp1047
| `cp1124
| `cp1125
| `cp1129
| `cp1132
| `cp1133
| `cp1160
| `cp1161
| `cp1162
| `cp1163
| `cp1164
| `cp1250
| `cp1251
| `cp1252
| `cp1253
| `cp1254
| `cp1255
| `cp1256
| `cp1257
| `cp1258
| `cp273
| `cp274
| `cp275
| `cp278
| `cp280
| `cp281
| `cp284
| `cp285
| `cp290
| `cp297
| `cp367
| `cp420
| `cp423
| `cp424
| `cp437
| `cp500
| `cp737
| `cp775
| `cp819
| `cp850
| `cp851
| `cp852
| `cp855
| `cp856
| `cp857
| `cp860
| `cp861
| `cp862
| `cp863
| `cp864
| `cp865
| `cp866
| `cp866nav
| `cp868
| `cp869
| `cp870
| `cp871
| `cp874
| `cp875
| `cp880
| `cp891
| `cp903
| `cp904
| `cp905
| `cp918
| `cp922
| `cp932
| `cp949
| `cp_ar
| `cp_gr
| `cp_hu
| `cp_is
| `csa7_1
| `csa7_2
| `csa_t500_1983
| `csa_z243_4_1985_1
| `csa_z243_4_1985_2
| `csa_z243_4_1985_gr
| `csn_369103
| `cuba
| `cwi_2
| `cwi
| `cyrillic
| `dec
| `dec_mcs
| `de
| `din_66003
| `dk
| `ds_2089
| `ds2089
| `e13b
| `ebcdic_at_de_a
| `ebcdic_at_de
| `ebcdic_be
| `ebcdic_br
| `ebcdic_ca_fr
| `ebcdic_cp_ar1
| `ebcdic_cp_ar2
| `ebcdic_cp_be
| `ebcdic_cp_ca
| `ebcdic_cp_ch
| `ebcdic_cp_dk
| `ebcdic_cp_es
| `ebcdic_cp_fi
| `ebcdic_cp_fr
| `ebcdic_cp_gb
| `ebcdic_cp_gr
| `ebcdic_cp_he
| `ebcdic_cp_is
| `ebcdic_cp_it
| `ebcdic_cp_nl
| `ebcdic_cp_no
| `ebcdic_cp_roece
| `ebcdic_cp_se
| `ebcdic_cp_tr
| `ebcdic_cp_us
| `ebcdic_cp_wt
| `ebcdic_cp_yu
| `ebcdic_cyrillic
| `ebcdic_dk_no_a
| `ebcdic_dk_no
| `ebcdic_es_a
| `ebcdic_es
| `ebcdic_es_s
| `ebcdic_fi_se_a
| `ebcdic_fi_se
| `ebcdic_fr
| `ebcdic_greek
| `ebcdic_int1
| `ebcdic_int
| `ebcdic_is_friss
| `ebcdic_it
| `ebcdic_jp_e
| `ebcdic_jp_kana
| `ebcdic_pt
| `ebcdic_uk
| `ebcdic_us
| `ecma_114
| `ecma_118
| `ecma_cyrillic
| `elot_928
| `es2
| `es
| `euc_jisx0213
| `euc_jp
| `euc_kr
| `euc_tw
| `fi
| `friss
| `fr
| `gb18030
| `gb_1988_80
| `gb2312
| `gbk
| `gb
| `georgian_academy
| `georgian_ps
| `gost_19768_74
| `greek7
| `greek7_old
| `greek8
| `greek_ccitt
| `greek
| `hebrew
| `hp_roman8
| `hu
| `ibm037
| `ibm038
| `ibm1004
| `ibm1026
| `ibm1047
| `ibm1124
| `ibm1129
| `ibm1132
| `ibm1133
| `ibm1160
| `ibm1161
| `ibm1162
| `ibm1163
| `ibm1164
| `ibm256
| `ibm273
| `ibm274
| `ibm275
| `ibm277
| `ibm278
| `ibm280
| `ibm281
| `ibm284
| `ibm285
| `ibm290
| `ibm297
| `ibm367
| `ibm420
| `ibm423
| `ibm424
| `ibm437
| `ibm500
| `ibm819
| `ibm848
| `ibm850
| `ibm851
| `ibm852
| `ibm855
| `ibm856
| `ibm857
| `ibm860
| `ibm861
| `ibm862
| `ibm863
| `ibm864
| `ibm865
| `ibm866
| `ibm866nav
| `ibm868
| `ibm869
| `ibm870
| `ibm871
| `ibm874
| `ibm875
| `ibm880
| `ibm891
| `ibm903
| `ibm904
| `ibm905
| `ibm918
| `ibm922
| `iec_p27_1
| `inis_8
| `inis_cyrillic
| `inis
| `invariant
| `irv
| `isiri_3342
| `iso_10367_box
| `iso_2033_1983
| `iso_5427_1981
| `iso_5427_ext
| `iso_5427
| `iso_5428_1980
| `iso_5428
| `iso_646_basic_1983
| `iso_646_basic
| `iso646_ca2
| `iso646_ca
| `iso646_cn
| `iso646_cu
| `iso646_de
| `iso646_dk
| `iso646_es2
| `iso646_es
| `iso646_fi
| `iso646_fr1
| `iso646_fr
| `iso646_gb
| `iso646_hu
| `iso_646_irv_1983
| `iso_646_irv_1991
| `iso_646_irv
| `iso646_it
| `iso646_jp
| `iso646_jp_ocr_b
| `iso646_kr
| `iso646_no2
| `iso646_no
| `iso646_pt2
| `iso646_pt
| `iso646_se2
| `iso646_se
| `iso646_us
| `iso646_yu
| `iso_6937_1992
| `iso_6937_2_1983
| `iso_6937_2_25
| `iso_6937_2_add
| `iso_6937
| `iso6937
| `iso_8859_10_1992
| `iso_8859_10
| `iso_8859_1_1987
| `iso_8859_11
| `iso_8859_13
| `iso_8859_14
| `iso_8859_15(** West-european characters, including the Euro currency.*)
| `iso_8859_16
| `iso_8859_1 (** West-european characters, without Euro.*)
| `iso_8859_2_1987
| `iso_8859_2
| `iso_8859_3_1988
| `iso_8859_3
| `iso_8859_4_1988
| `iso_8859_4
| `iso_8859_5_1988
| `iso_8859_5
| `iso_8859_6_1987
| `iso_8859_6
| `iso_8859_7_1987
| `iso_8859_7
| `iso_8859_8_1988
| `iso_8859_8
| `iso_8859_9_1989
| `iso_8859_9
| `iso_8859_supp
| `iso_9036
| `iso_ir_100
| `iso_ir_101
| `iso_ir_102
| `iso_ir_103
| `iso_ir_109
| `iso_ir_10
| `iso_ir_110
| `iso_ir_111
| `iso_ir_11
| `iso_ir_121
| `iso_ir_122
| `iso_ir_123
| `iso_ir_126
| `iso_ir_127
| `iso_ir_128
| `iso_ir_138
| `iso_ir_139
| `iso_ir_141
| `iso_ir_142
| `iso_ir_143
| `iso_ir_144
| `iso_ir_146
| `iso_ir_147
| `iso_ir_148
| `iso_ir_14
| `iso_ir_150
| `iso_ir_151
| `iso_ir_152
| `iso_ir_153
| `iso_ir_154
| `iso_ir_155
| `iso_ir_156
| `iso_ir_157
| `iso_ir_158
| `iso_ir_15
| `iso_ir_166
| `iso_ir_16
| `iso_ir_170
| `iso_ir_179
| `iso_ir_17
| `iso_ir_18
| `iso_ir_197
| `iso_ir_19
| `iso_ir_209
| `iso_ir_21
| `iso_ir_226
| `iso_ir_25
| `iso_ir_27
| `iso_ir_2
| `iso_ir_37
| `iso_ir_47
| `iso_ir_49
| `iso_ir_4
| `iso_ir_50
| `iso_ir_51
| `iso_ir_54
| `iso_ir_55
| `iso_ir_57
| `iso_ir_60
| `iso_ir_61
| `iso_ir_69
| `iso_ir_6
| `iso_ir_70
| `iso_ir_8_1
| `iso_ir_84
| `iso_ir_85
| `iso_ir_86
| `iso_ir_88
| `iso_ir_89
| `iso_ir_90
| `iso_ir_9_1
| `iso_ir_92
| `iso_ir_98
| `iso_ir_99
| `it
| `jis_c6220_1969_ro
| `jis_c6229_1984_b
| `jis_x0201
| `johab
| `jp
| `jp_ocr_b
| `js
| `jus_i_b1_002
| `jus_i_b1_003_mac
| `jus_i_b1_003_serb
| `koi_7
| `koi_8
| `koi8_r
| `koi8_t
| `koi8_u
| `ksc5636
| `l10
| `l1
| `l2
| `l3
| `l4
| `l5
| `l6
| `l7
| `l8
| `lap
| `latin10
| `latin1_2_5
| `latin1 (** West-european characters.*)
| `latin2
| `latin3
| `latin4
| `latin5
| `latin6
| `latin7
| `latin8
| `latin_greek_1
| `latin_greek
| `latin_lap
| `mac_cyrillic
| `macedonian
| `macintosh
| `mac_is
| `mac
| `mac_sami
| `mac_uk
| `ms_ansi
| `ms_arab
| `ms_cyrl
| `ms_ee
| `ms_greek
| `ms_hebr
| `ms_turk
| `msz_7795_3
| `naplps
| `nats_dano
| `nats_sefi
| `nc_nc00_10_81
| `nc_nc00_10
| `next
| `nextstep
| `nf_z_62_010_1973
| `nf_z_62_010
| `no2
| `no
| `ns_4551_1
| `ns_4551_2
| `os2latin1
| `pt2
| `pt
| `r8
| `ref_encoding
| `roman8
| `ruscii
| `sami
| `se2
| `se
| `sen_850200_b
| `sen_850200_c
| `serbian
| `shift_jis
| `shift_jisx0213
| `sjis
| `ss636127
| `st_sev_358_88
| `t_101_g2
| `t_61_7bit
| `t_61_8bit
| `t_61
| `tcvn5712_1_1993
| `tcvn5712_1
| `tcvn_5712
| `tcvn
| `tis620_0
| `tis620_2529_1
| `tis620_2533_0
| `tis_620
| `tis620
| `uk
| `us_ascii
| `us
| `videotex_suppl
| `viscii
| `winbaltrim
| `windows_sami2
| `win_sami_2
| `ws2
| `x0201
| `yu
| `named of string (**An encoding given as a name.
		      The list of named encodings depends on the system configuration.*) ]

val name_of_encoding : encoding -> string
(** Return the name of the encoding.

    Example: [Char_encodings.name_of_encoding `ibm860 = "IBM860"]
*)

(**
   {6 Type-safe encodings}
*)

(**
   {7 Types}
*)

type ('a, 'b) t constraint 'b = [< encoding]
(** The type of items of type ['a], encoded using encoding ['b].

    This type, along with its constructor/destructor, is provided
    as a convenience to represent data encoded within a given
    encoding.

    For instance, an input encoded as ASCII may be represented
    as a [(input, [`ascii]) t].
*)

val as_encoded : 'a -> ([< encoding] as 'b) -> ('a, 'b) t
(**
   [as_encoded x enc] returns an element of type [t] used to mark
   that [x] is encoded with encoding [enc].

   Example: [let stdin_lat = Char_encodings.as_encoded stdin `latin1]
*)

val encoded_as : ('a, 'b) t -> 'a
(**
   [encoded_as t] returns the [x] such that [t = as_encoded x enc].

   Example: [encoded_as stdin_lat = stdin]
*)

val encoding_of_t : ('a, 'b) t -> 'b
(**
   Return the encoding of a [t].

   Example: [encoding_of_t stdin_lat = `latin1]
*)

(**
   {7 Transcoders}
*)
val transcode_in : (input,'a) t -> ([< encoding] as 'b) -> (input,'b) t
  (**Convert the contents of an input between encodings.
     
     [transcode_in inp enc] produces a new input, whose
     contents are the same as those of [inp]. However,
     the encoding of the result is specified by [enc].

     {b Note} The resulting [input] may raise [Malformed_code] if the
     encoding specified as ['a] was incorrect.

     Example: [let stdin_ebc = transcode_in stdin_lat `ebcdic_us]
  *)

val transcode_out : (unit output,'a) t -> ([< encoding] as 'c) -> (unit output,'c) t
  (**Convert the contents of an output between encodings.
     
     [transcode_in out enc] produces a new output. Anything
     written to this output should be written with encoding
     [enc] and is translated to the encoding of [out] before
     being written to [out].

     Example: [
     let out_utf8 = Char_encodings.transcode_out (Char_encodings.as_encoded stdout `utf8) `latin1 in
     BatIO.nwrite out_utf8 "«αι» in unicode";
     ]

     This code writes the UTF-8 version of the given latin1 string to stdout.
  *)
