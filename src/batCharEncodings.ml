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
open BatCamomile

module Encoding = BatCamomile.CharEncoding

exception Malformed_code = Encoding.Malformed_code

(**
   The list if known encodings.

   List taken from Camomile 0.7.1
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

let camomile_of_encoding = function
| `charset_1026 -> Encoding.of_name "1026"
| `charset_1047 -> Encoding.of_name "1047"
| `charset_437 -> Encoding.of_name "437"
| `charset_500 -> Encoding.of_name "500"
| `charset_500v1 -> Encoding.of_name "500V1"
| `charset_850 -> Encoding.of_name "850"
| `charset_851 -> Encoding.of_name "851"
| `charset_852 -> Encoding.of_name "852"
| `charset_855 -> Encoding.of_name "855"
| `charset_856 -> Encoding.of_name "856"
| `charset_857 -> Encoding.of_name "857"
| `charset_860 -> Encoding.of_name "860"
| `charset_861 -> Encoding.of_name "861"
| `charset_862 -> Encoding.of_name "862"
| `charset_863 -> Encoding.of_name "863"
| `charset_865 -> Encoding.of_name "865"
| `charset_866 -> Encoding.of_name "866"
| `charset_866nav -> Encoding.of_name "866NAV"
| `charset_869 -> Encoding.of_name "869"
| `charset_904 -> Encoding.of_name "904"
| `ansi_x3_110_1983 -> Encoding.of_name "ANSI_X3.110-1983"
| `ansi_x3_4_1968 -> Encoding.of_name "ANSI_X3.4-1968"
| `ansi_x3_4_1986 -> Encoding.of_name "ANSI_X3.4-1986"
| `arabic7 -> Encoding.of_name "ARABIC7"
| `arabic -> Encoding.of_name "ARABIC"
| `armscii_8 -> Encoding.of_name "ARMSCII-8"
| `ascii -> Encoding.of_name "ASCII"
| `asmo_449 -> Encoding.of_name "ASMO_449"
| `asmo_708 -> Encoding.of_name "ASMO-708"
| `big5_cp950 -> Encoding.of_name "BIG5-CP950"
| `big5_hkscs -> Encoding.of_name "BIG5-HKSCS"
| `big5hkscs -> Encoding.of_name "BIG5HKSCS"
| `big5 -> Encoding.of_name "BIG5"
| `bs_4730 -> Encoding.of_name "BS_4730"
| `bs_viewdata -> Encoding.of_name "BS_VIEWDATA"
| `ca -> Encoding.of_name "CA"
| `cn -> Encoding.of_name "CN"
| `cp037 -> Encoding.of_name "CP037"
| `cp038 -> Encoding.of_name "CP038"
| `cp10007 -> Encoding.of_name "CP10007"
| `cp1004 -> Encoding.of_name "CP1004"
| `cp1026 -> Encoding.of_name "CP1026"
| `cp1047 -> Encoding.of_name "CP1047"
| `cp1124 -> Encoding.of_name "CP1124"
| `cp1125 -> Encoding.of_name "CP1125"
| `cp1129 -> Encoding.of_name "CP1129"
| `cp1132 -> Encoding.of_name "CP1132"
| `cp1133 -> Encoding.of_name "CP1133"
| `cp1160 -> Encoding.of_name "CP1160"
| `cp1161 -> Encoding.of_name "CP1161"
| `cp1162 -> Encoding.of_name "CP1162"
| `cp1163 -> Encoding.of_name "CP1163"
| `cp1164 -> Encoding.of_name "CP1164"
| `cp1250 -> Encoding.of_name "CP1250"
| `cp1251 -> Encoding.of_name "CP1251"
| `cp1252 -> Encoding.of_name "CP1252"
| `cp1253 -> Encoding.of_name "CP1253"
| `cp1254 -> Encoding.of_name "CP1254"
| `cp1255 -> Encoding.of_name "CP1255"
| `cp1256 -> Encoding.of_name "CP1256"
| `cp1257 -> Encoding.of_name "CP1257"
| `cp1258 -> Encoding.of_name "CP1258"
| `cp273 -> Encoding.of_name "CP273"
| `cp274 -> Encoding.of_name "CP274"
| `cp275 -> Encoding.of_name "CP275"
| `cp278 -> Encoding.of_name "CP278"
| `cp280 -> Encoding.of_name "CP280"
| `cp281 -> Encoding.of_name "CP281"
| `cp284 -> Encoding.of_name "CP284"
| `cp285 -> Encoding.of_name "CP285"
| `cp290 -> Encoding.of_name "CP290"
| `cp297 -> Encoding.of_name "CP297"
| `cp367 -> Encoding.of_name "CP367"
| `cp420 -> Encoding.of_name "CP420"
| `cp423 -> Encoding.of_name "CP423"
| `cp424 -> Encoding.of_name "CP424"
| `cp437 -> Encoding.of_name "CP437"
| `cp500 -> Encoding.of_name "CP500"
| `cp737 -> Encoding.of_name "CP737"
| `cp775 -> Encoding.of_name "CP775"
| `cp819 -> Encoding.of_name "CP819"
| `cp850 -> Encoding.of_name "CP850"
| `cp851 -> Encoding.of_name "CP851"
| `cp852 -> Encoding.of_name "CP852"
| `cp855 -> Encoding.of_name "CP855"
| `cp856 -> Encoding.of_name "CP856"
| `cp857 -> Encoding.of_name "CP857"
| `cp860 -> Encoding.of_name "CP860"
| `cp861 -> Encoding.of_name "CP861"
| `cp862 -> Encoding.of_name "CP862"
| `cp863 -> Encoding.of_name "CP863"
| `cp864 -> Encoding.of_name "CP864"
| `cp865 -> Encoding.of_name "CP865"
| `cp866 -> Encoding.of_name "CP866"
| `cp866nav -> Encoding.of_name "CP866NAV"
| `cp868 -> Encoding.of_name "CP868"
| `cp869 -> Encoding.of_name "CP869"
| `cp870 -> Encoding.of_name "CP870"
| `cp871 -> Encoding.of_name "CP871"
| `cp874 -> Encoding.of_name "CP874"
| `cp875 -> Encoding.of_name "CP875"
| `cp880 -> Encoding.of_name "CP880"
| `cp891 -> Encoding.of_name "CP891"
| `cp903 -> Encoding.of_name "CP903"
| `cp904 -> Encoding.of_name "CP904"
| `cp905 -> Encoding.of_name "CP905"
| `cp918 -> Encoding.of_name "CP918"
| `cp922 -> Encoding.of_name "CP922"
| `cp932 -> Encoding.of_name "CP932"
| `cp949 -> Encoding.of_name "CP949"
| `cp_ar -> Encoding.of_name "CP-AR"
| `cp_gr -> Encoding.of_name "CP-GR"
| `cp_hu -> Encoding.of_name "CP-HU"
| `cp_is -> Encoding.of_name "CP-IS"
| `csa7_1 -> Encoding.of_name "CSA7-1"
| `csa7_2 -> Encoding.of_name "CSA7-2"
| `csa_t500_1983 -> Encoding.of_name "CSA_T500-1983"
| `csa_z243_4_1985_1 -> Encoding.of_name "CSA_Z243.4-1985-1"
| `csa_z243_4_1985_2 -> Encoding.of_name "CSA_Z243.4-1985-2"
| `csa_z243_4_1985_gr -> Encoding.of_name "CSA_Z243.4-1985-GR"
| `csn_369103 -> Encoding.of_name "CSN_369103"
| `cuba -> Encoding.of_name "CUBA"
| `cwi_2 -> Encoding.of_name "CWI-2"
| `cwi -> Encoding.of_name "CWI"
| `cyrillic -> Encoding.of_name "CYRILLIC"
| `dec -> Encoding.of_name "DEC"
| `dec_mcs -> Encoding.of_name "DEC-MCS"
| `de -> Encoding.of_name "DE"
| `din_66003 -> Encoding.of_name "DIN_66003"
| `dk -> Encoding.of_name "DK"
| `ds_2089 -> Encoding.of_name "DS_2089"
| `ds2089 -> Encoding.of_name "DS2089"
| `e13b -> Encoding.of_name "E13B"
| `ebcdic_at_de_a -> Encoding.of_name "EBCDIC-AT-DE-A"
| `ebcdic_at_de -> Encoding.of_name "EBCDIC-AT-DE"
| `ebcdic_be -> Encoding.of_name "EBCDIC-BE"
| `ebcdic_br -> Encoding.of_name "EBCDIC-BR"
| `ebcdic_ca_fr -> Encoding.of_name "EBCDIC-CA-FR"
| `ebcdic_cp_ar1 -> Encoding.of_name "EBCDIC-CP-AR1"
| `ebcdic_cp_ar2 -> Encoding.of_name "EBCDIC-CP-AR2"
| `ebcdic_cp_be -> Encoding.of_name "EBCDIC-CP-BE"
| `ebcdic_cp_ca -> Encoding.of_name "EBCDIC-CP-CA"
| `ebcdic_cp_ch -> Encoding.of_name "EBCDIC-CP-CH"
| `ebcdic_cp_dk -> Encoding.of_name "EBCDIC-CP-DK"
| `ebcdic_cp_es -> Encoding.of_name "EBCDIC-CP-ES"
| `ebcdic_cp_fi -> Encoding.of_name "EBCDIC-CP-FI"
| `ebcdic_cp_fr -> Encoding.of_name "EBCDIC-CP-FR"
| `ebcdic_cp_gb -> Encoding.of_name "EBCDIC-CP-GB"
| `ebcdic_cp_gr -> Encoding.of_name "EBCDIC-CP-GR"
| `ebcdic_cp_he -> Encoding.of_name "EBCDIC-CP-HE"
| `ebcdic_cp_is -> Encoding.of_name "EBCDIC-CP-IS"
| `ebcdic_cp_it -> Encoding.of_name "EBCDIC-CP-IT"
| `ebcdic_cp_nl -> Encoding.of_name "EBCDIC-CP-NL"
| `ebcdic_cp_no -> Encoding.of_name "EBCDIC-CP-NO"
| `ebcdic_cp_roece -> Encoding.of_name "EBCDIC-CP-ROECE"
| `ebcdic_cp_se -> Encoding.of_name "EBCDIC-CP-SE"
| `ebcdic_cp_tr -> Encoding.of_name "EBCDIC-CP-TR"
| `ebcdic_cp_us -> Encoding.of_name "EBCDIC-CP-US"
| `ebcdic_cp_wt -> Encoding.of_name "EBCDIC-CP-WT"
| `ebcdic_cp_yu -> Encoding.of_name "EBCDIC-CP-YU"
| `ebcdic_cyrillic -> Encoding.of_name "EBCDIC-CYRILLIC"
| `ebcdic_dk_no_a -> Encoding.of_name "EBCDIC-DK-NO-A"
| `ebcdic_dk_no -> Encoding.of_name "EBCDIC-DK-NO"
| `ebcdic_es_a -> Encoding.of_name "EBCDIC-ES-A"
| `ebcdic_es -> Encoding.of_name "EBCDIC-ES"
| `ebcdic_es_s -> Encoding.of_name "EBCDIC-ES-S"
| `ebcdic_fi_se_a -> Encoding.of_name "EBCDIC-FI-SE-A"
| `ebcdic_fi_se -> Encoding.of_name "EBCDIC-FI-SE"
| `ebcdic_fr -> Encoding.of_name "EBCDIC-FR"
| `ebcdic_greek -> Encoding.of_name "EBCDIC-GREEK"
| `ebcdic_int1 -> Encoding.of_name "EBCDIC-INT1"
| `ebcdic_int -> Encoding.of_name "EBCDIC-INT"
| `ebcdic_is_friss -> Encoding.of_name "EBCDIC-IS-FRISS"
| `ebcdic_it -> Encoding.of_name "EBCDIC-IT"
| `ebcdic_jp_e -> Encoding.of_name "EBCDIC-JP-E"
| `ebcdic_jp_kana -> Encoding.of_name "EBCDIC-JP-KANA"
| `ebcdic_pt -> Encoding.of_name "EBCDIC-PT"
| `ebcdic_uk -> Encoding.of_name "EBCDIC-UK"
| `ebcdic_us -> Encoding.of_name "EBCDIC-US"
| `ecma_114 -> Encoding.of_name "ECMA-114"
| `ecma_118 -> Encoding.of_name "ECMA-118"
| `ecma_cyrillic -> Encoding.of_name "ECMA-CYRILLIC"
| `elot_928 -> Encoding.of_name "ELOT_928"
| `es2 -> Encoding.of_name "ES2"
| `es -> Encoding.of_name "ES"
| `euc_jisx0213 -> Encoding.of_name "EUC-JISX0213"
| `euc_jp -> Encoding.of_name "EUC-JP"
| `euc_kr -> Encoding.of_name "EUC-KR"
| `euc_tw -> Encoding.of_name "EUC-TW"
| `fi -> Encoding.of_name "FI"
| `friss -> Encoding.of_name "FRISS"
| `fr -> Encoding.of_name "FR"
| `gb18030 -> Encoding.of_name "GB18030"
| `gb_1988_80 -> Encoding.of_name "GB_1988-80"
| `gb2312 -> Encoding.of_name "GB2312"
| `gbk -> Encoding.of_name "GBK"
| `gb -> Encoding.of_name "GB"
| `georgian_academy -> Encoding.of_name "GEORGIAN-ACADEMY"
| `georgian_ps -> Encoding.of_name "GEORGIAN-PS"
| `gost_19768_74 -> Encoding.of_name "GOST_19768-74"
| `greek7 -> Encoding.of_name "GREEK7"
| `greek7_old -> Encoding.of_name "GREEK7-OLD"
| `greek8 -> Encoding.of_name "GREEK8"
| `greek_ccitt -> Encoding.of_name "GREEK-CCITT"
| `greek -> Encoding.of_name "GREEK"
| `hebrew -> Encoding.of_name "HEBREW"
| `hp_roman8 -> Encoding.of_name "HP-ROMAN8"
| `hu -> Encoding.of_name "HU"
| `ibm037 -> Encoding.of_name "IBM037"
| `ibm038 -> Encoding.of_name "IBM038"
| `ibm1004 -> Encoding.of_name "IBM1004"
| `ibm1026 -> Encoding.of_name "IBM1026"
| `ibm1047 -> Encoding.of_name "IBM1047"
| `ibm1124 -> Encoding.of_name "IBM1124"
| `ibm1129 -> Encoding.of_name "IBM1129"
| `ibm1132 -> Encoding.of_name "IBM1132"
| `ibm1133 -> Encoding.of_name "IBM1133"
| `ibm1160 -> Encoding.of_name "IBM1160"
| `ibm1161 -> Encoding.of_name "IBM1161"
| `ibm1162 -> Encoding.of_name "IBM1162"
| `ibm1163 -> Encoding.of_name "IBM1163"
| `ibm1164 -> Encoding.of_name "IBM1164"
| `ibm256 -> Encoding.of_name "IBM256"
| `ibm273 -> Encoding.of_name "IBM273"
| `ibm274 -> Encoding.of_name "IBM274"
| `ibm275 -> Encoding.of_name "IBM275"
| `ibm277 -> Encoding.of_name "IBM277"
| `ibm278 -> Encoding.of_name "IBM278"
| `ibm280 -> Encoding.of_name "IBM280"
| `ibm281 -> Encoding.of_name "IBM281"
| `ibm284 -> Encoding.of_name "IBM284"
| `ibm285 -> Encoding.of_name "IBM285"
| `ibm290 -> Encoding.of_name "IBM290"
| `ibm297 -> Encoding.of_name "IBM297"
| `ibm367 -> Encoding.of_name "IBM367"
| `ibm420 -> Encoding.of_name "IBM420"
| `ibm423 -> Encoding.of_name "IBM423"
| `ibm424 -> Encoding.of_name "IBM424"
| `ibm437 -> Encoding.of_name "IBM437"
| `ibm500 -> Encoding.of_name "IBM500"
| `ibm819 -> Encoding.of_name "IBM819"
| `ibm848 -> Encoding.of_name "IBM848"
| `ibm850 -> Encoding.of_name "IBM850"
| `ibm851 -> Encoding.of_name "IBM851"
| `ibm852 -> Encoding.of_name "IBM852"
| `ibm855 -> Encoding.of_name "IBM855"
| `ibm856 -> Encoding.of_name "IBM856"
| `ibm857 -> Encoding.of_name "IBM857"
| `ibm860 -> Encoding.of_name "IBM860"
| `ibm861 -> Encoding.of_name "IBM861"
| `ibm862 -> Encoding.of_name "IBM862"
| `ibm863 -> Encoding.of_name "IBM863"
| `ibm864 -> Encoding.of_name "IBM864"
| `ibm865 -> Encoding.of_name "IBM865"
| `ibm866 -> Encoding.of_name "IBM866"
| `ibm866nav -> Encoding.of_name "IBM866NAV"
| `ibm868 -> Encoding.of_name "IBM868"
| `ibm869 -> Encoding.of_name "IBM869"
| `ibm870 -> Encoding.of_name "IBM870"
| `ibm871 -> Encoding.of_name "IBM871"
| `ibm874 -> Encoding.of_name "IBM874"
| `ibm875 -> Encoding.of_name "IBM875"
| `ibm880 -> Encoding.of_name "IBM880"
| `ibm891 -> Encoding.of_name "IBM891"
| `ibm903 -> Encoding.of_name "IBM903"
| `ibm904 -> Encoding.of_name "IBM904"
| `ibm905 -> Encoding.of_name "IBM905"
| `ibm918 -> Encoding.of_name "IBM918"
| `ibm922 -> Encoding.of_name "IBM922"
| `iec_p27_1 -> Encoding.of_name "IEC_P27-1"
| `inis_8 -> Encoding.of_name "INIS-8"
| `inis_cyrillic -> Encoding.of_name "INIS-CYRILLIC"
| `inis -> Encoding.of_name "INIS"
| `invariant -> Encoding.of_name "INVARIANT"
| `irv -> Encoding.of_name "IRV"
| `isiri_3342 -> Encoding.of_name "ISIRI-3342"
| `iso_10367_box -> Encoding.of_name "ISO_10367-BOX"
| `iso_2033_1983 -> Encoding.of_name "ISO_2033-1983"
| `iso_5427_1981 -> Encoding.of_name "ISO_5427:1981"
| `iso_5427_ext -> Encoding.of_name "ISO_5427-EXT"
| `iso_5427 -> Encoding.of_name "ISO_5427"
| `iso_5428_1980 -> Encoding.of_name "ISO_5428:1980"
| `iso_5428 -> Encoding.of_name "ISO_5428"
| `iso_646_basic_1983 -> Encoding.of_name "ISO_646.BASIC:1983"
| `iso_646_basic -> Encoding.of_name "ISO_646.BASIC"
| `iso646_ca2 -> Encoding.of_name "ISO646-CA2"
| `iso646_ca -> Encoding.of_name "ISO646-CA"
| `iso646_cn -> Encoding.of_name "ISO646-CN"
| `iso646_cu -> Encoding.of_name "ISO646-CU"
| `iso646_de -> Encoding.of_name "ISO646-DE"
| `iso646_dk -> Encoding.of_name "ISO646-DK"
| `iso646_es2 -> Encoding.of_name "ISO646-ES2"
| `iso646_es -> Encoding.of_name "ISO646-ES"
| `iso646_fi -> Encoding.of_name "ISO646-FI"
| `iso646_fr1 -> Encoding.of_name "ISO646-FR1"
| `iso646_fr -> Encoding.of_name "ISO646-FR"
| `iso646_gb -> Encoding.of_name "ISO646-GB"
| `iso646_hu -> Encoding.of_name "ISO646-HU"
| `iso_646_irv_1983 -> Encoding.of_name "ISO_646.IRV:1983"
| `iso_646_irv_1991 -> Encoding.of_name "ISO_646.IRV:1991"
| `iso_646_irv -> Encoding.of_name "ISO_646.IRV"
| `iso646_it -> Encoding.of_name "ISO646-IT"
| `iso646_jp -> Encoding.of_name "ISO646-JP"
| `iso646_jp_ocr_b -> Encoding.of_name "ISO646-JP-OCR-B"
| `iso646_kr -> Encoding.of_name "ISO646-KR"
| `iso646_no2 -> Encoding.of_name "ISO646-NO2"
| `iso646_no -> Encoding.of_name "ISO646-NO"
| `iso646_pt2 -> Encoding.of_name "ISO646-PT2"
| `iso646_pt -> Encoding.of_name "ISO646-PT"
| `iso646_se2 -> Encoding.of_name "ISO646-SE2"
| `iso646_se -> Encoding.of_name "ISO646-SE"
| `iso646_us -> Encoding.of_name "ISO646-US"
| `iso646_yu -> Encoding.of_name "ISO646-YU"
| `iso_6937_1992 -> Encoding.of_name "ISO_6937:1992"
| `iso_6937_2_1983 -> Encoding.of_name "ISO_6937-2:1983"
| `iso_6937_2_25 -> Encoding.of_name "ISO_6937-2-25"
| `iso_6937_2_add -> Encoding.of_name "ISO_6937-2-ADD"
| `iso_6937 -> Encoding.of_name "ISO_6937"
| `iso6937 -> Encoding.of_name "ISO6937"
| `iso_8859_10_1992 -> Encoding.of_name "ISO_8859-10:1992"
| `iso_8859_10 -> Encoding.of_name "ISO_8859-10"
| `iso_8859_1_1987 -> Encoding.of_name "ISO_8859-1:1987"
| `iso_8859_11 -> Encoding.of_name "ISO-8859-11"
| `iso_8859_13 -> Encoding.of_name "ISO-8859-13"
| `iso_8859_14 -> Encoding.of_name "ISO-8859-14"
| `iso_8859_15 -> Encoding.of_name "ISO-8859-15"
| `iso_8859_16 -> Encoding.of_name "ISO-8859-16"
| `iso_8859_1 -> Encoding.of_name "ISO_8859-1"
| `iso_8859_2_1987 -> Encoding.of_name "ISO_8859-2:1987"
| `iso_8859_2 -> Encoding.of_name "ISO_8859-2"
| `iso_8859_3_1988 -> Encoding.of_name "ISO_8859-3:1988"
| `iso_8859_3 -> Encoding.of_name "ISO_8859-3"
| `iso_8859_4_1988 -> Encoding.of_name "ISO_8859-4:1988"
| `iso_8859_4 -> Encoding.of_name "ISO_8859-4"
| `iso_8859_5_1988 -> Encoding.of_name "ISO_8859-5:1988"
| `iso_8859_5 -> Encoding.of_name "ISO_8859-5"
| `iso_8859_6_1987 -> Encoding.of_name "ISO_8859-6:1987"
| `iso_8859_6 -> Encoding.of_name "ISO_8859-6"
| `iso_8859_7_1987 -> Encoding.of_name "ISO_8859-7:1987"
| `iso_8859_7 -> Encoding.of_name "ISO_8859-7"
| `iso_8859_8_1988 -> Encoding.of_name "ISO_8859-8:1988"
| `iso_8859_8 -> Encoding.of_name "ISO_8859-8"
| `iso_8859_9_1989 -> Encoding.of_name "ISO_8859-9:1989"
| `iso_8859_9 -> Encoding.of_name "ISO_8859-9"
| `iso_8859_supp -> Encoding.of_name "ISO_8859-SUPP"
| `iso_9036 -> Encoding.of_name "ISO_9036"
| `iso_ir_100 -> Encoding.of_name "ISO-IR-100"
| `iso_ir_101 -> Encoding.of_name "ISO-IR-101"
| `iso_ir_102 -> Encoding.of_name "ISO-IR-102"
| `iso_ir_103 -> Encoding.of_name "ISO-IR-103"
| `iso_ir_109 -> Encoding.of_name "ISO-IR-109"
| `iso_ir_10 -> Encoding.of_name "ISO-IR-10"
| `iso_ir_110 -> Encoding.of_name "ISO-IR-110"
| `iso_ir_111 -> Encoding.of_name "ISO-IR-111"
| `iso_ir_11 -> Encoding.of_name "ISO-IR-11"
| `iso_ir_121 -> Encoding.of_name "ISO-IR-121"
| `iso_ir_122 -> Encoding.of_name "ISO-IR-122"
| `iso_ir_123 -> Encoding.of_name "ISO-IR-123"
| `iso_ir_126 -> Encoding.of_name "ISO-IR-126"
| `iso_ir_127 -> Encoding.of_name "ISO-IR-127"
| `iso_ir_128 -> Encoding.of_name "ISO-IR-128"
| `iso_ir_138 -> Encoding.of_name "ISO-IR-138"
| `iso_ir_139 -> Encoding.of_name "ISO-IR-139"
| `iso_ir_141 -> Encoding.of_name "ISO-IR-141"
| `iso_ir_142 -> Encoding.of_name "ISO-IR-142"
| `iso_ir_143 -> Encoding.of_name "ISO-IR-143"
| `iso_ir_144 -> Encoding.of_name "ISO-IR-144"
| `iso_ir_146 -> Encoding.of_name "ISO-IR-146"
| `iso_ir_147 -> Encoding.of_name "ISO-IR-147"
| `iso_ir_148 -> Encoding.of_name "ISO-IR-148"
| `iso_ir_14 -> Encoding.of_name "ISO-IR-14"
| `iso_ir_150 -> Encoding.of_name "ISO-IR-150"
| `iso_ir_151 -> Encoding.of_name "ISO-IR-151"
| `iso_ir_152 -> Encoding.of_name "ISO-IR-152"
| `iso_ir_153 -> Encoding.of_name "ISO-IR-153"
| `iso_ir_154 -> Encoding.of_name "ISO-IR-154"
| `iso_ir_155 -> Encoding.of_name "ISO-IR-155"
| `iso_ir_156 -> Encoding.of_name "ISO-IR-156"
| `iso_ir_157 -> Encoding.of_name "ISO-IR-157"
| `iso_ir_158 -> Encoding.of_name "ISO-IR-158"
| `iso_ir_15 -> Encoding.of_name "ISO-IR-15"
| `iso_ir_166 -> Encoding.of_name "ISO-IR-166"
| `iso_ir_16 -> Encoding.of_name "ISO-IR-16"
| `iso_ir_170 -> Encoding.of_name "ISO-IR-170"
| `iso_ir_179 -> Encoding.of_name "ISO-IR-179"
| `iso_ir_17 -> Encoding.of_name "ISO-IR-17"
| `iso_ir_18 -> Encoding.of_name "ISO-IR-18"
| `iso_ir_197 -> Encoding.of_name "ISO-IR-197"
| `iso_ir_19 -> Encoding.of_name "ISO-IR-19"
| `iso_ir_209 -> Encoding.of_name "ISO-IR-209"
| `iso_ir_21 -> Encoding.of_name "ISO-IR-21"
| `iso_ir_226 -> Encoding.of_name "ISO-IR-226"
| `iso_ir_25 -> Encoding.of_name "ISO-IR-25"
| `iso_ir_27 -> Encoding.of_name "ISO-IR-27"
| `iso_ir_2 -> Encoding.of_name "ISO-IR-2"
| `iso_ir_37 -> Encoding.of_name "ISO-IR-37"
| `iso_ir_47 -> Encoding.of_name "ISO-IR-47"
| `iso_ir_49 -> Encoding.of_name "ISO-IR-49"
| `iso_ir_4 -> Encoding.of_name "ISO-IR-4"
| `iso_ir_50 -> Encoding.of_name "ISO-IR-50"
| `iso_ir_51 -> Encoding.of_name "ISO-IR-51"
| `iso_ir_54 -> Encoding.of_name "ISO-IR-54"
| `iso_ir_55 -> Encoding.of_name "ISO-IR-55"
| `iso_ir_57 -> Encoding.of_name "ISO-IR-57"
| `iso_ir_60 -> Encoding.of_name "ISO-IR-60"
| `iso_ir_61 -> Encoding.of_name "ISO-IR-61"
| `iso_ir_69 -> Encoding.of_name "ISO-IR-69"
| `iso_ir_6 -> Encoding.of_name "ISO-IR-6"
| `iso_ir_70 -> Encoding.of_name "ISO-IR-70"
| `iso_ir_8_1 -> Encoding.of_name "ISO-IR-8-1"
| `iso_ir_84 -> Encoding.of_name "ISO-IR-84"
| `iso_ir_85 -> Encoding.of_name "ISO-IR-85"
| `iso_ir_86 -> Encoding.of_name "ISO-IR-86"
| `iso_ir_88 -> Encoding.of_name "ISO-IR-88"
| `iso_ir_89 -> Encoding.of_name "ISO-IR-89"
| `iso_ir_90 -> Encoding.of_name "ISO-IR-90"
| `iso_ir_9_1 -> Encoding.of_name "ISO-IR-9-1"
| `iso_ir_92 -> Encoding.of_name "ISO-IR-92"
| `iso_ir_98 -> Encoding.of_name "ISO-IR-98"
| `iso_ir_99 -> Encoding.of_name "ISO-IR-99"
| `it -> Encoding.of_name "IT"
| `jis_c6220_1969_ro -> Encoding.of_name "JIS_C6220-1969-RO"
| `jis_c6229_1984_b -> Encoding.of_name "JIS_C6229-1984-B"
| `jis_x0201 -> Encoding.of_name "JIS_X0201"
| `johab -> Encoding.of_name "JOHAB"
| `jp -> Encoding.of_name "JP"
| `jp_ocr_b -> Encoding.of_name "JP-OCR-B"
| `js -> Encoding.of_name "JS"
| `jus_i_b1_002 -> Encoding.of_name "JUS_I.B1.002"
| `jus_i_b1_003_mac -> Encoding.of_name "JUS_I.B1.003-MAC"
| `jus_i_b1_003_serb -> Encoding.of_name "JUS_I.B1.003-SERB"
| `koi_7 -> Encoding.of_name "KOI-7"
| `koi_8 -> Encoding.of_name "KOI-8"
| `koi8_r -> Encoding.of_name "KOI8-R"
| `koi8_t -> Encoding.of_name "KOI8-T"
| `koi8_u -> Encoding.of_name "KOI8-U"
| `ksc5636 -> Encoding.of_name "KSC5636"
| `l10 -> Encoding.of_name "L10"
| `l1 -> Encoding.of_name "L1"
| `l2 -> Encoding.of_name "L2"
| `l3 -> Encoding.of_name "L3"
| `l4 -> Encoding.of_name "L4"
| `l5 -> Encoding.of_name "L5"
| `l6 -> Encoding.of_name "L6"
| `l7 -> Encoding.of_name "L7"
| `l8 -> Encoding.of_name "L8"
| `lap -> Encoding.of_name "LAP"
| `latin10 -> Encoding.of_name "LATIN10"
| `latin1_2_5 -> Encoding.of_name "LATIN1-2-5"
| `latin1 -> Encoding.of_name "LATIN1"
| `latin2 -> Encoding.of_name "LATIN2"
| `latin3 -> Encoding.of_name "LATIN3"
| `latin4 -> Encoding.of_name "LATIN4"
| `latin5 -> Encoding.of_name "LATIN5"
| `latin6 -> Encoding.of_name "LATIN6"
| `latin7 -> Encoding.of_name "LATIN7"
| `latin8 -> Encoding.of_name "LATIN8"
| `latin_greek_1 -> Encoding.of_name "LATIN-GREEK-1"
| `latin_greek -> Encoding.of_name "LATIN-GREEK"
| `latin_lap -> Encoding.of_name "LATIN-LAP"
| `mac_cyrillic -> Encoding.of_name "MAC-CYRILLIC"
| `macedonian -> Encoding.of_name "MACEDONIAN"
| `macintosh -> Encoding.of_name "MACINTOSH"
| `mac_is -> Encoding.of_name "MAC-IS"
| `mac -> Encoding.of_name "MAC"
| `mac_sami -> Encoding.of_name "MAC-SAMI"
| `mac_uk -> Encoding.of_name "MAC-UK"
| `ms_ansi -> Encoding.of_name "MS-ANSI"
| `ms_arab -> Encoding.of_name "MS-ARAB"
| `ms_cyrl -> Encoding.of_name "MS-CYRL"
| `ms_ee -> Encoding.of_name "MS-EE"
| `ms_greek -> Encoding.of_name "MS-GREEK"
| `ms_hebr -> Encoding.of_name "MS-HEBR"
| `ms_turk -> Encoding.of_name "MS-TURK"
| `msz_7795_3 -> Encoding.of_name "MSZ_7795.3"
| `naplps -> Encoding.of_name "NAPLPS"
| `nats_dano -> Encoding.of_name "NATS-DANO"
| `nats_sefi -> Encoding.of_name "NATS-SEFI"
| `nc_nc00_10_81 -> Encoding.of_name "NC_NC00-10:81"
| `nc_nc00_10 -> Encoding.of_name "NC_NC00-10"
| `next -> Encoding.of_name "NEXT"
| `nextstep -> Encoding.of_name "NEXTSTEP"
| `nf_z_62_010_1973 -> Encoding.of_name "NF_Z_62-010_(1973)"
| `nf_z_62_010 -> Encoding.of_name "NF_Z_62-010"
| `no2 -> Encoding.of_name "NO2"
| `no -> Encoding.of_name "NO"
| `ns_4551_1 -> Encoding.of_name "NS_4551-1"
| `ns_4551_2 -> Encoding.of_name "NS_4551-2"
| `os2latin1 -> Encoding.of_name "OS2LATIN1"
| `pt2 -> Encoding.of_name "PT2"
| `pt -> Encoding.of_name "PT"
| `r8 -> Encoding.of_name "R8"
| `ref_encoding -> Encoding.of_name "REF"
| `roman8 -> Encoding.of_name "ROMAN8"
| `ruscii -> Encoding.of_name "RUSCII"
| `sami -> Encoding.of_name "SAMI"
| `se2 -> Encoding.of_name "SE2"
| `se -> Encoding.of_name "SE"
| `sen_850200_b -> Encoding.of_name "SEN_850200_B"
| `sen_850200_c -> Encoding.of_name "SEN_850200_C"
| `serbian -> Encoding.of_name "SERBIAN"
| `shift_jis -> Encoding.of_name "SHIFT_JIS"
| `shift_jisx0213 -> Encoding.of_name "SHIFT_JISX0213"
| `sjis -> Encoding.of_name "SJIS"
| `ss636127 -> Encoding.of_name "SS636127"
| `st_sev_358_88 -> Encoding.of_name "ST_SEV_358-88"
| `t_101_g2 -> Encoding.of_name "T.101-G2"
| `t_61_7bit -> Encoding.of_name "T.61-7BIT"
| `t_61_8bit -> Encoding.of_name "T.61-8BIT"
| `t_61 -> Encoding.of_name "T.61"
| `tcvn5712_1_1993 -> Encoding.of_name "TCVN5712-1:1993"
| `tcvn5712_1 -> Encoding.of_name "TCVN5712-1"
| `tcvn_5712 -> Encoding.of_name "TCVN-5712"
| `tcvn -> Encoding.of_name "TCVN"
| `tis620_0 -> Encoding.of_name "TIS620-0"
| `tis620_2529_1 -> Encoding.of_name "TIS620.2529-1"
| `tis620_2533_0 -> Encoding.of_name "TIS620.2533-0"
| `tis_620 -> Encoding.of_name "TIS-620"
| `tis620 -> Encoding.of_name "TIS620"
| `uk -> Encoding.of_name "UK"
| `us_ascii -> Encoding.ascii
| `us -> Encoding.of_name "US"
| `videotex_suppl -> Encoding.of_name "VIDEOTEX-SUPPL"
| `viscii -> Encoding.of_name "VISCII"
| `winbaltrim -> Encoding.of_name "WINBALTRIM"
| `windows_sami2 -> Encoding.of_name "WINDOWS-SAMI2"
| `win_sami_2 -> Encoding.of_name "WIN-SAMI-2"
| `ws2 -> Encoding.of_name "WS2"
| `x0201 -> Encoding.of_name "X0201"
| `yu -> Encoding.of_name "YU" 
| `named s -> Encoding.of_name s
| `ucs4 -> Encoding.ucs4
| `utf16 -> Encoding.utf16
| `utf16be -> Encoding.utf16be
| `utf16le -> Encoding.utf16le
| `utf32   -> Encoding.utf32
| `utf32be -> Encoding.utf32be
| `utf32le -> Encoding.utf32le
| `utf8    -> Encoding.utf8

let name_of_encoding (e:[< encoding]) : string = Encoding.name_of (camomile_of_encoding e)

type ('a, 'b) t =
{
  content  : 'a(**The actual encoded value*);
  encoding : 'b(**The encoding used*)
} constraint 'b = [< encoding]

let as_encoded x enc =
{
  content = x;
  encoding= enc
}

let encoded_as    t = t.content
let encoding_of_t t = t.encoding

(**Change [encoding] without performing any actual conversion.
   This should only be used when it has been already checked that
   the encoding of [t] and encoding [enc] have the same meaning,
   regardless of their distinct types, e.g. [`named "ASCII"] and
   [`ascii]*)
let unsafe_convert t enc =
{
  (t) with encoding = enc
}

let transcode_in inp enc = (*TODO: Make more efficient*)
  let in_enc  = camomile_of_encoding inp.encoding
  and out_enc = camomile_of_encoding enc in
    if Encoding.name_of in_enc = Encoding.name_of out_enc then 
	unsafe_convert inp enc (*No need to get through objects for something that simple.*)
    else
      let buffer       = BatRefList.empty () (*A list of bytes already read*) in
      let push_next () =
	(*Need to read one char, convert it, decompose the converted string and return the first byte*)
	let c       = read inp.content in
	let encoded = Encoding.recode_string (BatString.of_char c) ~in_enc ~out_enc in
	  BatRefList.copy_enum ~dst:buffer ~src:(BatString.enum encoded)
      in
      let transcoded = 
	wrap_in 
	  ~read: (fun ()    -> if BatRefList.is_empty buffer then push_next (); BatRefList.pop buffer)
	  ~input:(fun s o l -> 
		    BatReturn.label (fun label ->
  		      for i = 0 to l - 1 do
			if BatRefList.is_empty buffer then 
			  try
			    push_next ();
			    String.set s ( o + i ) ( BatRefList.pop buffer )
			  with No_more_input as e -> (*We're done reading.*)
			                             (*Now, we must determine if it's cause to raise an exception.*)
			    if i = 0 then (*The input was empty*) raise e
			    else          (*The input was just shorter than expected*) BatReturn.return label i
		      done;l))
	  ~close:(fun () -> BatRefList.clear buffer)
	  ~underlying:[inp.content]
      in as_encoded transcoded enc
      


let transcode_out out enc = (*TODO: Make more efficient*)
  let in_enc  = camomile_of_encoding out.encoding
  and out_enc = camomile_of_encoding enc in
    if Encoding.name_of in_enc = Encoding.name_of out_enc then 
      unsafe_convert out enc  (*No need to get through objects for something that simple.*)
    else 
      let transcoded = (*We don't use [Encoding.convert_out] as this seems to cause a problem with flushing.*)
      wrap_out
	~write: (fun c     -> nwrite out.content (Encoding.recode_string ~in_enc ~out_enc (BatString.of_char c)))
	~output:(fun s o l -> let converted = Encoding.recode_string ~in_enc ~out_enc (String.sub s o l) in
		   nwrite out.content converted;
		   l)
	~flush: (fun ()    -> flush out.content)
	~close: ignore
	~underlying:[out.content]
      in as_encoded transcoded enc
