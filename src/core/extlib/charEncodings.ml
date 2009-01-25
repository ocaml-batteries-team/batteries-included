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

open IO
open CamomileLibrary
TYPE_CONV_PATH "" (*For Sexplib, Bin-prot...*)

module Encoding = CharEncoding.Configure(CamomileDefaultConfig)

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
| `charset_1026 -> Encoding.of_name "1026.mar"
| `charset_1047 -> Encoding.of_name "1047.mar"
| `charset_437 -> Encoding.of_name "437.mar"
| `charset_500 -> Encoding.of_name "500.mar"
| `charset_500v1 -> Encoding.of_name "500V1.mar"
| `charset_850 -> Encoding.of_name "850.mar"
| `charset_851 -> Encoding.of_name "851.mar"
| `charset_852 -> Encoding.of_name "852.mar"
| `charset_855 -> Encoding.of_name "855.mar"
| `charset_856 -> Encoding.of_name "856.mar"
| `charset_857 -> Encoding.of_name "857.mar"
| `charset_860 -> Encoding.of_name "860.mar"
| `charset_861 -> Encoding.of_name "861.mar"
| `charset_862 -> Encoding.of_name "862.mar"
| `charset_863 -> Encoding.of_name "863.mar"
| `charset_865 -> Encoding.of_name "865.mar"
| `charset_866 -> Encoding.of_name "866.mar"
| `charset_866nav -> Encoding.of_name "866NAV.mar"
| `charset_869 -> Encoding.of_name "869.mar"
| `charset_904 -> Encoding.of_name "904.mar"
| `ansi_x3_110_1983 -> Encoding.of_name "ANSI_X3.110-1983.mar"
| `ansi_x3_4_1968 -> Encoding.of_name "ANSI_X3.4-1968.mar"
| `ansi_x3_4_1986 -> Encoding.of_name "ANSI_X3.4-1986.mar"
| `arabic7 -> Encoding.of_name "ARABIC7.mar"
| `arabic -> Encoding.of_name "ARABIC.mar"
| `armscii_8 -> Encoding.of_name "ARMSCII-8.mar"
| `ascii -> Encoding.of_name "ASCII.mar"
| `asmo_449 -> Encoding.of_name "ASMO_449.mar"
| `asmo_708 -> Encoding.of_name "ASMO-708.mar"
| `big5_cp950 -> Encoding.of_name "BIG5-CP950.mar"
| `big5_hkscs -> Encoding.of_name "BIG5-HKSCS.mar"
| `big5hkscs -> Encoding.of_name "BIG5HKSCS.mar"
| `big5 -> Encoding.of_name "BIG5.mar"
| `bs_4730 -> Encoding.of_name "BS_4730.mar"
| `bs_viewdata -> Encoding.of_name "BS_VIEWDATA.mar"
| `ca -> Encoding.of_name "CA.mar"
| `cn -> Encoding.of_name "CN.mar"
| `cp037 -> Encoding.of_name "CP037.mar"
| `cp038 -> Encoding.of_name "CP038.mar"
| `cp10007 -> Encoding.of_name "CP10007.mar"
| `cp1004 -> Encoding.of_name "CP1004.mar"
| `cp1026 -> Encoding.of_name "CP1026.mar"
| `cp1047 -> Encoding.of_name "CP1047.mar"
| `cp1124 -> Encoding.of_name "CP1124.mar"
| `cp1125 -> Encoding.of_name "CP1125.mar"
| `cp1129 -> Encoding.of_name "CP1129.mar"
| `cp1132 -> Encoding.of_name "CP1132.mar"
| `cp1133 -> Encoding.of_name "CP1133.mar"
| `cp1160 -> Encoding.of_name "CP1160.mar"
| `cp1161 -> Encoding.of_name "CP1161.mar"
| `cp1162 -> Encoding.of_name "CP1162.mar"
| `cp1163 -> Encoding.of_name "CP1163.mar"
| `cp1164 -> Encoding.of_name "CP1164.mar"
| `cp1250 -> Encoding.of_name "CP1250.mar"
| `cp1251 -> Encoding.of_name "CP1251.mar"
| `cp1252 -> Encoding.of_name "CP1252.mar"
| `cp1253 -> Encoding.of_name "CP1253.mar"
| `cp1254 -> Encoding.of_name "CP1254.mar"
| `cp1255 -> Encoding.of_name "CP1255.mar"
| `cp1256 -> Encoding.of_name "CP1256.mar"
| `cp1257 -> Encoding.of_name "CP1257.mar"
| `cp1258 -> Encoding.of_name "CP1258.mar"
| `cp273 -> Encoding.of_name "CP273.mar"
| `cp274 -> Encoding.of_name "CP274.mar"
| `cp275 -> Encoding.of_name "CP275.mar"
| `cp278 -> Encoding.of_name "CP278.mar"
| `cp280 -> Encoding.of_name "CP280.mar"
| `cp281 -> Encoding.of_name "CP281.mar"
| `cp284 -> Encoding.of_name "CP284.mar"
| `cp285 -> Encoding.of_name "CP285.mar"
| `cp290 -> Encoding.of_name "CP290.mar"
| `cp297 -> Encoding.of_name "CP297.mar"
| `cp367 -> Encoding.of_name "CP367.mar"
| `cp420 -> Encoding.of_name "CP420.mar"
| `cp423 -> Encoding.of_name "CP423.mar"
| `cp424 -> Encoding.of_name "CP424.mar"
| `cp437 -> Encoding.of_name "CP437.mar"
| `cp500 -> Encoding.of_name "CP500.mar"
| `cp737 -> Encoding.of_name "CP737.mar"
| `cp775 -> Encoding.of_name "CP775.mar"
| `cp819 -> Encoding.of_name "CP819.mar"
| `cp850 -> Encoding.of_name "CP850.mar"
| `cp851 -> Encoding.of_name "CP851.mar"
| `cp852 -> Encoding.of_name "CP852.mar"
| `cp855 -> Encoding.of_name "CP855.mar"
| `cp856 -> Encoding.of_name "CP856.mar"
| `cp857 -> Encoding.of_name "CP857.mar"
| `cp860 -> Encoding.of_name "CP860.mar"
| `cp861 -> Encoding.of_name "CP861.mar"
| `cp862 -> Encoding.of_name "CP862.mar"
| `cp863 -> Encoding.of_name "CP863.mar"
| `cp864 -> Encoding.of_name "CP864.mar"
| `cp865 -> Encoding.of_name "CP865.mar"
| `cp866 -> Encoding.of_name "CP866.mar"
| `cp866nav -> Encoding.of_name "CP866NAV.mar"
| `cp868 -> Encoding.of_name "CP868.mar"
| `cp869 -> Encoding.of_name "CP869.mar"
| `cp870 -> Encoding.of_name "CP870.mar"
| `cp871 -> Encoding.of_name "CP871.mar"
| `cp874 -> Encoding.of_name "CP874.mar"
| `cp875 -> Encoding.of_name "CP875.mar"
| `cp880 -> Encoding.of_name "CP880.mar"
| `cp891 -> Encoding.of_name "CP891.mar"
| `cp903 -> Encoding.of_name "CP903.mar"
| `cp904 -> Encoding.of_name "CP904.mar"
| `cp905 -> Encoding.of_name "CP905.mar"
| `cp918 -> Encoding.of_name "CP918.mar"
| `cp922 -> Encoding.of_name "CP922.mar"
| `cp932 -> Encoding.of_name "CP932.mar"
| `cp949 -> Encoding.of_name "CP949.mar"
| `cp_ar -> Encoding.of_name "CP-AR.mar"
| `cp_gr -> Encoding.of_name "CP-GR.mar"
| `cp_hu -> Encoding.of_name "CP-HU.mar"
| `cp_is -> Encoding.of_name "CP-IS.mar"
| `csa7_1 -> Encoding.of_name "CSA7-1.mar"
| `csa7_2 -> Encoding.of_name "CSA7-2.mar"
| `csa_t500_1983 -> Encoding.of_name "CSA_T500-1983.mar"
| `csa_z243_4_1985_1 -> Encoding.of_name "CSA_Z243.4-1985-1.mar"
| `csa_z243_4_1985_2 -> Encoding.of_name "CSA_Z243.4-1985-2.mar"
| `csa_z243_4_1985_gr -> Encoding.of_name "CSA_Z243.4-1985-GR.mar"
| `csn_369103 -> Encoding.of_name "CSN_369103.mar"
| `cuba -> Encoding.of_name "CUBA.mar"
| `cwi_2 -> Encoding.of_name "CWI-2.mar"
| `cwi -> Encoding.of_name "CWI.mar"
| `cyrillic -> Encoding.of_name "CYRILLIC.mar"
| `dec -> Encoding.of_name "DEC.mar"
| `dec_mcs -> Encoding.of_name "DEC-MCS.mar"
| `de -> Encoding.of_name "DE.mar"
| `din_66003 -> Encoding.of_name "DIN_66003.mar"
| `dk -> Encoding.of_name "DK.mar"
| `ds_2089 -> Encoding.of_name "DS_2089.mar"
| `ds2089 -> Encoding.of_name "DS2089.mar"
| `e13b -> Encoding.of_name "E13B.mar"
| `ebcdic_at_de_a -> Encoding.of_name "EBCDIC-AT-DE-A.mar"
| `ebcdic_at_de -> Encoding.of_name "EBCDIC-AT-DE.mar"
| `ebcdic_be -> Encoding.of_name "EBCDIC-BE.mar"
| `ebcdic_br -> Encoding.of_name "EBCDIC-BR.mar"
| `ebcdic_ca_fr -> Encoding.of_name "EBCDIC-CA-FR.mar"
| `ebcdic_cp_ar1 -> Encoding.of_name "EBCDIC-CP-AR1.mar"
| `ebcdic_cp_ar2 -> Encoding.of_name "EBCDIC-CP-AR2.mar"
| `ebcdic_cp_be -> Encoding.of_name "EBCDIC-CP-BE.mar"
| `ebcdic_cp_ca -> Encoding.of_name "EBCDIC-CP-CA.mar"
| `ebcdic_cp_ch -> Encoding.of_name "EBCDIC-CP-CH.mar"
| `ebcdic_cp_dk -> Encoding.of_name "EBCDIC-CP-DK.mar"
| `ebcdic_cp_es -> Encoding.of_name "EBCDIC-CP-ES.mar"
| `ebcdic_cp_fi -> Encoding.of_name "EBCDIC-CP-FI.mar"
| `ebcdic_cp_fr -> Encoding.of_name "EBCDIC-CP-FR.mar"
| `ebcdic_cp_gb -> Encoding.of_name "EBCDIC-CP-GB.mar"
| `ebcdic_cp_gr -> Encoding.of_name "EBCDIC-CP-GR.mar"
| `ebcdic_cp_he -> Encoding.of_name "EBCDIC-CP-HE.mar"
| `ebcdic_cp_is -> Encoding.of_name "EBCDIC-CP-IS.mar"
| `ebcdic_cp_it -> Encoding.of_name "EBCDIC-CP-IT.mar"
| `ebcdic_cp_nl -> Encoding.of_name "EBCDIC-CP-NL.mar"
| `ebcdic_cp_no -> Encoding.of_name "EBCDIC-CP-NO.mar"
| `ebcdic_cp_roece -> Encoding.of_name "EBCDIC-CP-ROECE.mar"
| `ebcdic_cp_se -> Encoding.of_name "EBCDIC-CP-SE.mar"
| `ebcdic_cp_tr -> Encoding.of_name "EBCDIC-CP-TR.mar"
| `ebcdic_cp_us -> Encoding.of_name "EBCDIC-CP-US.mar"
| `ebcdic_cp_wt -> Encoding.of_name "EBCDIC-CP-WT.mar"
| `ebcdic_cp_yu -> Encoding.of_name "EBCDIC-CP-YU.mar"
| `ebcdic_cyrillic -> Encoding.of_name "EBCDIC-CYRILLIC.mar"
| `ebcdic_dk_no_a -> Encoding.of_name "EBCDIC-DK-NO-A.mar"
| `ebcdic_dk_no -> Encoding.of_name "EBCDIC-DK-NO.mar"
| `ebcdic_es_a -> Encoding.of_name "EBCDIC-ES-A.mar"
| `ebcdic_es -> Encoding.of_name "EBCDIC-ES.mar"
| `ebcdic_es_s -> Encoding.of_name "EBCDIC-ES-S.mar"
| `ebcdic_fi_se_a -> Encoding.of_name "EBCDIC-FI-SE-A.mar"
| `ebcdic_fi_se -> Encoding.of_name "EBCDIC-FI-SE.mar"
| `ebcdic_fr -> Encoding.of_name "EBCDIC-FR.mar"
| `ebcdic_greek -> Encoding.of_name "EBCDIC-GREEK.mar"
| `ebcdic_int1 -> Encoding.of_name "EBCDIC-INT1.mar"
| `ebcdic_int -> Encoding.of_name "EBCDIC-INT.mar"
| `ebcdic_is_friss -> Encoding.of_name "EBCDIC-IS-FRISS.mar"
| `ebcdic_it -> Encoding.of_name "EBCDIC-IT.mar"
| `ebcdic_jp_e -> Encoding.of_name "EBCDIC-JP-E.mar"
| `ebcdic_jp_kana -> Encoding.of_name "EBCDIC-JP-KANA.mar"
| `ebcdic_pt -> Encoding.of_name "EBCDIC-PT.mar"
| `ebcdic_uk -> Encoding.of_name "EBCDIC-UK.mar"
| `ebcdic_us -> Encoding.of_name "EBCDIC-US.mar"
| `ecma_114 -> Encoding.of_name "ECMA-114.mar"
| `ecma_118 -> Encoding.of_name "ECMA-118.mar"
| `ecma_cyrillic -> Encoding.of_name "ECMA-CYRILLIC.mar"
| `elot_928 -> Encoding.of_name "ELOT_928.mar"
| `es2 -> Encoding.of_name "ES2.mar"
| `es -> Encoding.of_name "ES.mar"
| `euc_jisx0213 -> Encoding.of_name "EUC-JISX0213.mar"
| `euc_jp -> Encoding.of_name "EUC-JP.mar"
| `euc_kr -> Encoding.of_name "EUC-KR.mar"
| `euc_tw -> Encoding.of_name "EUC-TW.mar"
| `fi -> Encoding.of_name "FI.mar"
| `friss -> Encoding.of_name "FRISS.mar"
| `fr -> Encoding.of_name "FR.mar"
| `gb18030 -> Encoding.of_name "GB18030.mar"
| `gb_1988_80 -> Encoding.of_name "GB_1988-80.mar"
| `gb2312 -> Encoding.of_name "GB2312.mar"
| `gbk -> Encoding.of_name "GBK.mar"
| `gb -> Encoding.of_name "GB.mar"
| `georgian_academy -> Encoding.of_name "GEORGIAN-ACADEMY.mar"
| `georgian_ps -> Encoding.of_name "GEORGIAN-PS.mar"
| `gost_19768_74 -> Encoding.of_name "GOST_19768-74.mar"
| `greek7 -> Encoding.of_name "GREEK7.mar"
| `greek7_old -> Encoding.of_name "GREEK7-OLD.mar"
| `greek8 -> Encoding.of_name "GREEK8.mar"
| `greek_ccitt -> Encoding.of_name "GREEK-CCITT.mar"
| `greek -> Encoding.of_name "GREEK.mar"
| `hebrew -> Encoding.of_name "HEBREW.mar"
| `hp_roman8 -> Encoding.of_name "HP-ROMAN8.mar"
| `hu -> Encoding.of_name "HU.mar"
| `ibm037 -> Encoding.of_name "IBM037.mar"
| `ibm038 -> Encoding.of_name "IBM038.mar"
| `ibm1004 -> Encoding.of_name "IBM1004.mar"
| `ibm1026 -> Encoding.of_name "IBM1026.mar"
| `ibm1047 -> Encoding.of_name "IBM1047.mar"
| `ibm1124 -> Encoding.of_name "IBM1124.mar"
| `ibm1129 -> Encoding.of_name "IBM1129.mar"
| `ibm1132 -> Encoding.of_name "IBM1132.mar"
| `ibm1133 -> Encoding.of_name "IBM1133.mar"
| `ibm1160 -> Encoding.of_name "IBM1160.mar"
| `ibm1161 -> Encoding.of_name "IBM1161.mar"
| `ibm1162 -> Encoding.of_name "IBM1162.mar"
| `ibm1163 -> Encoding.of_name "IBM1163.mar"
| `ibm1164 -> Encoding.of_name "IBM1164.mar"
| `ibm256 -> Encoding.of_name "IBM256.mar"
| `ibm273 -> Encoding.of_name "IBM273.mar"
| `ibm274 -> Encoding.of_name "IBM274.mar"
| `ibm275 -> Encoding.of_name "IBM275.mar"
| `ibm277 -> Encoding.of_name "IBM277.mar"
| `ibm278 -> Encoding.of_name "IBM278.mar"
| `ibm280 -> Encoding.of_name "IBM280.mar"
| `ibm281 -> Encoding.of_name "IBM281.mar"
| `ibm284 -> Encoding.of_name "IBM284.mar"
| `ibm285 -> Encoding.of_name "IBM285.mar"
| `ibm290 -> Encoding.of_name "IBM290.mar"
| `ibm297 -> Encoding.of_name "IBM297.mar"
| `ibm367 -> Encoding.of_name "IBM367.mar"
| `ibm420 -> Encoding.of_name "IBM420.mar"
| `ibm423 -> Encoding.of_name "IBM423.mar"
| `ibm424 -> Encoding.of_name "IBM424.mar"
| `ibm437 -> Encoding.of_name "IBM437.mar"
| `ibm500 -> Encoding.of_name "IBM500.mar"
| `ibm819 -> Encoding.of_name "IBM819.mar"
| `ibm848 -> Encoding.of_name "IBM848.mar"
| `ibm850 -> Encoding.of_name "IBM850.mar"
| `ibm851 -> Encoding.of_name "IBM851.mar"
| `ibm852 -> Encoding.of_name "IBM852.mar"
| `ibm855 -> Encoding.of_name "IBM855.mar"
| `ibm856 -> Encoding.of_name "IBM856.mar"
| `ibm857 -> Encoding.of_name "IBM857.mar"
| `ibm860 -> Encoding.of_name "IBM860.mar"
| `ibm861 -> Encoding.of_name "IBM861.mar"
| `ibm862 -> Encoding.of_name "IBM862.mar"
| `ibm863 -> Encoding.of_name "IBM863.mar"
| `ibm864 -> Encoding.of_name "IBM864.mar"
| `ibm865 -> Encoding.of_name "IBM865.mar"
| `ibm866 -> Encoding.of_name "IBM866.mar"
| `ibm866nav -> Encoding.of_name "IBM866NAV.mar"
| `ibm868 -> Encoding.of_name "IBM868.mar"
| `ibm869 -> Encoding.of_name "IBM869.mar"
| `ibm870 -> Encoding.of_name "IBM870.mar"
| `ibm871 -> Encoding.of_name "IBM871.mar"
| `ibm874 -> Encoding.of_name "IBM874.mar"
| `ibm875 -> Encoding.of_name "IBM875.mar"
| `ibm880 -> Encoding.of_name "IBM880.mar"
| `ibm891 -> Encoding.of_name "IBM891.mar"
| `ibm903 -> Encoding.of_name "IBM903.mar"
| `ibm904 -> Encoding.of_name "IBM904.mar"
| `ibm905 -> Encoding.of_name "IBM905.mar"
| `ibm918 -> Encoding.of_name "IBM918.mar"
| `ibm922 -> Encoding.of_name "IBM922.mar"
| `iec_p27_1 -> Encoding.of_name "IEC_P27-1.mar"
| `inis_8 -> Encoding.of_name "INIS-8.mar"
| `inis_cyrillic -> Encoding.of_name "INIS-CYRILLIC.mar"
| `inis -> Encoding.of_name "INIS.mar"
| `invariant -> Encoding.of_name "INVARIANT.mar"
| `irv -> Encoding.of_name "IRV.mar"
| `isiri_3342 -> Encoding.of_name "ISIRI-3342.mar"
| `iso_10367_box -> Encoding.of_name "ISO_10367-BOX.mar"
| `iso_2033_1983 -> Encoding.of_name "ISO_2033-1983.mar"
| `iso_5427_1981 -> Encoding.of_name "ISO_5427:1981.mar"
| `iso_5427_ext -> Encoding.of_name "ISO_5427-EXT.mar"
| `iso_5427 -> Encoding.of_name "ISO_5427.mar"
| `iso_5428_1980 -> Encoding.of_name "ISO_5428:1980.mar"
| `iso_5428 -> Encoding.of_name "ISO_5428.mar"
| `iso_646_basic_1983 -> Encoding.of_name "ISO_646.BASIC:1983.mar"
| `iso_646_basic -> Encoding.of_name "ISO_646.BASIC.mar"
| `iso646_ca2 -> Encoding.of_name "ISO646-CA2.mar"
| `iso646_ca -> Encoding.of_name "ISO646-CA.mar"
| `iso646_cn -> Encoding.of_name "ISO646-CN.mar"
| `iso646_cu -> Encoding.of_name "ISO646-CU.mar"
| `iso646_de -> Encoding.of_name "ISO646-DE.mar"
| `iso646_dk -> Encoding.of_name "ISO646-DK.mar"
| `iso646_es2 -> Encoding.of_name "ISO646-ES2.mar"
| `iso646_es -> Encoding.of_name "ISO646-ES.mar"
| `iso646_fi -> Encoding.of_name "ISO646-FI.mar"
| `iso646_fr1 -> Encoding.of_name "ISO646-FR1.mar"
| `iso646_fr -> Encoding.of_name "ISO646-FR.mar"
| `iso646_gb -> Encoding.of_name "ISO646-GB.mar"
| `iso646_hu -> Encoding.of_name "ISO646-HU.mar"
| `iso_646_irv_1983 -> Encoding.of_name "ISO_646.IRV:1983.mar"
| `iso_646_irv_1991 -> Encoding.of_name "ISO_646.IRV:1991.mar"
| `iso_646_irv -> Encoding.of_name "ISO_646.IRV.mar"
| `iso646_it -> Encoding.of_name "ISO646-IT.mar"
| `iso646_jp -> Encoding.of_name "ISO646-JP.mar"
| `iso646_jp_ocr_b -> Encoding.of_name "ISO646-JP-OCR-B.mar"
| `iso646_kr -> Encoding.of_name "ISO646-KR.mar"
| `iso646_no2 -> Encoding.of_name "ISO646-NO2.mar"
| `iso646_no -> Encoding.of_name "ISO646-NO.mar"
| `iso646_pt2 -> Encoding.of_name "ISO646-PT2.mar"
| `iso646_pt -> Encoding.of_name "ISO646-PT.mar"
| `iso646_se2 -> Encoding.of_name "ISO646-SE2.mar"
| `iso646_se -> Encoding.of_name "ISO646-SE.mar"
| `iso646_us -> Encoding.of_name "ISO646-US.mar"
| `iso646_yu -> Encoding.of_name "ISO646-YU.mar"
| `iso_6937_1992 -> Encoding.of_name "ISO_6937:1992.mar"
| `iso_6937_2_1983 -> Encoding.of_name "ISO_6937-2:1983.mar"
| `iso_6937_2_25 -> Encoding.of_name "ISO_6937-2-25.mar"
| `iso_6937_2_add -> Encoding.of_name "ISO_6937-2-ADD.mar"
| `iso_6937 -> Encoding.of_name "ISO_6937.mar"
| `iso6937 -> Encoding.of_name "ISO6937.mar"
| `iso_8859_10_1992 -> Encoding.of_name "ISO_8859-10:1992.mar"
| `iso_8859_10 -> Encoding.of_name "ISO_8859-10.mar"
| `iso_8859_1_1987 -> Encoding.of_name "ISO_8859-1:1987.mar"
| `iso_8859_11 -> Encoding.of_name "ISO-8859-11.mar"
| `iso_8859_13 -> Encoding.of_name "ISO-8859-13.mar"
| `iso_8859_14 -> Encoding.of_name "ISO-8859-14.mar"
| `iso_8859_15 -> Encoding.of_name "ISO-8859-15.mar"
| `iso_8859_16 -> Encoding.of_name "ISO-8859-16.mar"
| `iso_8859_1 -> Encoding.of_name "ISO_8859-1.mar"
| `iso_8859_2_1987 -> Encoding.of_name "ISO_8859-2:1987.mar"
| `iso_8859_2 -> Encoding.of_name "ISO_8859-2.mar"
| `iso_8859_3_1988 -> Encoding.of_name "ISO_8859-3:1988.mar"
| `iso_8859_3 -> Encoding.of_name "ISO_8859-3.mar"
| `iso_8859_4_1988 -> Encoding.of_name "ISO_8859-4:1988.mar"
| `iso_8859_4 -> Encoding.of_name "ISO_8859-4.mar"
| `iso_8859_5_1988 -> Encoding.of_name "ISO_8859-5:1988.mar"
| `iso_8859_5 -> Encoding.of_name "ISO_8859-5.mar"
| `iso_8859_6_1987 -> Encoding.of_name "ISO_8859-6:1987.mar"
| `iso_8859_6 -> Encoding.of_name "ISO_8859-6.mar"
| `iso_8859_7_1987 -> Encoding.of_name "ISO_8859-7:1987.mar"
| `iso_8859_7 -> Encoding.of_name "ISO_8859-7.mar"
| `iso_8859_8_1988 -> Encoding.of_name "ISO_8859-8:1988.mar"
| `iso_8859_8 -> Encoding.of_name "ISO_8859-8.mar"
| `iso_8859_9_1989 -> Encoding.of_name "ISO_8859-9:1989.mar"
| `iso_8859_9 -> Encoding.of_name "ISO_8859-9.mar"
| `iso_8859_supp -> Encoding.of_name "ISO_8859-SUPP.mar"
| `iso_9036 -> Encoding.of_name "ISO_9036.mar"
| `iso_ir_100 -> Encoding.of_name "ISO-IR-100.mar"
| `iso_ir_101 -> Encoding.of_name "ISO-IR-101.mar"
| `iso_ir_102 -> Encoding.of_name "ISO-IR-102.mar"
| `iso_ir_103 -> Encoding.of_name "ISO-IR-103.mar"
| `iso_ir_109 -> Encoding.of_name "ISO-IR-109.mar"
| `iso_ir_10 -> Encoding.of_name "ISO-IR-10.mar"
| `iso_ir_110 -> Encoding.of_name "ISO-IR-110.mar"
| `iso_ir_111 -> Encoding.of_name "ISO-IR-111.mar"
| `iso_ir_11 -> Encoding.of_name "ISO-IR-11.mar"
| `iso_ir_121 -> Encoding.of_name "ISO-IR-121.mar"
| `iso_ir_122 -> Encoding.of_name "ISO-IR-122.mar"
| `iso_ir_123 -> Encoding.of_name "ISO-IR-123.mar"
| `iso_ir_126 -> Encoding.of_name "ISO-IR-126.mar"
| `iso_ir_127 -> Encoding.of_name "ISO-IR-127.mar"
| `iso_ir_128 -> Encoding.of_name "ISO-IR-128.mar"
| `iso_ir_138 -> Encoding.of_name "ISO-IR-138.mar"
| `iso_ir_139 -> Encoding.of_name "ISO-IR-139.mar"
| `iso_ir_141 -> Encoding.of_name "ISO-IR-141.mar"
| `iso_ir_142 -> Encoding.of_name "ISO-IR-142.mar"
| `iso_ir_143 -> Encoding.of_name "ISO-IR-143.mar"
| `iso_ir_144 -> Encoding.of_name "ISO-IR-144.mar"
| `iso_ir_146 -> Encoding.of_name "ISO-IR-146.mar"
| `iso_ir_147 -> Encoding.of_name "ISO-IR-147.mar"
| `iso_ir_148 -> Encoding.of_name "ISO-IR-148.mar"
| `iso_ir_14 -> Encoding.of_name "ISO-IR-14.mar"
| `iso_ir_150 -> Encoding.of_name "ISO-IR-150.mar"
| `iso_ir_151 -> Encoding.of_name "ISO-IR-151.mar"
| `iso_ir_152 -> Encoding.of_name "ISO-IR-152.mar"
| `iso_ir_153 -> Encoding.of_name "ISO-IR-153.mar"
| `iso_ir_154 -> Encoding.of_name "ISO-IR-154.mar"
| `iso_ir_155 -> Encoding.of_name "ISO-IR-155.mar"
| `iso_ir_156 -> Encoding.of_name "ISO-IR-156.mar"
| `iso_ir_157 -> Encoding.of_name "ISO-IR-157.mar"
| `iso_ir_158 -> Encoding.of_name "ISO-IR-158.mar"
| `iso_ir_15 -> Encoding.of_name "ISO-IR-15.mar"
| `iso_ir_166 -> Encoding.of_name "ISO-IR-166.mar"
| `iso_ir_16 -> Encoding.of_name "ISO-IR-16.mar"
| `iso_ir_170 -> Encoding.of_name "ISO-IR-170.mar"
| `iso_ir_179 -> Encoding.of_name "ISO-IR-179.mar"
| `iso_ir_17 -> Encoding.of_name "ISO-IR-17.mar"
| `iso_ir_18 -> Encoding.of_name "ISO-IR-18.mar"
| `iso_ir_197 -> Encoding.of_name "ISO-IR-197.mar"
| `iso_ir_19 -> Encoding.of_name "ISO-IR-19.mar"
| `iso_ir_209 -> Encoding.of_name "ISO-IR-209.mar"
| `iso_ir_21 -> Encoding.of_name "ISO-IR-21.mar"
| `iso_ir_226 -> Encoding.of_name "ISO-IR-226.mar"
| `iso_ir_25 -> Encoding.of_name "ISO-IR-25.mar"
| `iso_ir_27 -> Encoding.of_name "ISO-IR-27.mar"
| `iso_ir_2 -> Encoding.of_name "ISO-IR-2.mar"
| `iso_ir_37 -> Encoding.of_name "ISO-IR-37.mar"
| `iso_ir_47 -> Encoding.of_name "ISO-IR-47.mar"
| `iso_ir_49 -> Encoding.of_name "ISO-IR-49.mar"
| `iso_ir_4 -> Encoding.of_name "ISO-IR-4.mar"
| `iso_ir_50 -> Encoding.of_name "ISO-IR-50.mar"
| `iso_ir_51 -> Encoding.of_name "ISO-IR-51.mar"
| `iso_ir_54 -> Encoding.of_name "ISO-IR-54.mar"
| `iso_ir_55 -> Encoding.of_name "ISO-IR-55.mar"
| `iso_ir_57 -> Encoding.of_name "ISO-IR-57.mar"
| `iso_ir_60 -> Encoding.of_name "ISO-IR-60.mar"
| `iso_ir_61 -> Encoding.of_name "ISO-IR-61.mar"
| `iso_ir_69 -> Encoding.of_name "ISO-IR-69.mar"
| `iso_ir_6 -> Encoding.of_name "ISO-IR-6.mar"
| `iso_ir_70 -> Encoding.of_name "ISO-IR-70.mar"
| `iso_ir_8_1 -> Encoding.of_name "ISO-IR-8-1.mar"
| `iso_ir_84 -> Encoding.of_name "ISO-IR-84.mar"
| `iso_ir_85 -> Encoding.of_name "ISO-IR-85.mar"
| `iso_ir_86 -> Encoding.of_name "ISO-IR-86.mar"
| `iso_ir_88 -> Encoding.of_name "ISO-IR-88.mar"
| `iso_ir_89 -> Encoding.of_name "ISO-IR-89.mar"
| `iso_ir_90 -> Encoding.of_name "ISO-IR-90.mar"
| `iso_ir_9_1 -> Encoding.of_name "ISO-IR-9-1.mar"
| `iso_ir_92 -> Encoding.of_name "ISO-IR-92.mar"
| `iso_ir_98 -> Encoding.of_name "ISO-IR-98.mar"
| `iso_ir_99 -> Encoding.of_name "ISO-IR-99.mar"
| `it -> Encoding.of_name "IT.mar"
| `jis_c6220_1969_ro -> Encoding.of_name "JIS_C6220-1969-RO.mar"
| `jis_c6229_1984_b -> Encoding.of_name "JIS_C6229-1984-B.mar"
| `jis_x0201 -> Encoding.of_name "JIS_X0201.mar"
| `johab -> Encoding.of_name "JOHAB.mar"
| `jp -> Encoding.of_name "JP.mar"
| `jp_ocr_b -> Encoding.of_name "JP-OCR-B.mar"
| `js -> Encoding.of_name "JS.mar"
| `jus_i_b1_002 -> Encoding.of_name "JUS_I.B1.002.mar"
| `jus_i_b1_003_mac -> Encoding.of_name "JUS_I.B1.003-MAC.mar"
| `jus_i_b1_003_serb -> Encoding.of_name "JUS_I.B1.003-SERB.mar"
| `koi_7 -> Encoding.of_name "KOI-7.mar"
| `koi_8 -> Encoding.of_name "KOI-8.mar"
| `koi8_r -> Encoding.of_name "KOI8-R.mar"
| `koi8_t -> Encoding.of_name "KOI8-T.mar"
| `koi8_u -> Encoding.of_name "KOI8-U.mar"
| `ksc5636 -> Encoding.of_name "KSC5636.mar"
| `l10 -> Encoding.of_name "L10.mar"
| `l1 -> Encoding.of_name "L1.mar"
| `l2 -> Encoding.of_name "L2.mar"
| `l3 -> Encoding.of_name "L3.mar"
| `l4 -> Encoding.of_name "L4.mar"
| `l5 -> Encoding.of_name "L5.mar"
| `l6 -> Encoding.of_name "L6.mar"
| `l7 -> Encoding.of_name "L7.mar"
| `l8 -> Encoding.of_name "L8.mar"
| `lap -> Encoding.of_name "LAP.mar"
| `latin10 -> Encoding.of_name "LATIN10.mar"
| `latin1_2_5 -> Encoding.of_name "LATIN1-2-5.mar"
| `latin1 -> Encoding.of_name "LATIN1.mar"
| `latin2 -> Encoding.of_name "LATIN2.mar"
| `latin3 -> Encoding.of_name "LATIN3.mar"
| `latin4 -> Encoding.of_name "LATIN4.mar"
| `latin5 -> Encoding.of_name "LATIN5.mar"
| `latin6 -> Encoding.of_name "LATIN6.mar"
| `latin7 -> Encoding.of_name "LATIN7.mar"
| `latin8 -> Encoding.of_name "LATIN8.mar"
| `latin_greek_1 -> Encoding.of_name "LATIN-GREEK-1.mar"
| `latin_greek -> Encoding.of_name "LATIN-GREEK.mar"
| `latin_lap -> Encoding.of_name "LATIN-LAP.mar"
| `mac_cyrillic -> Encoding.of_name "MAC-CYRILLIC.mar"
| `macedonian -> Encoding.of_name "MACEDONIAN.mar"
| `macintosh -> Encoding.of_name "MACINTOSH.mar"
| `mac_is -> Encoding.of_name "MAC-IS.mar"
| `mac -> Encoding.of_name "MAC.mar"
| `mac_sami -> Encoding.of_name "MAC-SAMI.mar"
| `mac_uk -> Encoding.of_name "MAC-UK.mar"
| `ms_ansi -> Encoding.of_name "MS-ANSI.mar"
| `ms_arab -> Encoding.of_name "MS-ARAB.mar"
| `ms_cyrl -> Encoding.of_name "MS-CYRL.mar"
| `ms_ee -> Encoding.of_name "MS-EE.mar"
| `ms_greek -> Encoding.of_name "MS-GREEK.mar"
| `ms_hebr -> Encoding.of_name "MS-HEBR.mar"
| `ms_turk -> Encoding.of_name "MS-TURK.mar"
| `msz_7795_3 -> Encoding.of_name "MSZ_7795.3.mar"
| `naplps -> Encoding.of_name "NAPLPS.mar"
| `nats_dano -> Encoding.of_name "NATS-DANO.mar"
| `nats_sefi -> Encoding.of_name "NATS-SEFI.mar"
| `nc_nc00_10_81 -> Encoding.of_name "NC_NC00-10:81.mar"
| `nc_nc00_10 -> Encoding.of_name "NC_NC00-10.mar"
| `next -> Encoding.of_name "NEXT.mar"
| `nextstep -> Encoding.of_name "NEXTSTEP.mar"
| `nf_z_62_010_1973 -> Encoding.of_name "NF_Z_62-010_(1973).mar"
| `nf_z_62_010 -> Encoding.of_name "NF_Z_62-010.mar"
| `no2 -> Encoding.of_name "NO2.mar"
| `no -> Encoding.of_name "NO.mar"
| `ns_4551_1 -> Encoding.of_name "NS_4551-1.mar"
| `ns_4551_2 -> Encoding.of_name "NS_4551-2.mar"
| `os2latin1 -> Encoding.of_name "OS2LATIN1.mar"
| `pt2 -> Encoding.of_name "PT2.mar"
| `pt -> Encoding.of_name "PT.mar"
| `r8 -> Encoding.of_name "R8.mar"
| `ref_encoding -> Encoding.of_name "REF.mar"
| `roman8 -> Encoding.of_name "ROMAN8.mar"
| `ruscii -> Encoding.of_name "RUSCII.mar"
| `sami -> Encoding.of_name "SAMI.mar"
| `se2 -> Encoding.of_name "SE2.mar"
| `se -> Encoding.of_name "SE.mar"
| `sen_850200_b -> Encoding.of_name "SEN_850200_B.mar"
| `sen_850200_c -> Encoding.of_name "SEN_850200_C.mar"
| `serbian -> Encoding.of_name "SERBIAN.mar"
| `shift_jis -> Encoding.of_name "SHIFT_JIS.mar"
| `shift_jisx0213 -> Encoding.of_name "SHIFT_JISX0213.mar"
| `sjis -> Encoding.of_name "SJIS.mar"
| `ss636127 -> Encoding.of_name "SS636127.mar"
| `st_sev_358_88 -> Encoding.of_name "ST_SEV_358-88.mar"
| `t_101_g2 -> Encoding.of_name "T.101-G2.mar"
| `t_61_7bit -> Encoding.of_name "T.61-7BIT.mar"
| `t_61_8bit -> Encoding.of_name "T.61-8BIT.mar"
| `t_61 -> Encoding.of_name "T.61.mar"
| `tcvn5712_1_1993 -> Encoding.of_name "TCVN5712-1:1993.mar"
| `tcvn5712_1 -> Encoding.of_name "TCVN5712-1.mar"
| `tcvn_5712 -> Encoding.of_name "TCVN-5712.mar"
| `tcvn -> Encoding.of_name "TCVN.mar"
| `tis620_0 -> Encoding.of_name "TIS620-0.mar"
| `tis620_2529_1 -> Encoding.of_name "TIS620.2529-1.mar"
| `tis620_2533_0 -> Encoding.of_name "TIS620.2533-0.mar"
| `tis_620 -> Encoding.of_name "TIS-620.mar"
| `tis620 -> Encoding.of_name "TIS620.mar"
| `uk -> Encoding.of_name "UK.mar"
| `us_ascii -> Encoding.ascii
| `us -> Encoding.of_name "US.mar"
| `videotex_suppl -> Encoding.of_name "VIDEOTEX-SUPPL.mar"
| `viscii -> Encoding.of_name "VISCII.mar"
| `winbaltrim -> Encoding.of_name "WINBALTRIM.mar"
| `windows_sami2 -> Encoding.of_name "WINDOWS-SAMI2.mar"
| `win_sami_2 -> Encoding.of_name "WIN-SAMI-2.mar"
| `ws2 -> Encoding.of_name "WS2.mar"
| `x0201 -> Encoding.of_name "X0201.mar"
| `yu -> Encoding.of_name "YU.mar" 
| `named s -> Encoding.of_name s
| `ucs4 -> Encoding.ucs4
| `utf16 -> Encoding.utf16
| `utf16be -> Encoding.utf16be
| `utf16le -> Encoding.utf16le
| `utf32   -> Encoding.utf32
| `utf32be -> Encoding.utf32be
| `utf32le -> Encoding.utf32le
| `utf8    -> Encoding.utf8

type 'a encoded_in = 
{
  encoding_in : 'a;
  input       : input
} constraint 'a = [< encoding]

type ('a,'b) encoded_out =
{
  encoding_out : 'a;
  output       : 'b output
} constraint 'a = [< encoding]

let encoded_of_in inp enc =
{
  input       = inp;
  encoding_in = enc
}

let in_of_encoded e = e.input

let encoded_of_out out enc =
{
  output      = out;
  encoding_out= enc
}

let out_of_encoded e = e.output

external unsafe_convert_in  : 'a encoded_in  -> 'b encoded_in  = "%identity%"
external unsafe_convert_out : ('a,'c) encoded_out -> ('b,'c) encoded_out = "%identity%"

let transcode_in (inp:[<encoding] encoded_in) (enc:[<encoding] as 'a) : 'a encoded_in =
  let in_enc  = camomile_of_encoding inp.encoding_in
  and out_enc = camomile_of_encoding enc in
    if in_enc = out_enc then unsafe_convert_in inp (*No need to get through objects for something that simple.*)
    else encoded_of_in (from_in_channel (new Encoding.convert_input ~in_enc ~out_enc (new IO.in_channel inp.input)))
      enc

let transcode_out out enc =
  let in_enc  = camomile_of_encoding out.encoding_out
  and out_enc = camomile_of_encoding enc in
    if in_enc = out_enc then unsafe_convert_out out (*No need to get through objects for something that simple.*)
    else encoded_of_out (from_out_channel (new Encoding.convert_output ~in_enc ~out_enc (new IO.out_channel out.output)))
      enc


(*
(**
   {7 Transcoders}
*)
val transcode_in : 'a encoded_in -> ([< encoding] as 'b) -> 'b encoded_in
  (**Convert the contents of an input between encodings.
     
     [transcode_in inp enc] produces a new input, whose
     contents are the same as those of [inp]. However,
     the encoding of the result is specified by [enc].
  *)

val transcode_out : ('a,'b) encoded_out -> ([< encoding] as 'c) -> ('c,'b) encoded_in
  (**Convert the contents of an output between encodings.
     
     [transcode_in out enc] produces a new output. Anything
     written to this output should be written with encoding
     [enc] and is translated to the encoding of [out] before
     being written to [out].
  *)
*)
