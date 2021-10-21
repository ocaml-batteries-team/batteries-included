(*
 * BatGC - Extended GC operations
 * Copyright (C) 1996 Damien Doligez
 *               2008 David Teller, LIFO, Universite d'Orleans
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

open BatPrintf
include Gc

let print_stat c = (* copied from original module *)
  let st = stat () in
  fprintf c "minor_collections: %d\n" st.minor_collections;
  fprintf c "major_collections: %d\n" st.major_collections;
  fprintf c "compactions:       %d\n" st.compactions;
  fprintf c "\n";
  let l1 = String.length (sprintf "%.0f" st.minor_words) in
  fprintf c "minor_words:    %*.0f\n" l1 st.minor_words;
  fprintf c "promoted_words: %*.0f\n" l1 st.promoted_words;
  fprintf c "major_words:    %*.0f\n" l1 st.major_words;
  fprintf c "\n";
  let l2 = String.length (sprintf "%d" st.top_heap_words) in
  fprintf c "top_heap_words: %*d\n" l2 st.top_heap_words;
  fprintf c "heap_words:     %*d\n" l2 st.heap_words;
  fprintf c "live_words:     %*d\n" l2 st.live_words;
  fprintf c "free_words:     %*d\n" l2 st.free_words;
  fprintf c "largest_free:   %*d\n" l2 st.largest_free;
  fprintf c "fragments:      %*d\n" l2 st.fragments;
  fprintf c "\n";
  fprintf c "live_blocks: %d\n" st.live_blocks;
  fprintf c "free_blocks: %d\n" st.free_blocks;
  fprintf c "heap_chunks: %d\n" st.heap_chunks

    (*$T print_stat
      (IO.output_string () |> tap print_stat |> IO.close_out |> String.nsplit ~by:"\n" |> List.length) = 19
    *)
