(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

let of_string s =
  String.split_on_char ',' s |> List.map (fun t -> int_of_string (String.trim t))

let is_valid = function
  | [] -> false
  | l -> List.for_all ((<) 0) l

let default = [8]

let next_stop ?(tab_stops = default) col =
  let rec go pos = function
    | [w] ->
      if pos + w > col then pos + w
      else pos + ((col - pos) / w  + 1) * w
    | w :: rest ->
      let stop = pos + w in
      if stop > col then stop else go stop rest
    | [] -> col + 1
  in
  go 1 tab_stops


(* --- *)

(** Location of a visual column in a line. *)
type column_spot =
  | Char of int    (** byte offset of the character at the visual column *)
  | Tab of { offset: int; start_col: int; next_col: int }
  (** tab at byte [offset] that spans the visual column ([start_col <= col <
      next_col]) *)
  | Short                         (** the line ends before the visual column *)

(** [locate_column ?tab_stops line col] locates the visual column [col]
    (1-based) in [line].  UTF-8 continuation bytes do not count as columns. *)
let locate_column ?tab_stops line col =
  let len = String.length line in
  let rec aux i c =
    if i >= len then Short
    else match line.[i] with
      | '\t' ->
          let next_col = next_stop ?tab_stops c in
          if c <= col && col < next_col
          then Tab { offset = i; start_col = c; next_col }
          else aux (succ i) next_col
      | '\128'..'\191' ->                         (* UTF-8 continuation byte *)
          aux (succ i) c
      | _ when c = col ->
          Char i
      | _ ->
          aux (succ i) (succ c)
  in
  aux 0 1

(** [column_at ?tab_stops line offset] is the visual column (1-based) at which
    the byte at [offset] in [line] starts, or would start if [line] were long
    enough.  UTF-8 continuation bytes do not count as columns. *)
let column_at ?tab_stops line offset =
  let len = String.length line in
  let rec aux i c =
    if i >= offset then c
    else if i >= len then c + offset - i
    else match line.[i] with
      | '\t' -> aux (succ i) (next_stop ?tab_stops c)
      | '\128'..'\191' -> aux (succ i) c
      | _ -> aux (succ i) (succ c)
  in
  aux 0 1
