(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* Escape a raw string into the body of a double-quoted JS string
   literal. Handles backslashes, quotes, and control characters. *)
let escape_js_string s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter begin function
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' ->  Buffer.add_string buf "\\\""
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\x00' .. '\x1f' as c ->
        Printf.bprintf buf "\\u%04x" (Char.code c)
    | c ->
        Buffer.add_char buf c
  end s;
  Buffer.contents buf

(* Neutralize any "</" sequence so a "</script>" (in any case, or
   split across the escaped text) can't prematurely close the
   surrounding <script> block. Replacing "</" with "<\/" is valid
   inside a JS string/regex and invisible to the JS runtime. *)
let escape_closing_tags s =
  let buf = Buffer.create (String.length s + 16) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '<' && !i + 1 < n && s.[!i + 1] = '/' then begin
      Buffer.add_string buf "<\\/";
      i := !i + 2
    end
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let js_string_literal_for_html raw =
  escape_closing_tags @@ escape_js_string raw
