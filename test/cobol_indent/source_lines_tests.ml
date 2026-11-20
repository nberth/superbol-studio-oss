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

(** Tests for the line model of the indenter, built on
    {!Cobol_preproc.fold_source_lines}. *)

module SL = Cobol_indent.Source_lines

let show ?(source_format = Cobol_config.(SF SFFixed)) ?tab_stops contents =
  let options = Prog_preproc.options ~source_format ?tab_stops () in
  let src = SL.scan ~options ~filename:"prog.cob" contents in
  Array.iter begin fun (line: SL.line) ->
    let words =
      List.map (fun SL.{ start_offset; end_offset } ->
          match end_offset with
          | Some e -> Printf.sprintf "%d-%d" start_offset e
          | None -> Printf.sprintf "%d-" start_offset) line.words
    and comments =
      List.map (fun SL.{ comment_offset; comment_kind; comment_text } ->
          Printf.sprintf "%s@%d:%S"
            (match comment_kind with `Line -> "line" | `Floating -> "float")
            comment_offset comment_text) line.comments
    in
    Pretty.out "%d: text@%d%s%s words=[%s] comments=[%s]@\n" line.lnum
      (SL.text_column line.format)
      (if line.continued then " continued" else "")
      (if line.continues then " continues" else "")
      (String.concat " " words) (String.concat " " comments)
  end src.lines

let%expect_test "fixed-format-lines" =
  show {|      * comment line
       DISPLAY "A" *> floating
       DISPLAY "a long literal that is continued on the next line abcd
      -    "efgh"
      D    DISPLAY "debug"
      / page
|};
  [%expect {|
    1: text@7 words=[] comments=[line@6:"* comment line"]
    2: text@7 words=[7-14 15-18] comments=[float@19:"*> floating"]
    3: text@7 continues words=[7-14 15-] comments=[]
    4: text@7 continued words=[] comments=[]
    5: text@7 words=[] comments=[]
    6: text@7 words=[] comments=[line@6:"/ page"]
    7: text@7 words=[] comments=[] |}]

let%expect_test "change-of-source-format" =
  show {|       DISPLAY "FIXED"
       >>SOURCE FORMAT IS FREE
*> free comment
DISPLAY "FREE" *> floating
|};
  [%expect {|
    1: text@7 words=[7-14 15-22] comments=[]
    2: text@7 words=[7-15 16-22 23-25 26-30] comments=[]
    3: text@0 words=[] comments=[float@0:"*> free comment"]
    4: text@0 words=[0-7 8-14] comments=[float@15:"*> floating"]
    5: text@0 words=[] comments=[] |}]

let%expect_test "tabs-and-bom" =
  show "\xEF\xBB\xBF       DISPLAY \"A\"\n\t    DISPLAY \"B\"\n";
  [%expect {|
    1: text@7 words=[10-17 18-21] comments=[]
    2: text@7 words=[5-12 13-16] comments=[]
    3: text@7 words=[] comments=[] |}]
