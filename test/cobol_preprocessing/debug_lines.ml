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

(** Tests for debugging lines: with a [D] indicator, or a [>>D] marker
    (floating debugging lines, ISO/IEC 1989:2002), that are not compiled as
    debugging lines are not activated. *)

let show ?(source_format = Cobol_config.(SF SFFixed)) contents =
  Preproc_testing.show_text ~source_format contents;
  let options = Prog_preproc.options ~source_format () in
  let rec read pp =
    match Cobol_preproc.next_chunk pp with
    | text, pp when List.exists (fun w -> Cobol_common.Srcloc.payload w =
                                          Cobol_preproc.Text.Eof) text -> pp
    | _, pp -> read pp
  in
  let pp =
    read @@ Cobol_preproc.preprocessor ~options @@
    Cobol_preproc.Input.string ~filename:"prog.cob" contents
  in
  List.iter begin fun Cobol_preproc.Text.{ debug_loc = (s, e); debug_kind;
                                           debug_contents } ->
    Pretty.out "debug line (%s) %d.%d-%d.%d: %S@\n"
      (match debug_kind with `Indicator -> "indicator" | `Floating -> "floating")
      s.pos_lnum (s.pos_cnum - s.pos_bol) e.pos_lnum (e.pos_cnum - e.pos_bol)
      debug_contents
  end (List.rev (Cobol_preproc.rev_debug_lines pp))

let%expect_test "fixed-format" =
  show {|
       PROCEDURE DIVISION.
      D    DISPLAY "indicator"
      d    DISPLAY "lowercase indicator"
       >>D DISPLAY "floating in area A"
           >>D DISPLAY "floating in area B"
       >>D
       >>DEFINE X 1
           STOP RUN.
|};
  [%expect {|
    PROCEDURE@<prog.cob:2-7|2-16>
    DIVISION@<prog.cob:2-17|2-25>
    .@<prog.cob:2-25|2-26>
    STOP@<prog.cob:9-11|9-15>
    RUN@<prog.cob:9-16|9-19>
    .@<prog.cob:9-19|9-20>
    debug line (indicator) 3.6-3.30: "D    DISPLAY \"indicator\""
    debug line (indicator) 4.6-4.40: "d    DISPLAY \"lowercase indicator\""
    debug line (floating) 5.7-5.39: ">>D DISPLAY \"floating in area A\""
    debug line (floating) 6.11-6.43: ">>D DISPLAY \"floating in area B\""
    debug line (floating) 7.7-7.10: ">>D" |}]

let%expect_test "free-format" =
  show ~source_format:Cobol_config.(SF SFFree) {|
PROCEDURE DIVISION.
>>D DISPLAY "floating"
    >>d DISPLAY "indented, lowercase"
    DISPLAY ">>D in a literal" >>D
    STOP RUN.
|};
  [%expect {|
    PROCEDURE@<prog.cob:2-0|2-9>
    DIVISION@<prog.cob:2-10|2-18>
    .@<prog.cob:2-18|2-19>
    DISPLAY@<prog.cob:5-4|5-11>
    ">>D in a literal"@<prog.cob:5-12|5-30>
    >>D@<prog.cob:5-31|5-34>
    STOP@<prog.cob:6-4|6-8>
    RUN@<prog.cob:6-9|6-12>
    .@<prog.cob:6-12|6-13>
    debug line (floating) 3.0-3.22: ">>D DISPLAY \"floating\""
    debug line (floating) 4.4-4.37: ">>d DISPLAY \"indented, lowercase\"" |}]

let%expect_test "xopen-and-crt-formats" =
  show ~source_format:Cobol_config.(SF SFXOpen) {|
PROCEDURE DIVISION.
D DISPLAY "X/Open"
 >>D DISPLAY "floating"
 STOP RUN.
|};
  show ~source_format:Cobol_config.(SF SFCRT) {|
 PROCEDURE DIVISION.
DDISPLAY "CRT"
 STOP RUN.
|};
  [%expect {|
    PROCEDURE@<prog.cob:2-0|2-9>
    DIVISION@<prog.cob:2-10|2-18>
    .@<prog.cob:2-18|2-19>
    STOP@<prog.cob:5-1|5-5>
    RUN@<prog.cob:5-6|5-9>
    .@<prog.cob:5-9|5-10>
    debug line (indicator) 3.0-3.18: "D DISPLAY \"X/Open\""
    debug line (floating) 4.1-4.23: ">>D DISPLAY \"floating\""
    PROCEDURE@<prog.cob:2-1|2-10>
    DIVISION@<prog.cob:2-11|2-19>
    .@<prog.cob:2-19|2-20>
    STOP@<prog.cob:4-1|4-5>
    RUN@<prog.cob:4-6|4-9>
    .@<prog.cob:4-9|4-10>
    debug line (indicator) 3.0-3.14: "DDISPLAY \"CRT\"" |}]
