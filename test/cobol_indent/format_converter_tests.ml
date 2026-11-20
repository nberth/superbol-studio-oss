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

(** Tests for the conversion between source formats. *)

module SL = Cobol_indent.Source_lines

let words ~source_format contents =
  let options = Prog_preproc.options ~source_format () in
  Cobol_preproc.Input.string ~filename:"prog.cob" contents |>
  Cobol_preproc.text_of_input ~options |>
  Cobol_preproc.Outputs.show_n_forget ~ppf:Fmt.stdout |>
  List.filter_map begin fun w ->
    match Cobol_common.Srcloc.payload w with
    | Cobol_preproc.Text.Eof -> None
    | w -> Some (Pretty.to_string "%a" Cobol_preproc.Text.pp_word w)
  end

(* Joins alphanumeric literals concatenated with "&" (with the same
   quote). *)
let rec join_concatenations = function
  | a :: "&" :: b :: rest
    when String.length a >= 2 && String.length b >= 2 &&
         (a.[0] = '"' || a.[0] = '\'') && b.[0] = a.[0] ->
      join_concatenations
        ((String.sub a 0 (String.length a - 1) ^
          String.sub b 1 (String.length b - 1)) :: rest)
  | w :: rest -> w :: join_concatenations rest
  | [] -> []

(* Shows the conversion of [contents] into [target], with a [|] after the
   last column of the text area of lines that reach it, and checks that the
   converted text gives the same words as the original. *)
let show ?(source_format = Cobol_config.(SF SFFree)) ?(target = "FIXED")
    ?right_margin ?keep_sequence_numbers ?add_source_format_directive
    contents =
  let options = Prog_preproc.options ~source_format () in
  let src = SL.scan ~options ~filename:"prog.cob" contents in
  let target_format = Cobol_config.Options.format_of_string target in
  let last_col = match target_format with
    | SF f ->
        (match Cobol_preproc.Src_format.from_config f with
         | SF (_, FixedWidth { cut_at_col; _ }) -> cut_at_col
         | SF (_, FreePaging) -> max_int)
    | Auto -> assert false
  in
  let last_col = match right_margin with
    | Some c -> min c last_col
    | None -> last_col
  in
  let target_name = target in
  let fixed, warnings =
    let open Cobol_indent.Format_converter in
    match
      Result.bind (target_of_name target_name) begin fun t ->
        match right_margin with
        | Some c -> with_right_margin c t
        | None -> Ok t
      end
    with
    | Error msg -> raise (Invalid_argument msg)
    | Ok target ->
        Cobol_indent.Format_converter.convert ~target src ~eol:"\n"
          ?keep_sequence_numbers ?add_source_format_directive
  in
  List.iter begin fun l ->
    if String.length l > last_col then Pretty.out "%s<- too long@\n" l
    else if String.length l = last_col then Pretty.out "%s|@\n" l
    else Pretty.out "%s@\n" l
  end (String.split_on_char '\n' fixed);
  List.iter (fun Cobol_indent.Format_converter.{ line; message } ->
      Pretty.out "warning: line %d: %s@\n" line message) warnings;
  let orig = join_concatenations (words ~source_format contents)
  and conv = join_concatenations (words ~source_format:target_format fixed) in
  if orig = conv
  then Pretty.out "same words@\n"
  else Pretty.out "DIFFERENT words:@\n  %s@\n  %s@\n"
      (String.concat " " orig) (String.concat " " conv)

let%expect_test "free-to-fixed" =
  show {|IDENTIFICATION DIVISION.
PROGRAM-ID. PROG.
*> a comment line
*> a very long comment line that does not fit within the text area of the fixed format
PROCEDURE DIVISION.
MAIN.
    DISPLAY "HELLO"   *> a floating comment
    IF A = B
       MOVE 1 TO X
    END-IF
>>D DISPLAY "DEBUG"
    STOP RUN.
|};
  [%expect {|
           IDENTIFICATION DIVISION.
           PROGRAM-ID. PROG.
          * a comment line
          * a very long comment line that does not fit within the text area
          * of the fixed format
           PROCEDURE DIVISION.
           MAIN.
          * a floating comment
               DISPLAY "HELLO"
               IF A = B
                  MOVE 1 TO X
               END-IF
          D    DISPLAY "DEBUG"
               STOP RUN.

    prog.cob:11.0-11.19:
    >> Error: Invalid >>D compiler directive

    same words |}]

let%expect_test "long-lines" =
  show {|PROCEDURE DIVISION.
                       CALL "SOMEPROGRAM" USING BY REFERENCE A-VERY-LONG-NAME B-NAME
    MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2 C-VERY-LONG-DATA-NAME-3 D-VERY-LONG-DATA-NAME-4
|};
  [%expect {|
           PROCEDURE DIVISION.
               CALL "SOMEPROGRAM" USING BY REFERENCE A-VERY-LONG-NAME B-NAME|
               MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2
                  C-VERY-LONG-DATA-NAME-3 D-VERY-LONG-DATA-NAME-4

    same words |}]

let%expect_test "long-literals" =
  show {|PROCEDURE DIVISION.
    DISPLAY "This is a very long literal that will not fit within the text area of a single line in fixed format, and that is even longer than two lines of fixed format text area" UPON CONSOLE
    MOVE 'quotes '' inside a long literal ''''''''''''''''''''''''''''''''''''''''''''' TO X
    MOVE X"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF01" TO Y
|};
  [%expect {|
           PROCEDURE DIVISION.
               DISPLAY "This is a very long literal that will not fit within|
          -    " the text area of a single line in fixed format, and that is|
          -    " even longer than two lines of fixed format text area" UPON
                  CONSOLE
               MOVE 'quotes '' inside a long literal '''''''''''''''''''''''|
          -    ''''''''''''''''''''''' TO X
               MOVE X"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF012345|
          -    "6789ABCDEF01" TO Y

    warning: line 3: literal continued right after a quote
    same words |}]

let%expect_test "fixed-to-fixed" =
  show ~source_format:Cobol_config.(SF SFFixed) ~keep_sequence_numbers:true
{|000100 IDENTIFICATION DIVISION.                                         PROG0001
000200 PROGRAM-ID. PROG.                                                PROG0002
000300* comment line                                                    PROG0003
000400 PROCEDURE DIVISION.                                              PROG0004
000500     DISPLAY "a literal that is continued on the next line without
000600-    " a gap" *> floating
000700D    DISPLAY "DEBUG"
000800     STOP RUN.
|};
  [%expect {|
    000100 IDENTIFICATION DIVISION.
    000200 PROGRAM-ID. PROG.
    000300* comment line
    000400 PROCEDURE DIVISION.
    000500     DISPLAY "a literal that is continued on the next line without|
          -    " a gap"
          * floating
    000700D    DISPLAY "DEBUG"
    000800     STOP RUN.

    same words |}]

let%expect_test "change-of-source-format" =
  show ~source_format:Cobol_config.(SF SFFixed) {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG.
       >>SOURCE FORMAT IS FREE
PROCEDURE DIVISION.
    DISPLAY "FREE"
    STOP RUN.
|};
  [%expect {|
           IDENTIFICATION DIVISION.
           PROGRAM-ID. PROG.
          * >>SOURCE FORMAT IS FREE
           PROCEDURE DIVISION.
               DISPLAY "FREE"
               STOP RUN.

    warning: line 3: source format directive commented out
    same words |}]

let fixed_prog = {|000100 IDENTIFICATION DIVISION.                                         PROG0001
000200 PROGRAM-ID. PROG.                                                PROG0002
000300* comment line                                                    PROG0003
000400 PROCEDURE DIVISION.                                              PROG0004
000500     DISPLAY "a literal that is continued on the next line without
000600-    " a gap" *> floating
000700D    DISPLAY "DEBUG"
000800     MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2 C-3
000900     STOP RUN.
|}

let%expect_test "mf-directive" =
  (* GnuCOBOL does not accept `$' in the indicator area *)
  show ~source_format:Cobol_config.(SF SFFixed) ~target:"FIXED"
    {|      $SET NESTCALL
       PROGRAM-ID. PROG.
|};
  [%expect {|
           $SET NESTCALL
           PROGRAM-ID. PROG.

    prog.cob:1.11-1.19:
    >> Warning: Ignored compiler directive

    prog.cob:1.12-1.20:
    >> Warning: Ignored compiler directive

    same words |}]

let%expect_test "fixed-to-free" =
  show ~source_format:Cobol_config.(SF SFFixed) ~target:"FREE" fixed_prog;
  [%expect {|
    IDENTIFICATION DIVISION.
    PROGRAM-ID. PROG.
    *> comment line
    PROCEDURE DIVISION.
        DISPLAY "a literal that is continued on the next line without a gap"
    *> floating
    >>D    DISPLAY "DEBUG"
        MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2 C-3
        STOP RUN.

    prog.cob:7.0-7.22:
    >> Error: Invalid >>D compiler directive

    same words |}]

let%expect_test "fixed-to-cobolx" =
  show ~source_format:Cobol_config.(SF SFFixed) ~target:"COBOLX" fixed_prog;
  [%expect {|
     IDENTIFICATION DIVISION.
     PROGRAM-ID. PROG.
    * comment line
     PROCEDURE DIVISION.
         DISPLAY "a literal that is continued on the next line without a gap"
    * floating
    D    DISPLAY "DEBUG"
         MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2 C-3
         STOP RUN.

    same words |}]

let%expect_test "free-to-variable" =
  show ~target:"VARIABLE" {|PROCEDURE DIVISION.
    MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2 C-VERY-LONG-DATA-NAME-3 D-VERY-LONG-DATA-NAME-4
|};
  [%expect {|
           PROCEDURE DIVISION.
               MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2 C-VERY-LONG-DATA-NAME-3 D-VERY-LONG-DATA-NAME-4

    same words |}]

let%expect_test "free-to-cobolx-long-literal" =
  show ~target:"COBOLX" {|PROCEDURE DIVISION.
    DISPLAY "This is a very long literal that will not fit within the text area of a single line in COBOLX format, which has 254 columns for the text area, and that therefore needs to be continued on another line, but not in a padded way, unlike the FIXED format" UPON CONSOLE.
|};
  [%expect {|
     PROCEDURE DIVISION.
         DISPLAY "This is a very long literal that will not fit within the text area of a single line in COBOLX format, which has 254 columns for the text area, and that therefore needs to be continued on another line, but not in a padded way, unlike the FIXE|
    -    "D format" UPON CONSOLE.

    same words |}]

let%expect_test "with-source-format-directive" =
  show ~source_format:Cobol_config.(SF SFFixed) ~target:"FREE"
    ~add_source_format_directive:true {|       PROGRAM-ID. PROG.
       PROCEDURE DIVISION.
           STOP RUN.
|};
  show ~add_source_format_directive:true {|PROGRAM-ID. PROG.
PROCEDURE DIVISION.
    STOP RUN.
|};
  [%expect {|
    >>SOURCE FORMAT IS FREE
    PROGRAM-ID. PROG.
    PROCEDURE DIVISION.
        STOP RUN.

    same words
           >>SOURCE FORMAT IS FIXED
           PROGRAM-ID. PROG.
           PROCEDURE DIVISION.
               STOP RUN.

    same words |}]

let%expect_test "unclosed-literal" =
  (* the literal reaches column 72, and is not continued *)
  show ~source_format:Cobol_config.(SF SFFixed) ~target:"FREE"
    {|       PROCEDURE DIVISION.
           DISPLAY "A LITERAL THAT IS NEVER CLOSED ON THIS LINE OF TEXTS".
|};
  [%expect {|
    PROCEDURE DIVISION.
        DISPLAY "A LITERAL THAT IS NEVER CLOSED ON THIS LINE OF TEXTS"

    prog.cob:2.19-2.72:
    >> Error: Missing continuation of
              `A LITERAL THAT IS NEVER CLOSED ON THIS LINE OF TEXTS'

    same words |}]

let%expect_test "floating-comments-in-free-format" =
  show ~source_format:Cobol_config.(SF SFFixed) ~target:"FREE"
    {|       PROCEDURE DIVISION.
           DISPLAY "HELLO"     *> greetings
           STOP RUN. *> end
|};
  [%expect {|
    PROCEDURE DIVISION.
        DISPLAY "HELLO"     *> greetings
        STOP RUN. *> end

    same words |}]

let long_prog = {|PROCEDURE DIVISION.
    DISPLAY "This is a very long literal that will not fit within the right margin, and that has ""quotes"" in it" UPON CONSOLE
    MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2 C-VERY-LONG-DATA-NAME-3
    MOVE X"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789" TO Y
*> a comment that is long enough to be wrapped at the right margin in free format
    STOP RUN. *> a floating comment that would cross the right margin if left there
|}

let%expect_test "right-margin-free" =
  show ~target:"FREE" ~right_margin:72 long_prog;
  [%expect {|
    PROCEDURE DIVISION.
        DISPLAY "This is a very long literal that will not fit within " &
           "the right margin, and that has ""quotes"" in it" UPON CONSOLE
        MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2
           C-VERY-LONG-DATA-NAME-3
        MOVE
           X"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789"<- too long
           TO Y
    *> a comment that is long enough to be wrapped at the right margin in
    *> free format
    *> a floating comment that would cross the right margin if left there
        STOP RUN.

    warning: line 4: word "X\"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789\"" extends beyond the text area
    same words |}]

let%expect_test "right-margin-variable" =
  show ~target:"VARIABLE" ~right_margin:80 long_prog;
  [%expect {|
           PROCEDURE DIVISION.
               DISPLAY "This is a very long literal that will not fit within the " &|
                  "right margin, and that has ""quotes"" in it" UPON CONSOLE
               MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2
                  C-VERY-LONG-DATA-NAME-3
               MOVE
                  X"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789"<- too long
                  TO Y
          * a comment that is long enough to be wrapped at the right margin in free
          * format
          * a floating comment that would cross the right margin if left there
               STOP RUN.

    warning: line 4: word "X\"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789\"" extends beyond the text area
    same words |}]

let%expect_test "right-margin-cobolx" =
  show ~target:"COBOLX" ~right_margin:72 long_prog;
  [%expect {|
     PROCEDURE DIVISION.
         DISPLAY "This is a very long literal that will not fit within the r|
    -    "ight margin, and that has ""quotes"" in it" UPON CONSOLE
         MOVE A-VERY-LONG-DATA-NAME-1 TO B-VERY-LONG-DATA-NAME-2
            C-VERY-LONG-DATA-NAME-3
         MOVE X"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789AB|
    -    "CDEF0123456789" TO Y
    * a comment that is long enough to be wrapped at the right margin in
    * free format
    * a floating comment that would cross the right margin if left there
         STOP RUN.

    same words |}]

let%expect_test "right-margin-fixed" =
  (* no effect *)
  show ~target:"FIXED" ~right_margin:100 {|PROCEDURE DIVISION.
    STOP RUN.
|};
  (try show ~target:"FREE" ~right_margin:60 "" with
   | Invalid_argument msg -> Pretty.out "error: %s@\n" msg);
  [%expect {|
           PROCEDURE DIVISION.
               STOP RUN.

    same words
    error: right margin 60 is less than 72 |}]

let%expect_test "right-margin-debugging-line" =
  show ~source_format:Cobol_config.(SF SFFixed) ~target:"FIXED" {|       PROCEDURE DIVISION.
      D    DISPLAY A-VERY-LONG-DATA-NAME-1 B-VERY-LONG-DATA-NAME-2 C-VERY-LO
      D        NG-NAME
           STOP RUN.
|};
  show ~target:"FIXED" {|PROCEDURE DIVISION.
>>D DISPLAY A-VERY-LONG-DATA-NAME-1 B-VERY-LONG-DATA-NAME-2 C-VERY-LONG-DATA-NAME-3
    STOP RUN.
|};
  [%expect {|
           PROCEDURE DIVISION.
          D    DISPLAY A-VERY-LONG-DATA-NAME-1 B-VERY-LONG-DATA-NAME-2 C-VER|
          D        NG-NAME
               STOP RUN.

    same words
           PROCEDURE DIVISION.
          D    DISPLAY A-VERY-LONG-DATA-NAME-1 B-VERY-LONG-DATA-NAME-2
          D       C-VERY-LONG-DATA-NAME-3
               STOP RUN.

    prog.cob:2.0-2.83:
    >> Error: Invalid >>D compiler directive

    same words |}]
