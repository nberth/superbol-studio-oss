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

(** Tests for the conversion between source formats
    ([superbol/convertSourceFormat]). *)

open Lsp_testing

let convert ?(keep_sequence_numbers = false)
    ?(add_source_format_directive = false) ?(target = "FIXED")
    ~source_format text =
  let { projdir; end_with_postproc }, server =
    make_lsp_project () ~toml:(Printf.sprintf {toml|
      [cobol]
      source-format = "%s"
    |toml} source_format)
  in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" text in
  begin match
    LSP.Request.INTERNAL.convert_source_format server ~uri:prog.uri
      ~target:(Result.get_ok @@
               Cobol_indent.Format_converter.target_of_name target)
      ~keep_sequence_numbers ~add_source_format_directive
  with
  | None ->
      Pretty.out "no conversion"
  | Some (text, warnings) ->
      String.split_on_char '\n' text |>
      List.iter (fun l -> Pretty.out "%s|@\n" l);
      List.iter (fun Cobol_indent.Format_converter.{ line; message } ->
          Pretty.out "warning: line %d: %s@\n" line message) warnings
  end;
  end_with_postproc

let%expect_test "convert-free-to-fixed" =
  let end_with_postproc = convert ~source_format:"free" {cobol|
IDENTIFICATION DIVISION.
PROGRAM-ID. PROG.
PROCEDURE DIVISION.
    DISPLAY "HELLO" *> greetings
    >>SOURCE FORMAT IS FIXED
       STOP RUN.
|cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    |
           IDENTIFICATION DIVISION.|
           PROGRAM-ID. PROG.|
           PROCEDURE DIVISION.|
          * greetings|
               DISPLAY "HELLO"|
          * >>SOURCE FORMAT IS FIXED|
           STOP RUN.|
    |
    warning: line 6: source format directive commented out |}]

let%expect_test "convert-fixed-to-free" =
  let end_with_postproc = convert ~source_format:"fixed" ~target:"FREE"
      ~add_source_format_directive:true {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG.
      * comment
       PROCEDURE DIVISION.
           DISPLAY "HELLO".
|cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    >>SOURCE FORMAT IS FREE|
    |
    IDENTIFICATION DIVISION.|
    PROGRAM-ID. PROG.|
    *> comment|
    PROCEDURE DIVISION.|
        DISPLAY "HELLO".|
    | |}]
