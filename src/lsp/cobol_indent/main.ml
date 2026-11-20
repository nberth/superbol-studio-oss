(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(* Known limitations:

* No support for directives >> EVALUATE
* No support for negatively indented lines (i.e. lines that start before
  area A)
* Not yet understood:
  * REPOSITORY, FUNCTION, FUNCTION-ID
* Possible indentation improvements:
  * COMPUTE xxx = A
                  + B
*)

open Ez_file.V1

open Types

let apply_edits = Line_editor.apply

(* TODO: do not try to indent lines after range. Maybe we could save a
   state at a given position, and indent a range by starting at the
   closest line with a saved state. *)
let indent ~options ~config ~filename ?(verbose=false) ?output ?contents ?range
    () =
  let platform = options.Cobol_preproc.Options.platform in
  let contents = match contents with
    | None -> platform.read_text_file filename
    | Some contents -> contents
  in
  let range = match range with
    | None -> { start_line = 1; end_line = max_int }
    | Some range -> range
  in

  let config =
    Config.load ~platform ~unparsed:config ~filename ()
  in
  if Engine.verbose then
    Printf.eprintf "Config: %s\n%!" (Config.to_string config);

  let src = Source_lines.scan ~options ~filename contents in
  let { Line_scanner.toks; edits } = Line_scanner.tokenize ~config src in
  Engine.indent_tokens ~config toks;

  let edits =
    List.filter (fun edit ->
        edit.offset_orig <> edit.offset_modif &&
        range.start_line <= edit.lnum && edit.lnum <= range.end_line
      ) edits
  in

  if verbose then begin
    if edits = [] then
      Printf.eprintf "File %S: good indentation\n%!" filename
    else begin
      Printf.eprintf "File %S: %d lines to modify\n%!" filename
        ( List.length edits );
      List.iter (fun edit ->
          Printf.printf "  Line %d: move from %d to %d\n%!"
            edit.lnum edit.offset_orig edit.offset_modif
        ) edits;
    end ;
  end;

  let line_edits = Line_editor.edits src edits in
  begin match output with
    | None -> ()
    | Some "-" ->
        print_string (apply_edits contents line_edits)
    | Some output ->
        EzFile.write_file output (apply_edits contents line_edits);
        Printf.eprintf "File %S indented\n%!" output
  end;
  edits, line_edits
