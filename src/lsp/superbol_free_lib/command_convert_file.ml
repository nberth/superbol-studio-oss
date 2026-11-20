(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2
open EZCMD.TYPES

let action ~target:target_name ?right_margin ~keep_sequence_numbers
    ~add_source_format_directive
    ~inplace ?suffix Common_args.{ preproc_options; platform; _ } files =
  let target =
    let open Cobol_indent.Format_converter in
    match
      Result.bind (target_of_name target_name) begin fun target ->
        match right_margin with
        | None -> Ok target
        | Some margin -> with_right_margin margin target
      end
    with
    | Ok target -> target
    | Error msg -> Pretty.failwith "%s" msg
  in
  let convert ~filename contents =
    let src =
      Cobol_indent.Source_lines.scan ~options:preproc_options ~filename
        contents
    in
    let text, warnings =
      Cobol_indent.Format_converter.convert ~target src
        ~keep_sequence_numbers ~add_source_format_directive
        ~eol:(Cobol_indent.Format_converter.eol_of_text contents)
    in
    List.iter (fun Cobol_indent.Format_converter.{ line; message } ->
        Pretty.error "%s:%d: Warning: %s@." filename line message) warnings;
    text
  in
  let output ~filename text =
    match suffix with
    | Some ext ->
        let output = filename ^ "." ^ ext in
        Ez_file.V1.EzFile.write_file output text;
        Pretty.error "File %S generated@." output
    | None when inplace ->
        Ez_file.V1.EzFile.write_file filename text;
        Pretty.error "File %S converted@." filename
    | None ->
        print_string text
  in
  match files with
  | [] ->
      let contents = In_channel.input_all stdin in
      let filename = Filename.concat (Sys.getcwd ()) "stdin.cob" in
      print_string (convert ~filename contents)
  | files ->
      List.iter (fun filename ->
          output ~filename
            (convert ~filename (platform.read_text_file filename))) files

let cmd =
  let files = ref [] in
  let inplace = ref false in
  let suffix = ref None in
  let target = ref "FIXED" in
  let right_margin = ref None in
  let keep_sequence_numbers = ref false in
  let add_source_format_directive = ref false in
  let common, common_args = Common_args.get () in
  EZCMD.sub
    "convert"
    (fun () ->
       action
         ~target:!target
         ?right_margin:!right_margin
         ~keep_sequence_numbers:!keep_sequence_numbers
         ~add_source_format_directive:!add_source_format_directive
         ~inplace:!inplace
         ?suffix:!suffix
         (common ())
         !files)
    ~args:(common_args @ [
        [], Arg.Anons (fun list -> files := list),
        EZCMD.info ~docv:"FILES" "COBOL files to convert" ;

        [ "to-format" ],
        Arg.Symbol (List.concat_map (fun n -> [n; String.lowercase_ascii n])
                      Cobol_indent.Format_converter.supported_target_names,
                    fun f -> target := f),
        EZCMD.info ~docv:"FORMAT"
          "Target source format (FIXED by default)";

        [ "right-margin" ], Arg.Int (fun c -> right_margin := Some c),
        EZCMD.info ~docv:"COLUMN"
          (Printf.sprintf
             "Write nothing after $(docv) (at least %d), in target formats \
              whose text area ends after it (VARIABLE, XCARD, COBOLX, and \
              FREE).  Alphanumeric literals that do not fit are then split \
              into several literals concatenated with $(b,&) (or continued \
              on the next line in COBOLX format)"
             Cobol_indent.Format_converter.min_right_margin);

        [ "inplace" ], Arg.Set inplace,
        EZCMD.info "Modify files in place";

        [ "suffix" ], Arg.String (fun s -> suffix := Some s),
        EZCMD.info ~docv:"EXT"
          "Write the result of converting FILE into FILE.EXT";

        [ "keep-sequence-numbers" ], Arg.Set keep_sequence_numbers,
        EZCMD.info
          "Keep the contents of the sequence area (columns 1-6) of lines \
           that are already in a format with a sequence area, if the target \
           format has one; the sequence area is blank otherwise";

        [ "add-source-format-directive" ], Arg.Set add_source_format_directive,
        EZCMD.info
          "Insert a $(b,>>SOURCE FORMAT IS) directive for the target format \
           as first line (note that, with Micro Focus dialects, $(b,FREE) \
           is read as the X/Open free format)";
      ])
    ~doc:"Convert COBOL source files into another source format"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Converts COBOL source files into the source format given with \
            $(b,--to-format) (FIXED by default, or COBOL85, VARIABLE, XCARD, \
            COBOLX, or FREE), and prints the result on the standard output \
            (unless $(b,--inplace) or $(b,--suffix) is given).  The source \
            format of each file is taken from the command line \
            ($(b,--source-format)), or guessed, and may change via compiler \
            directives.";
        `P "Each line keeps the column of its first word relatively to area \
            A, unless it is too long for the target format: it is then moved \
            to the left (without leaving area B), and then split between \
            words.  Alphanumeric literals that do not fit are continued on \
            the next lines.  Comments and debugging lines are written as the \
            target format requires; in formats with an indicator, floating \
            comments ($(b,*>)) are moved to comment lines just before the \
            line they were on.  Directives that change the source format are \
            commented out.";
      ];
    ]
