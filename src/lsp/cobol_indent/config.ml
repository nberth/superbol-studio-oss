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

open Ez_file.V1
open EzFile.OP
open Cobol_common.Platform.TYPES

open Types

let verbose = false

let default = {
  arg_offset = 3 ;
  inner_offset = 3 ;
  data_item_offset = None ;
  select_in_area_a = false ;

  whole_file_indent = 0 ;
}

let to_string config =
  let b = Buffer.create 1000 in
  Printf.bprintf b "{\n";
  Printf.bprintf b "  arg_offset = %d ;\n" config.arg_offset ;
  Printf.bprintf b "  inner_offset = %d ;\n" config.inner_offset ;
  Printf.bprintf b "  data_item_offset = %s ;\n" (match config.data_item_offset with
      | None -> "gap"
      | Some n -> string_of_int n);
  Printf.bprintf b "  select_in_area_a = %b ;\n" config.select_in_area_a ;
  Printf.bprintf b "  whole_file_indent = %d ;\n" config.whole_file_indent ;
  Printf.bprintf b "}";
  Buffer.contents b


(* Options given as [(name, value)] pairs, e.g, from the [indent] table of
   the project configuration.  Unknown options are ignored. *)
let apply_unparsed config (unparsed: unparsed_config) =
  List.fold_left begin fun config (option, n) ->
    match String.lowercase_ascii option with
    | "whole-file-indent" -> { config with whole_file_indent = n }
    | "arg-offset" -> { config with arg_offset = n }
    | "inner-offset" -> { config with inner_offset = n }
    | "data-item-offset" -> { config with data_item_offset = Some n }
    | "select-in-area-a" -> { config with select_in_area_a = n <> 0 }
    | _ -> config
  end config unparsed

let load ~platform ?(unparsed = []) ~filename () =
  if verbose then
    Printf.eprintf "Config.load for %S\n%!" filename;
  let filename =
    if Filename.is_relative filename then
      ( platform.getcwd ()) // filename
    else
      filename
  in
  let dirname = Filename.dirname filename in
  let default = apply_unparsed default unparsed in

  let rec iter dir =
    let dirdir = Filename.dirname dir in
    if dir = dirdir then
      default
    else
      let config = iter dirdir in
      let file = Filename.concat dir ".superbol-indent" in
      if Sys.file_exists file then
        let config = ref config in
        if verbose then
          Printf.eprintf "Reading file %S\n%!" file;
        EzFile.iter_lines (fun line ->
            let len = String.length line in
            if len > 0 then
              if line.[0] != '#' then
                let option, value = EzString.cut_at line '=' in
                let option = String.trim option in
                let value = String.trim value in
                let int f =
                  match int_of_string_opt value with
                  | Some n -> config := f n
                  | None ->
                      Printf.eprintf "Warning in %S: invalid value %S for \
                                      option %S\n%!" file value option
                in
                match String.lowercase_ascii option,
                      String.lowercase_ascii value
                with
                | "format", _ ->
                  (* the source format now comes from the project
                     configuration *)
                  ()

                | "whole-file-indent", _ ->
                  int (fun n -> { !config with whole_file_indent = n })

                | "arg-offset", _ ->
                  int (fun n -> { !config with arg_offset = n })

                | "select-in-area-a", ( "true" | "yes" | "y" ) ->
                  config := { !config with select_in_area_a = true }

                | "select-in-area-a", _ ->
                  config := { !config with select_in_area_a = false }

                | ( "data-item-offset" ), _ ->
                  int (fun n -> { !config with data_item_offset = Some n })

                | ( "inner-offset" ), _ ->
                  int (fun n -> { !config with inner_offset = n })

                | _ ->
                  Printf.eprintf "Warning in %S: unknown option %S\n%!"
                    file option
          ) file;
        !config
      else
        config
  in
  iter dirname

let generate ?(config=default) ?(only_comment = true) filename =
  let action = if only_comment then "# " else "" in
  let oc = open_out filename in

  let string_of_int_option x =
    match x with
    | None -> ""
    | Some n -> string_of_int n
  in

  List.iter (fun (name, comment, value, default) ->
      Printf.fprintf oc
        "### %s: %s\n" name comment;
      Printf.fprintf oc "%s%s = %s\n\n"
        (if value <> default then
           action else "# ") name
        value ;
    )

    [
      "whole-file-indent",
      "apply the given indent to the whole file",
      string_of_int config.whole_file_indent,
      string_of_int default.whole_file_indent ;

      "arg-offset",
      "offset of arguments",
      string_of_int config.arg_offset,
      string_of_int default.arg_offset ;

      "inner-offset",
      "offset of inner commands in blocks",
      string_of_int config.inner_offset,
      string_of_int default.inner_offset ;

      "data-item-offset",
      "offset of inner data items (comment for aligned)",
      string_of_int_option config.data_item_offset,
      string_of_int_option default.data_item_offset ;

      "select-in-area-a",
      "force select in area A instead of area B",
      string_of_bool config.select_in_area_a,
      string_of_bool default.select_in_area_a ;

    ] ;

  close_out oc;
  Printf.eprintf "File %S generated\n%!" filename
