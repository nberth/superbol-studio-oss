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

open Ezcmd.V2
open EZCMD.TYPES

let encoding () =
  let encoding = ref None in
  (fun () -> !encoding),
  [ ["encoding"],
    Arg.String begin fun s ->
      (* Unsupported encodings are not an error, as the LSP server reads files
         as they are with those. *)
      encoding :=
        match Cobol_common.Text_encoding.of_name s with
        | Ok enc ->
            Some enc
        | Error msg ->
            Cobol_common.Diagnostics.Now.warn Fmt.stderr
              "%s;@ files@ are@ read@ as@ they@ are" msg;
            None
    end,
    EZCMD.info ~docv:"ENCODING"
      "Decode files as Visual Studio Code does with this value of its \
       `files.encoding' setting (e.g. `windows1252', `cp850').  Files are read \
       as they are by default, or (with a warning listing the supported \
       encodings) if ENCODING is not supported" ]

let get () =
  let project_root = ref None in
  let encoding, encoding_args = encoding () in
  let args = [
    ["project-root"; "root"], Arg.File (fun f -> project_root := Some f),
    EZCMD.info ~docv:"DIR" "Set the root of the project (autodetected on a \
                            per-file basis otherwise)";
  ] @ encoding_args in
  let given arg ~default = match arg with None -> default | given -> given in
  let get ?(setup = Project_commands.default_setup) () =
    Project_commands.{
      setup with
      project_root = given !project_root ~default:setup.project_root;
      encoding = given (encoding ()) ~default:setup.encoding;
    }
  in
  get, args

(* For [--client-settings]. *)
let client_settings_of_string s =
  match Yojson.Safe.from_string s with
  | `Assoc assoc -> assoc
  | `Null -> []
  | _ | exception Yojson.Json_error _ ->
      Cobol_common.fatal "Expected a JSON object: %s" s

let client_settings ~doc () =
  let client_settings = ref None in
  (fun () -> !client_settings),
  [ ["client-settings"],
    Arg.String begin fun s ->
      client_settings := Some (client_settings_of_string s)
    end,
    EZCMD.info ~docv:"JSON" doc ]
