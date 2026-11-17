(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Promise.Syntax

(* Tools are implemented by the LSP server (see [Lsp_tools]), that also gives
   their declarations in the manifest: the extension only registers them,
   resolves the files they work on, and forwards their calls. *)

let outline_tool_name = "superbol_cobol_outline"
let find_symbol_tool_name = "superbol_cobol_find_symbol"
let diagnostics_tool_name = "superbol_cobol_diagnostics"
let project_settings_tool_name = "superbol_cobol_project_settings"
let control_flow_tool_name = "superbol_cobol_control_flow"

let file_path_field = "filePath"

(* --- Inputs --- *)

let input_fields input =
  match Jsonoo.t_of_js input |> Jsonoo.Decode.(dict id) with
  | fields -> fields
  | exception Jsonoo.Decode_error _ -> Hashtbl.create 1

let input_string name input =
  match Hashtbl.find_opt (input_fields input) name with
  | Some json -> (try Some (Jsonoo.Decode.string json)
                  with Jsonoo.Decode_error _ -> None)
  | None -> None

(** How a tool gets the file it works on. *)
type file_requirement =
  | No_file                             (** none, or the one it is given *)
  | Default_to_active_editor     (** the one it is given, or the active one *)
  | Open_file    (** same as above, opened so that the server analyzes it *)

(** Resolves a path given by a language model, which should be absolute, but
    may be relative to the (single) workspace folder.  Only needed to open
    files: the server resolves the paths it is given. *)
let resolve_path path =
  if not (Filename.is_relative path)
  then Ok (Vscode.Uri.file path)
  else match Vscode.Workspace.workspaceFolders () with
    | [folder] ->
        Ok (Vscode.Uri.joinPath (Vscode.WorkspaceFolder.uri folder)
              ~pathSegments:[path])
    | _ ->
        Error (Printf.sprintf "%S is not an absolute path." path)

let open_cobol_document uri : (unit, string) result Promise.t =
  Promise.catch
    ~rejected:(fun _ ->
        Promise.return @@ Error (Printf.sprintf "Unable to open %s."
                                   (Vscode.Uri.fsPath uri)))
    (let+ doc = Vscode.Workspace.openTextDocument (`Uri uri) in
     match Vscode.TextDocument.languageId doc with
     | "cobol" ->
         Ok ()
     | lang ->
         Error (Printf.sprintf "%s is not a COBOL source file (language: %s)."
                  (Vscode.Uri.fsPath uri) lang))

(** Arguments to send to the server: those of the language model, with the
    file of the active editor for tools that default to it, and with the
    absolute path of the file for tools that need it open. *)
let arguments ~file input : (Jsonoo.t, string) result Promise.t =
  let fields = input_fields input in
  let encode () = Jsonoo.Encode.dict Fun.id fields in
  let set_file uri =
    Hashtbl.replace fields file_path_field
      (Jsonoo.Encode.string (Vscode.Uri.fsPath uri))
  in
  let given_path = input_string file_path_field input in
  match file, given_path with
  | No_file, _ | Default_to_active_editor, Some _ ->
      Promise.return (Ok (encode ()))
  | Default_to_active_editor, None ->
      Option.iter set_file (Superbol_instance.current_document_uri ());
      Promise.return (Ok (encode ()))
  | Open_file, _ ->
      match
        match given_path with
        | Some path -> Result.map Option.some (resolve_path path)
        | None -> Ok (Superbol_instance.current_document_uri ())
      with
      | Error msg ->
          Promise.return (Error msg)
      | Ok None ->
          Promise.return (Error "No file was given, and no editor is active.")
      | Ok (Some uri) ->
          (* The server only analyzes the documents it was sent. *)
          open_cobol_document uri >>|
          Result.map (fun () -> set_file uri; encode ())

(* --- Calls --- *)

let error_message err =
  match Ojs.get_prop_ascii (Promise.error_to_js err) "message" with
  | m when Ojs.type_of m = "string" -> Ojs.string_of_js m
  | _ | exception _ -> "unknown error"

let call_tool instance ~name ~file input : string Promise.t =
  arguments ~file input >>= function
  | Error msg ->
      Promise.return msg
  | Ok arguments ->
      let data = Jsonoo.Encode.(object_ ["name", string name;
                                         "arguments", arguments]) in
      Promise.catch
        ~rejected:(fun err ->
            Promise.return @@
            Error ("The language server failed to answer: " ^
                   error_message err))
        (Superbol_instance.lsp_request instance ~meth:"superbol/callTool"
           ~data >>| Result.map_error Superbol_printer.show_error)
      >>| function
      | Ok json ->
          (try Jsonoo.Decode.(field "text" string) json
           with Jsonoo.Decode_error msg ->
             "Invalid answer from the language server: " ^ msg)
      | Error msg ->
          msg

(* --- Registration --- *)

let file_or_editor input =
  match input_string file_path_field input with
  | Some path -> Filename.basename path
  | None -> "the active editor"

(** Tools, with how they get their file, and the message shown while they
    run. *)
let tools : (string * file_requirement * (Ojs.t -> string)) list =
  [
    outline_tool_name, Open_file,
    (fun i -> "Outlining " ^ file_or_editor i);
    find_symbol_tool_name, No_file,
    (fun i -> match input_string "query" i with
       | Some q -> Printf.sprintf "Searching COBOL symbols for %S" q
       | None -> "Searching COBOL symbols");
    diagnostics_tool_name, No_file,
    (fun i -> match input_string file_path_field i with
       | Some path -> "Listing problems in " ^ Filename.basename path
       | None -> "Listing COBOL problems");
    project_settings_tool_name, Default_to_active_editor,
    (fun _ -> "Reading SuperBOL project settings");
    control_flow_tool_name, Open_file,
    (fun i -> "Computing the control flow of " ^
              match input_string "name" i with
              | Some name -> name
              | None -> file_or_editor i);
  ]

let tool instance ~name ~file ~invocation_message =
  let open Vscode in
  LanguageModelTool.create ()
    ~prepareInvocation:begin fun ~options ~token:_ ->
      let input = LanguageModelToolInvocationPrepareOptions.input options in
      `Value (Some (PreparedToolInvocation.create ()
                     ~invocationMessage:(`String (invocation_message input))))
    end
    ~invoke:begin fun ~options ~token:_ ->
      let input = LanguageModelToolInvocationOptions.input options in
      `Promise (call_tool instance ~name ~file input >>| fun text ->
                Some (LanguageModelToolResult.of_text text))
    end

let register instance =
  if Vscode.Lm.tools_available () then
    List.iter begin fun (name, file, invocation_message) ->
      Superbol_instance.subscribe_disposable instance @@
      Vscode.Lm.registerTool ~name
        ~tool:(tool instance ~name ~file ~invocation_message)
    end tools
