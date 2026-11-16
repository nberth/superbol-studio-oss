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

let outline_tool_name = "superbol_cobol_outline"

(* --- *)

(** Text result of a tool, to be read by a language model. *)
let text_result text =
  Vscode.LanguageModelToolResult.of_text text

(** Decodes the input of a tool, given as a JSON object. *)
let input_field name decoder input =
  try Jsonoo.Decode.(try_optional (field name decoder)) (Jsonoo.t_of_js input)
  with Jsonoo.Decode_error _ -> None

(** Resolves a path given by a language model, which should be absolute, but
    may be relative to the (single) workspace folder. *)
let resolve_path path =
  if not (Filename.is_relative path)
  then Ok (Vscode.Uri.file path)
  else match Vscode.Workspace.workspaceFolders () with
    | [folder] ->
        Ok (Vscode.Uri.joinPath (Vscode.WorkspaceFolder.uri folder)
              ~pathSegments:[path])
    | _ ->
        Error (Printf.sprintf "%S is not an absolute path." path)

(** The given file, or the one shown in the active editor. *)
let target_uri = function
  | Some path ->
      resolve_path path
  | None ->
      match Superbol_instance.current_document_uri () with
      | Some uri -> Ok uri
      | None -> Error "No file was given, and no editor is active."

let display_path uri =
  Vscode.Workspace.asRelativePath ~pathOrUri:(`Uri uri) ()

(* --- *)

(** Document symbols, as sent by the LSP server. *)
type symbol =
  {
    name: string;
    first_line: int;                                            (* 0-based *)
    last_line: int;                                             (* 0-based *)
    children: symbol list;
  }

let rec decode_symbol json =
  let open Jsonoo.Decode in
  {
    name = field "name" string json;
    first_line = at ["range"; "start"; "line"] int json;
    last_line = at ["range"; "end"; "line"] int json;
    children = try_default [] (field "children" (list decode_symbol)) json;
  }

let rec count_symbols symbols =
  List.fold_left (fun n s -> n + 1 + count_symbols s.children) 0 symbols

(** Maximum number of lines in an outline, so as to spare the context of the
    language model on very large programs. *)
let max_outline_lines = 2000

let render_outline ?max_depth ~path symbols =
  let buf = Buffer.create 4096 in
  let lines = ref 0 and omitted = ref 0 in
  Printf.bprintf buf
    "Outline of %s (lines are 1-based; indentation shows nesting):\n" path;
  let rec render depth s =
    if !lines >= max_outline_lines then
      omitted := !omitted + 1 + count_symbols s.children
    else begin
      incr lines;
      Buffer.add_string buf (String.make (2 * depth) ' ');
      Buffer.add_string buf s.name;
      if s.first_line = s.last_line
      then Printf.bprintf buf "  [L%d]" (s.first_line + 1)
      else Printf.bprintf buf "  [L%d-%d]" (s.first_line + 1) (s.last_line + 1);
      match max_depth, s.children with
      | Some d, (_ :: _ as children) when depth + 1 >= d ->
          let n = count_symbols children in
          Printf.bprintf buf "  (%d nested symbol%s hidden)\n"
            n (if n = 1 then "" else "s")
      | _ ->
          Buffer.add_char buf '\n';
          List.iter (render (depth + 1)) s.children
    end
  in
  List.iter (render 0) symbols;
  if !omitted > 0 then
    Printf.bprintf buf
      "... %d more symbols omitted; use `maxDepth` for a shallower outline.\n"
      !omitted;
  Buffer.contents buf

let request_document_symbols instance uri =
  let data =
    Jsonoo.Encode.(object_ [
        "textDocument", object_ [
          "uri", string @@ Vscode.Uri.toString uri ()
        ]
      ])
  in
  Superbol_instance.lsp_request instance ~data
    ~meth:"textDocument/documentSymbol" >>| function
  | Ok json ->
      Ok (Jsonoo.Decode.(nullable (list decode_symbol)) json
          |> Option.value ~default:[])
  | Error e ->
      Error (Superbol_printer.show_error e)

let outline instance ~path ?max_depth () =
  match target_uri path with
  | Error msg ->
      Promise.return msg
  | Ok uri ->
      (* Opening the document has the language client send it to the server,
         which only reports symbols for documents it knows of. *)
      Promise.catch
        ~rejected:(fun _ ->
            Promise.return @@ Error (Printf.sprintf "Unable to open %s."
                                       (Vscode.Uri.fsPath uri)))
        (let+ doc = Vscode.Workspace.openTextDocument (`Uri uri) in Ok doc)
      >>= function
      | Error msg ->
          Promise.return msg
      | Ok doc when Vscode.TextDocument.languageId doc <> "cobol" ->
          Promise.return @@
          Printf.sprintf "%s is not a COBOL source file (language: %s)."
            (display_path uri) (Vscode.TextDocument.languageId doc)
      | Ok _ ->
          Promise.catch
            ~rejected:(fun _ ->
                Promise.return @@ Error "The language server failed to answer.")
            (request_document_symbols instance uri)
          >>| function
          | Error msg ->
              msg
          | Ok [] ->
              Printf.sprintf "No symbols found in %s: it may be a copybook, or                               may not have been parsed successfully."
                (display_path uri)
          | Ok symbols ->
              render_outline ?max_depth ~path:(display_path uri) symbols

let outline_tool instance =
  let open Vscode in
  LanguageModelTool.create ()
    ~prepareInvocation:begin fun ~options ~token:_ ->
      let input = LanguageModelToolInvocationPrepareOptions.input options in
      let file = match input_field "filePath" Jsonoo.Decode.string input with
        | Some path -> Filename.basename path
        | None -> "the active editor"
      in
      `Value (Some (PreparedToolInvocation.create ()
                     ~invocationMessage:(`String ("Outlining " ^ file))))
    end
    ~invoke:begin fun ~options ~token:_ ->
      let input = LanguageModelToolInvocationOptions.input options in
      let path = input_field "filePath" Jsonoo.Decode.string input
      and max_depth = input_field "maxDepth" Jsonoo.Decode.int input in
      `Promise (outline instance ~path ?max_depth () >>| fun text ->
                Some (text_result text))
    end

(* --- *)

let register instance =
  if Vscode.Lm.tools_available () then
    Superbol_instance.subscribe_disposable instance @@
    Vscode.Lm.registerTool ~name:outline_tool_name ~tool:(outline_tool instance)
