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
let find_symbol_tool_name = "superbol_cobol_find_symbol"
let diagnostics_tool_name = "superbol_cobol_diagnostics"
let project_settings_tool_name = "superbol_cobol_project_settings"
let control_flow_tool_name = "superbol_cobol_control_flow"

(* --- Inputs --- *)

(** Decodes a field of the input of a tool, given as a JSON object. *)
let input_field name decoder input =
  try Jsonoo.Decode.(try_optional (field name decoder)) (Jsonoo.t_of_js input)
  with Jsonoo.Decode_error _ -> None

let file_path_input = input_field "filePath" Jsonoo.Decode.string

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

(* --- Outputs --- *)

let display_path uri =
  Vscode.Workspace.asRelativePath ~pathOrUri:(`Uri uri) ()

let display_uri_string s =
  display_path (Vscode.Uri.parse s ())

(** 1-based line range, from 0-based line numbers. *)
let pp_lines buf (first, last) =
  if first = last
  then Printf.bprintf buf "[L%d]" (first + 1)
  else Printf.bprintf buf "[L%d-%d]" (first + 1) (last + 1)

(** Maximum number of lines in the outputs of tools, so as to spare the context
    of language models on very large programs or workspaces. *)
let max_output_lines = 2000

(* --- Requests --- *)

let error_message err =
  match Ojs.get_prop_ascii (Promise.error_to_js err) "message" with
  | m when Ojs.type_of m = "string" -> Ojs.string_of_js m
  | _ | exception _ -> "unknown error"

let request instance ~meth ~data : (Jsonoo.t, string) result Promise.t =
  Promise.catch
    ~rejected:(fun err ->
        Promise.return @@
        Error ("The language server failed to answer: " ^ error_message err))
    (Superbol_instance.lsp_request instance ~meth ~data >>| function
      | Ok json -> Ok json
      | Error e -> Error (Superbol_printer.show_error e))

let uri_data ?(extra = []) uri =
  Jsonoo.Encode.(object_ (("uri", string @@ Vscode.Uri.toString uri ())
                          :: extra))

(** Opens the given COBOL document, which has the language client send it to
    the server: the server only answers requests about documents it knows
    of. *)
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
                  (display_path uri) lang))

let ( let** ) p f =
  p >>= function Ok x -> f x | Error msg -> Promise.return msg

(* --- Outline --- *)

(** Document symbols, as sent by the LSP server. *)
type doc_symbol =
  {
    name: string;
    lines: int * int;                                           (* 0-based *)
    children: doc_symbol list;
  }

let rec decode_doc_symbol json =
  let open Jsonoo.Decode in
  {
    name = field "name" string json;
    lines = (at ["range"; "start"; "line"] int json,
             at ["range"; "end"; "line"] int json);
    children = try_default [] (field "children" (list decode_doc_symbol)) json;
  }

let rec count_symbols symbols =
  List.fold_left (fun n s -> n + 1 + count_symbols s.children) 0 symbols

let render_outline ?max_depth ~path symbols =
  let buf = Buffer.create 4096 in
  let lines = ref 0 and omitted = ref 0 in
  Printf.bprintf buf
    "Outline of %s (lines are 1-based; indentation shows nesting):\n" path;
  let rec render depth s =
    if !lines >= max_output_lines then
      omitted := !omitted + 1 + count_symbols s.children
    else begin
      incr lines;
      Printf.bprintf buf "%s%s  %a" (String.make (2 * depth) ' ') s.name
        pp_lines s.lines;
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

let outline instance input =
  let max_depth = input_field "maxDepth" Jsonoo.Decode.int input in
  let** uri = Promise.return @@ target_uri (file_path_input input) in
  let** () = open_cobol_document uri in
  let data =
    Jsonoo.Encode.(object_ [
        "textDocument", object_ ["uri", string @@ Vscode.Uri.toString uri ()]
      ])
  in
  let** json = request instance ~meth:"textDocument/documentSymbol" ~data in
  Promise.return @@
  match Jsonoo.Decode.(nullable (list decode_doc_symbol)) json with
  | None | Some [] ->
      Printf.sprintf "No symbols found in %s: it may be a copybook, or may \
                      not have been parsed successfully." (display_path uri)
  | Some symbols ->
      render_outline ?max_depth ~path:(display_path uri) symbols

(* --- Symbol search --- *)

type ws_symbol =
  {
    sym_name: string;
    sym_kind: int;
    sym_container: string option;
    sym_uri: string;
    sym_lines: int * int;
  }

let decode_ws_symbol json =
  let open Jsonoo.Decode in
  {
    sym_name = field "name" string json;
    sym_kind = field "kind" int json;
    sym_container = try_optional (field "containerName" string) json;
    sym_uri = at ["location"; "uri"] string json;
    sym_lines = (at ["location"; "range"; "start"; "line"] int json,
                 at ["location"; "range"; "end"; "line"] int json);
  }

(* See [Lsp_workspace_symbols] for the kinds the server uses. *)
let describe_symbol_kind { sym_kind; sym_container; _ } =
  match sym_kind, sym_container with
  | 2, _ -> "program"
  | 3, _ -> "section"
  | 5, _ -> "class"
  | 6, _ -> "method"
  | 11, _ -> "interface"
  | 12, Some _ -> "paragraph"
  | 12, None -> "function"
  | (8 | 13), _ -> "data item"
  | 14, _ -> "constant"
  | 22, _ -> "condition-name"
  | _ -> "symbol"

let default_symbol_limit = 50

let find_symbol instance input =
  match input_field "query" Jsonoo.Decode.string input with
  | None | Some "" ->
      Promise.return "No query was given."
  | Some query ->
      let limit =
        input_field "limit" Jsonoo.Decode.int input |>
        Option.value ~default:default_symbol_limit |> max 1
      in
      let data = Jsonoo.Encode.(object_ ["query", string query]) in
      let** json = request instance ~meth:"workspace/symbol" ~data in
      let symbols =
        Jsonoo.Decode.(nullable (list decode_ws_symbol)) json |>
        Option.value ~default:[]
      in
      Promise.return @@
      match symbols with
      | [] ->
          Printf.sprintf "No symbol matches %S." query
      | symbols ->
          let buf = Buffer.create 4096 in
          let total = List.length symbols in
          Printf.bprintf buf "Symbols matching %S, best matches first \
                              (lines are 1-based):\n" query;
          List.iteri begin fun i s ->
            if i < limit then
              Printf.bprintf buf "%s %s%s: %s %a\n"
                (describe_symbol_kind s) s.sym_name
                (match s.sym_container with
                 | Some c -> " (in " ^ c ^ ")" | None -> "")
                (display_uri_string s.sym_uri) pp_lines s.sym_lines
          end symbols;
          if total > limit then
            Printf.bprintf buf "... %d more matches omitted.\n" (total - limit);
          Buffer.contents buf

(* --- Diagnostics --- *)

let severity_rank : Vscode.DiagnosticSeverity.t -> int = function
  | Error -> 0 | Warning -> 1 | Information -> 2 | Hint -> 3

let severity_name : Vscode.DiagnosticSeverity.t -> string = function
  | Error -> "error" | Warning -> "warning"
  | Information -> "info" | Hint -> "hint"

let render_diagnostics ~scope diags =
  let open Vscode in
  let diags =
    List.concat_map (fun (uri, ds) ->
        let path = display_path uri in
        List.map (fun d -> path, d) ds) diags |>
    List.stable_sort begin fun (p1, d1) (p2, d2) ->
      let pos d = Position.line (Range.start (Diagnostic.range d)) in
      match Int.compare (severity_rank (Diagnostic.severity d1))
              (severity_rank (Diagnostic.severity d2)) with
      | 0 -> (match String.compare p1 p2 with
          | 0 -> Int.compare (pos d1) (pos d2)
          | c -> c)
      | c -> c
    end
  in
  match diags with
  | [] ->
      Printf.sprintf "SuperBOL reports no problems %s." scope
  | diags ->
      let buf = Buffer.create 4096 in
      let count sev =
        List.length (List.filter (fun (_, d) -> Diagnostic.severity d = sev)
                       diags)
      in
      Printf.bprintf buf "SuperBOL reports %d error(s) and %d warning(s) %s \
                          (lines and columns are 1-based):\n"
        (count Error) (count Warning) scope;
      List.iteri begin fun i (path, d) ->
        if i < max_output_lines then
          let start = Range.start (Diagnostic.range d) in
          Printf.bprintf buf "%s:%d:%d: %s: %s\n" path
            (Position.line start + 1) (Position.character start + 1)
            (severity_name (Diagnostic.severity d))
            (String.concat " " @@
             String.split_on_char '\n' @@ Diagnostic.message d)
      end diags;
      let total = List.length diags in
      if total > max_output_lines then
        Printf.bprintf buf "... %d more problems omitted.\n"
          (total - max_output_lines);
      Buffer.contents buf

let diagnostics instance input =
  match Option.bind (Superbol_instance.client instance)
          Vscode_languageclient.LanguageClient.diagnostics with
  | None ->
      Promise.return "The SuperBOL language server is not running."
  | Some collection ->
      Promise.return @@
      match file_path_input input with
      | Some path ->
          (match resolve_path path with
           | Error msg ->
               msg
           | Ok uri ->
               let diags =
                 Vscode.DiagnosticCollection.get collection uri |>
                 Option.value ~default:[]
               in
               render_diagnostics [uri, diags]
                 ~scope:("in " ^ display_path uri))
      | None ->
          let diags = ref [] in
          Vscode.DiagnosticCollection.forEach collection
            ~callback:(fun ~uri ~diagnostics ~collection:_ ->
                diags := (uri, diagnostics) :: !diags);
          render_diagnostics (List.rev !diags) ~scope:"in the workspace"

(* --- Project settings --- *)

let project_settings instance input =
  let** uri =
    Promise.return @@
    match file_path_input input, Vscode.Workspace.workspaceFolders () with
    | None, folder :: _
      when Superbol_instance.current_document_uri () = None ->
        Ok (Vscode.WorkspaceFolder.uri folder)
    | path, _ ->
        target_uri path
  in
  let** json =
    request instance ~meth:"superbol/getProjectConfiguration"
      ~data:(uri_data uri)
  in
  let buf = Buffer.create 1024 in
  Printf.bprintf buf "SuperBOL settings of the project that holds %s:\n"
    (display_path uri);
  Ojs.iter_properties (Jsonoo.t_to_js json) begin fun key ->
    let value = Jsonoo.t_of_js (Ojs.get_prop_ascii (Jsonoo.t_to_js json) key) in
    Printf.bprintf buf "%s: %s\n" key (Jsonoo.stringify value)
  end;
  Promise.return @@ Buffer.contents buf

(* --- Control flow --- *)

type cfg_node =
  {
    id: int;
    node_name: string;     (* qualified by its section, e.g. `P IN S` *)
  }

type cfg_edge =
  {
    source: int;
    target: int;
    kind: string;                     (* "f" (fall-through), "p" or "g" *)
  }

let decode_cfg json =
  let open Jsonoo.Decode in
  let d3 = Jsonoo.try_parse_exn (field "string_repr_d3" string json) in
  let nodes =
    field "nodes" (list (fun j -> {
          id = field "id" int j;
          node_name = field "name" string j;
        })) d3
  and edges =
    field "links" (list (fun j -> {
          source = field "source" int j;
          target = field "target" int j;
          kind = field "type" string j;
        })) d3
  and lines =
    field "nodes_pos" (dict (fun j -> at ["start"; "line"] int j,
                                      at ["end"; "line"] int j)) json
  in
  nodes, edges, lines

let render_cfg ~name ~path (nodes, edges, lines) =
  let line_of n = Hashtbl.find_opt lines (string_of_int n.id) in
  let display n = n.node_name in
  let by_id = Hashtbl.create 17 in
  List.iter (fun n -> Hashtbl.replace by_id n.id n) nodes;
  let compare_nodes a b =
    match line_of a, line_of b with
    | Some l1, Some l2 -> compare l1 l2
    | Some _, None -> 1                 (* entry points and external nodes *)
    | None, Some _ -> -1
    | None, None -> Int.compare a.id b.id
  in
  let targets n kind =
    List.filter_map (fun e ->
        if e.source = n.id && e.kind = kind
        then Hashtbl.find_opt by_id e.target
        else None) edges |>
    List.stable_sort compare_nodes |>
    List.map display
  in
  let nodes = List.stable_sort compare_nodes nodes in
  let buf = Buffer.create 4096 in
  Printf.bprintf buf "Control flow graph of %s in %s.  For each node \
                      (paragraph, section, or entry point): the nodes it \
                      performs (PERFORM), goes to (GO TO), and falls through \
                      to (continuing with the next node in sequence); lines \
                      are 1-based.\n" name path;
  List.iteri begin fun i n ->
    if i < max_output_lines then begin
      Buffer.add_string buf (display n);
      Option.iter (Printf.bprintf buf " %a" pp_lines) (line_of n);
      let clauses =
        List.filter_map (fun (kind, verb) ->
            match targets n kind with
            | [] -> None
            | ts -> Some (verb ^ " " ^ String.concat ", " ts))
          ["p", "performs"; "g", "goes to"; "f", "falls through to"]
      in
      (match clauses with
       | [] -> Buffer.add_string buf ": no successor (returns to its \
                                      performer, or ends the run)"
       | cs -> Printf.bprintf buf ": %s" (String.concat "; " cs));
      Buffer.add_char buf '\n'
    end
  end nodes;
  let total = List.length nodes in
  if total > max_output_lines then
    Printf.bprintf buf "... %d more nodes omitted.\n" (total - max_output_lines);
  Buffer.contents buf

let control_flow instance input =
  let** uri = Promise.return @@ target_uri (file_path_input input) in
  let** () = open_cobol_document uri in
  let** json =
    request instance ~meth:"superbol/getPossibleCFG" ~data:(uri_data uri)
  in
  let names = Jsonoo.Decode.(list string) json in
  let list_names () =
    String.concat "\n" (List.map (fun n -> "- " ^ n) names)
  in
  let program_names =
    List.filter (fun n -> not (String.contains n '(')) names in
  let** name =
    Promise.return @@
    match input_field "name" Jsonoo.Decode.string input, program_names with
    | Some name, _ when List.mem name names ->
        Ok name
    | Some name, _ ->
        Error (Printf.sprintf "No control flow graph named %S in %s; \
                               available graphs are:\n%s"
                 name (display_path uri) (list_names ()))
    | None, [name] ->
        Ok name
    | None, _ ->
        Error (Printf.sprintf "Several control flow graphs are available in \
                               %s; pick one with `name`:\n%s"
                 (display_path uri) (list_names ()))
  in
  let** json =
    request instance ~meth:"superbol/getCFG"
      ~data:(uri_data uri ~extra:["name", Jsonoo.Encode.string name])
  in
  let others = List.filter (( <> ) name) names in
  Promise.return @@
  match decode_cfg json with
  | cfg ->
      render_cfg ~name ~path:(display_path uri) cfg ^
      if others = [] then ""
      else "Other graphs available in the same file (with `name`):\n" ^
           String.concat "\n" (List.map (fun n -> "- " ^ n) others) ^ "\n"
  | exception (Jsonoo.Decode_error msg) ->
      "Unable to read the control flow graph: " ^ msg

(* --- Registration --- *)

let tool ~invocation_message run instance =
  let open Vscode in
  LanguageModelTool.create ()
    ~prepareInvocation:begin fun ~options ~token:_ ->
      let input = LanguageModelToolInvocationPrepareOptions.input options in
      `Value (Some (PreparedToolInvocation.create ()
                     ~invocationMessage:(`String (invocation_message input))))
    end
    ~invoke:begin fun ~options ~token:_ ->
      let input = LanguageModelToolInvocationOptions.input options in
      `Promise (run instance input >>| fun text ->
                Some (LanguageModelToolResult.of_text text))
    end

let file_or_editor input =
  match file_path_input input with
  | Some path -> Filename.basename path
  | None -> "the active editor"

let tools =
  [
    outline_tool_name,
    tool outline
      ~invocation_message:(fun i -> "Outlining " ^ file_or_editor i);
    find_symbol_tool_name,
    tool find_symbol
      ~invocation_message:(fun i ->
          match input_field "query" Jsonoo.Decode.string i with
          | Some q -> Printf.sprintf "Searching COBOL symbols for %S" q
          | None -> "Searching COBOL symbols");
    diagnostics_tool_name,
    tool diagnostics
      ~invocation_message:(fun i ->
          match file_path_input i with
          | Some path -> "Listing problems in " ^ Filename.basename path
          | None -> "Listing COBOL problems");
    project_settings_tool_name,
    tool project_settings
      ~invocation_message:(fun _ -> "Reading SuperBOL project settings");
    control_flow_tool_name,
    tool control_flow
      ~invocation_message:(fun i ->
          "Computing the control flow of " ^
          match input_field "name" Jsonoo.Decode.string i with
          | Some name -> name
          | None -> file_or_editor i);
  ]

let register instance =
  if Vscode.Lm.tools_available () then
    List.iter begin fun (name, tool) ->
      Superbol_instance.subscribe_disposable instance @@
      Vscode.Lm.registerTool ~name ~tool:(tool instance)
    end tools
