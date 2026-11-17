(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Tools for language models (e.g. AI agents): what they are, and their
    outputs, as text meant for language models.  See the interface. *)

open Lsp_imports
open Lsp.Types

module DIAGS = Cobol_common.Diagnostics
module CACHED_DIAGS = Superbol_project.Cached_diagnostics

type tool =
  {
    name: string;
    display_name: string;
    reference_name: string;
    icon: string;
    user_description: string;
    model_description: string;
    input_schema: Yojson.Safe.t;
  }

type result =
  {
    text: string;
    is_error: bool;
  }

(* --- Inputs --- *)

type arguments = (string * Yojson.Safe.t) list

(** Raised with a message for the language model, on invalid inputs or when
    the tool cannot give a result. *)
exception Tool_error of string

let fail fmt = Fmt.kstr (fun msg -> raise (Tool_error msg)) fmt

let string_arg name (args: arguments) =
  match List.assoc_opt name args with
  | None | Some `Null -> None
  | Some (`String s) -> Some s
  | Some _ -> fail "`%s` must be a string." name

let int_arg name (args: arguments) =
  match List.assoc_opt name args with
  | None | Some `Null -> None
  | Some (`Int i) -> Some i
  | Some (`Float f) when Float.is_integer f -> Some (int_of_float f)
  | Some _ -> fail "`%s` must be an integer." name

let file_path_arg = "filePath"

(** URI of the file given as argument, which should be absolute, but may be
    relative to the root directory of the only project. *)
let file_uri registry args =
  match string_arg file_path_arg args with
  | None ->
      None
  | Some path when not (Filename.is_relative path) ->
      Some (Lsp.Uri.of_path path)
  | Some path ->
      match Lsp_project.SET.elements registry.Lsp_server.projects with
      | [project] ->
          let rootdir = Lsp_project.(string_of_rootdir @@ rootdir project) in
          Some (Lsp.Uri.of_path (Filename.concat rootdir path))
      | _ ->
          fail "%S is not an absolute path." path

let required_file_uri registry args =
  match file_uri registry args with
  | Some uri -> uri
  | None -> fail "No file was given (`%s`)." file_path_arg

(* --- Outputs --- *)

(** Path of [uri], relative to the root directory of its project if it is in
    one. *)
let display_path registry uri =
  let path = Lsp_utils.path_of_uri uri in
  match Lsp_project.SET.for_ ~uri registry.Lsp_server.projects with
  | project ->
      let rootdir = Lsp_project.(string_of_rootdir @@ rootdir project) in
      let prefix = Filename.concat rootdir "" in
      if String.starts_with ~prefix path
      then String.sub path (String.length prefix)
          (String.length path - String.length prefix)
      else path
  | exception Not_found ->
      path

(** 1-based line range, from 0-based line numbers. *)
let pp_lines ppf (first, last) =
  if first = last
  then Fmt.pf ppf "[L%d]" (first + 1)
  else Fmt.pf ppf "[L%d-%d]" (first + 1) (last + 1)

let range_lines (r: Range.t) = r.start.line, r.end_.line

(** Maximum number of lines in outputs, so as to spare the context of language
    models on very large programs or workspaces. *)
let max_output_lines = 2000

let plural n word = if n = 1 then word else word ^ "s"

(* --- Documents --- *)

(** The open document at [uri], analyzed.  Documents are looked up by their
    canonical paths, as the client may spell their URIs differently. *)
let checked_doc registry uri =
  let path = Superbol_cache.Witness.canonical_path @@ Lsp_utils.path_of_uri uri in
  let doc =
    URIMap.fold begin fun doc_uri doc found ->
      match found with
      | Some _ -> found
      | None ->
          if Superbol_cache.Witness.canonical_path
              (Lsp_utils.path_of_uri doc_uri) = path
          then Some doc else None
    end registry.Lsp_server.docs None
  in
  match doc with
  | None ->
      fail "%s is not open in the editor." (display_path registry uri)
  | Some doc ->
      match Lsp_document.checked doc with
      | checked -> doc, checked
      | exception Lsp_document.Copybook _ ->
          fail "%s is a copybook: it is only analyzed as part of the programs \
                that include it." (display_path registry uri)
      | exception Lsp_document.Unparseable _ ->
          fail "%s could not be parsed." (display_path registry uri)

(* --- Outline --- *)

let rec count_symbols symbols =
  List.fold_left (fun n (s: DocumentSymbol.t) ->
      n + 1 + count_symbols (Option.value ~default:[] s.children)) 0 symbols

let outline registry args =
  let uri = required_file_uri registry args in
  let max_depth = int_arg "maxDepth" args in
  let doc, { Cobol_typeck.Outputs.ptree; _ } = checked_doc registry uri in
  let path = display_path registry uri in
  match Lsp_document_symbol.from_ptree_at ~uri:(Lsp_document.uri doc) ptree with
  | [] ->
      Fmt.str "No symbols found in %s." path
  | symbols ->
      let buf = Buffer.create 4096 in
      let ppf = Fmt.with_buffer buf in
      let lines = ref 0 and omitted = ref 0 in
      Fmt.pf ppf "Outline of %s (lines are 1-based; indentation shows \
                  nesting):@\n" path;
      let rec render depth (s: DocumentSymbol.t) =
        let children = Option.value ~default:[] s.children in
        if !lines >= max_output_lines then
          omitted := !omitted + 1 + count_symbols children
        else begin
          incr lines;
          Fmt.pf ppf "%s%s  %a" (String.make (2 * depth) ' ') s.name
            pp_lines (range_lines s.range);
          match max_depth, children with
          | Some d, (_ :: _) when depth + 1 >= d ->
              let n = count_symbols children in
              Fmt.pf ppf "  (%d nested %s hidden)@\n" n (plural n "symbol")
          | _ ->
              Fmt.pf ppf "@\n";
              List.iter (render (depth + 1)) children
        end
      in
      List.iter (render 0) symbols;
      if !omitted > 0 then
        Fmt.pf ppf "... %d more symbols omitted; use `maxDepth` for a \
                    shallower outline.@\n" !omitted;
      Fmt.flush ppf ();
      Buffer.contents buf

(* --- Symbol search --- *)

(* See [Lsp_workspace_symbols] for the kinds of symbols. *)
let describe_symbol_kind (s: SymbolInformation.t) =
  match s.kind, s.containerName with
  | Module, _ -> "program"
  | Namespace, _ -> "section"
  | Class, _ -> "class"
  | Method, _ -> "method"
  | Interface, _ -> "interface"
  | Function, Some _ -> "paragraph"
  | Function, None -> "function"
  | (Field | Variable), _ -> "data item"
  | Constant, _ -> "constant"
  | EnumMember, _ -> "condition-name"
  | _ -> "symbol"

let default_symbol_limit = 50

let find_symbol registry args =
  let query = match string_arg "query" args with
    | None | Some "" -> fail "No query was given."
    | Some q -> q
  in
  let limit =
    Option.value ~default:default_symbol_limit (int_arg "limit" args) |> max 1
  in
  (* Ask for one more, to tell whether some were left out. *)
  match Lsp_server.workspace_symbols registry ~query ~limit:(limit + 1) with
  | [] ->
      Fmt.str "No symbol matches %S." query
  | symbols ->
      let buf = Buffer.create 4096 in
      let ppf = Fmt.with_buffer buf in
      Fmt.pf ppf "Symbols matching %S, best matches first (lines are \
                  1-based):@\n" query;
      List.iteri begin fun i (s: SymbolInformation.t) ->
        if i < limit then
          Fmt.pf ppf "%s %s%a: %s %a@\n" (describe_symbol_kind s) s.name
            Fmt.(option (any " (in " ++ string ++ any ")")) s.containerName
            (display_path registry s.location.uri)
            pp_lines (range_lines s.location.range)
      end symbols;
      if List.length symbols > limit then
        Fmt.pf ppf "... more matches omitted; refine the query, or raise \
                    `limit`.@\n";
      Fmt.flush ppf ();
      Buffer.contents buf

(* --- Diagnostics --- *)

(** Diagnostics of every file, as the server reports them: those of open
    documents, and the cached ones of other source files.  Copybooks show the
    diagnostics that the programs that include them report. *)
let all_diagnostics registry : Diagnostic.t list URIMap.t =
  let union = URIMap.union (fun _ a b -> Some (List.rev_append a b)) in
  let translate ~project ~uri diags =
    let rootdir = Lsp_project.(string_of_rootdir @@ rootdir project) in
    Lsp_diagnostics.translate diags ~uri ~rootdir ~focus_on_main_doc:false
  in
  let of_open_docs =
    URIMap.fold begin fun _ (Lsp_document.{ project; _ } as doc) acc ->
      union acc @@
      translate ~project ~uri:(Lsp_document.uri doc)
        (Lsp_document.diagnostics doc)
    end registry.Lsp_server.docs URIMap.empty
  in
  let is_open = Lsp_server.is_open_file registry in
  Lsp_project.SET.fold begin fun project acc ->
    Superbol_project.Walk.source_files project ~platform:Lsp_platform.record
      ~layout:registry.params.config.lsp_project_layout |>
    List.filter (fun f -> not (is_open f)) |>
    Lsp_project_cache.cached_diagnostics ~params:registry.params project |>
    List.fold_left begin fun acc -> function
      | _, None ->
          acc
      | filename, Some cached ->
          union acc @@
          translate ~project ~uri:(Lsp.Uri.of_path filename)
            (CACHED_DIAGS.diagnostics cached)
    end acc
  end registry.projects of_open_docs |>
  (* A copybook included by several programs may get a same diagnostic from
     each of them. *)
  URIMap.map (List.sort_uniq Stdlib.compare)

let severity_rank : DiagnosticSeverity.t option -> int = function
  | Some Error -> 0 | Some Warning -> 1
  | Some Information -> 2 | Some Hint | None -> 3

let severity_name : DiagnosticSeverity.t option -> string = function
  | Some Error -> "error" | Some Warning -> "warning"
  | Some Information -> "info" | Some Hint | None -> "hint"

let diagnostics registry args =
  let all = all_diagnostics registry in
  let scope, diags =
    match file_uri registry args with
    | None ->
        "in the workspace", URIMap.bindings all
    | Some uri ->
        let uri = Lsp_diagnostics.normalize_uri uri in
        "in " ^ display_path registry uri,
        [uri, Option.value ~default:[] (URIMap.find_opt uri all)]
  in
  let diags =
    List.concat_map (fun (uri, ds) ->
        let path = display_path registry uri in
        List.map (fun d -> path, d) ds) diags |>
    List.stable_sort begin fun (p1, (d1: Diagnostic.t)) (p2, d2) ->
      Stdlib.compare
        (severity_rank d1.severity, p1, d1.range.start.line)
        (severity_rank d2.severity, p2, d2.range.start.line)
    end
  in
  match diags with
  | [] ->
      Fmt.str "SuperBOL reports no problems %s." scope
  | diags ->
      let count sev =
        List.length (List.filter (fun (_, (d: Diagnostic.t)) ->
            d.severity = Some sev) diags) in
      let buf = Buffer.create 4096 in
      let ppf = Fmt.with_buffer buf in
      let errors = count Error and warnings = count Warning in
      Fmt.pf ppf "SuperBOL reports %d %s and %d %s %s (lines and columns are \
                  1-based):@\n" errors (plural errors "error")
        warnings (plural warnings "warning") scope;
      List.iteri begin fun i (path, (d: Diagnostic.t)) ->
        if i < max_output_lines then
          Fmt.pf ppf "%s:%d:%d: %s: %s@\n" path
            (d.range.start.line + 1) (d.range.start.character + 1)
            (severity_name d.severity)
            (String.concat " " @@ String.split_on_char '\n' d.message)
      end diags;
      let total = List.length diags in
      if total > max_output_lines then
        Fmt.pf ppf "... %d more problems omitted.@\n"
          (total - max_output_lines);
      Fmt.flush ppf ();
      Buffer.contents buf

(* --- Project settings --- *)

let project_settings registry args =
  let uri, config =
    match file_uri registry args,
          Lsp_project.SET.elements registry.Lsp_server.projects with
    | Some uri, _ ->
        uri, Lsp_server.get_project_config_command uri registry
    | None, project :: _ ->
        let rootdir = Lsp_project.(string_of_rootdir @@ rootdir project) in
        Lsp.Uri.of_path rootdir, Lsp_project.get_project_config project
    | None, [] ->
        fail "No file was given (`%s`), and there is no project." file_path_arg
  in
  let buf = Buffer.create 1024 in
  let ppf = Fmt.with_buffer buf in
  Fmt.pf ppf "SuperBOL settings of the project that holds %s:@\n"
    (display_path registry uri);
  (match config with
   | `Assoc fields ->
       List.iter (fun (key, value) ->
           Fmt.pf ppf "%s: %s@\n" key (Yojson.Safe.to_string value)) fields
   | json ->
       Fmt.pf ppf "%s@\n" (Yojson.Safe.to_string json));
  Fmt.flush ppf ();
  Buffer.contents buf

(* --- Control flow --- *)

let render_cfg ~name ~path ~filename cfg =
  let open Cobol_cfg.Types in
  let node_name n =
    match n.typ with
    | Normal (_, name) | External name | Split name
    | Entry (`Statement name | `Section name) -> name
    | Collapsed names -> String.concat ", " (NEL.to_list names)
    | Entry `Point -> "Entry point"
    | Entry `Paragraph -> "Entry paragraph"
  in
  let lines n =
    Option.map (fun loc ->
        range_lines (Lsp_position.range_of_srcloc_in ~filename loc)) n.loc
  in
  (* Nodes without location (entry points, external nodes) come first. *)
  let compare_nodes a b =
    Stdlib.compare (lines a, a.id) (lines b, b.id)
  in
  let targets n kind =
    CFG.fold_succ_e (fun (_, e, t) acc -> if e = kind then t :: acc else acc)
      cfg n [] |>
    List.sort_uniq compare_nodes |>
    List.map node_name
  in
  let nodes =
    CFG.fold_vertex List.cons cfg [] |> List.sort compare_nodes in
  let buf = Buffer.create 4096 in
  let ppf = Fmt.with_buffer buf in
  Fmt.pf ppf "Control flow graph of %s in %s.  For each node (paragraph, \
              section, or entry point): the nodes it performs (PERFORM), goes \
              to (GO TO), and falls through to (continuing with the next node \
              in sequence); lines are 1-based.@\n" name path;
  List.iteri begin fun i n ->
    if i < max_output_lines then begin
      Fmt.pf ppf "%s%a" (node_name n)
        Fmt.(option (any " " ++ pp_lines)) (lines n);
      let clauses =
        List.filter_map (fun (kind, verb) ->
            match targets n kind with
            | [] -> None
            | ts -> Some (verb ^ " " ^ String.concat ", " ts))
          [Perform, "performs"; Go, "goes to"; FallThrough, "falls through to"]
      in
      (match clauses with
       | [] -> Fmt.pf ppf ": no successor (returns to its performer, or ends \
                          the run)"
       | cs -> Fmt.pf ppf ": %s" (String.concat "; " cs));
      Fmt.pf ppf "@\n"
    end
  end nodes;
  let total = List.length nodes in
  if total > max_output_lines then
    Fmt.pf ppf "... %d more nodes omitted.@\n" (total - max_output_lines);
  Fmt.flush ppf ();
  Buffer.contents buf

let control_flow registry args =
  let uri = required_file_uri registry args in
  let doc, checked = checked_doc registry uri in
  let path = display_path registry uri in
  let names = Cobol_cfg.Builder.possible_cfgs_of_doc checked in
  let list_names names =
    String.concat "\n" (List.map (fun n -> "- " ^ n) names) in
  let program_names =
    List.filter (fun n -> not (String.contains n '(')) names in
  let name =
    match string_arg "name" args, program_names with
    | Some name, _ when List.mem name names ->
        name
    | Some name, _ ->
        fail "No control flow graph named %S in %s; available graphs \
              are:\n%s" name path (list_names names)
    | None, [name] ->
        name
    | None, _ ->
        fail "Several control flow graphs are available in %s; pick one \
              with `name`:\n%s" path (list_names names)
  in
  let cfg, _ =
    Cobol_cfg.Builder.make ~name checked
      ~options:(Lsp_cfg.cfg_options_of_json [])
  in
  let filename = Lsp_utils.path_of_uri (Lsp_document.uri doc) in
  render_cfg ~name ~path ~filename cfg ^
  match List.filter (( <> ) name) names with
  | [] -> ""
  | others ->
      "Other graphs available in the same file (with `name`):\n" ^
      list_names others ^ "\n"

(* --- Definitions --- *)

let schema ?(required = []) properties : Yojson.Safe.t =
  `Assoc ([
      "type", `String "object";
      "properties", `Assoc (List.map (fun (name, typ, description) ->
          name, `Assoc ["type", `String typ;
                        "description", `String description]) properties);
    ] @ match required with
    | [] -> []
    | r -> ["required", `List (List.map (fun s -> `String s) r)])

let file_path_property ~default =
  file_path_arg, "string",
  "Absolute path of the COBOL source file. Defaults to " ^ default ^ "."

let definitions = [
  {
    name = "superbol_cobol_outline";
    display_name = "COBOL Outline";
    reference_name = "cobolOutline";
    icon = "$(list-tree)";
    user_description = "Outline of a COBOL program: its divisions, sections, \
                        paragraphs, and data items, with their line ranges.";
    model_description = "Get the structure of a COBOL source file, as \
                         computed by the SuperBOL language server: programs \
                         (and classes, methods, functions), divisions, \
                         sections, data items with their level numbers (01, \
                         05, 77, 88, ...) and nesting, and procedure sections \
                         and paragraphs, each with its 1-based line range.  \
                         Use it to find your way in a COBOL program before \
                         reading parts of it, which is much cheaper than \
                         reading the whole file, as COBOL programs are often \
                         very long.  Data item types (PICTURE, USAGE) and \
                         statements are not included.";
    input_schema = schema [
        file_path_property ~default:"the file shown in the active editor";
        "maxDepth", "integer",
        "Maximum nesting depth of the symbols shown (programs are at depth \
         1, divisions at depth 2, sections at depth 3); deeper symbols are \
         counted but not listed.  Use 3 or 4 for a quick overview of large \
         programs.";
      ];
  };
  {
    name = "superbol_cobol_find_symbol";
    display_name = "Find COBOL Symbol";
    reference_name = "cobolSymbol";
    icon = "$(symbol-field)";
    user_description = "Find COBOL programs, sections, paragraphs, and data \
                        items by name, across the workspace.";
    model_description = "Search the COBOL symbols of the workspace by name: \
                         programs, sections, paragraphs, data items, \
                         condition-names (level 88), and constants.  Returns, \
                         best matches first, the kind of each symbol, its \
                         name, its enclosing program or section, and its file \
                         and 1-based line range.  Use it to find which file \
                         defines a program or paragraph, e.g. the target of a \
                         CALL or PERFORM, instead of searching files.  \
                         Matching ignores case and treats `-` and `_` alike, \
                         and the letters of the query only need to appear in \
                         order (e.g. `CALCTX` matches `CALC-TAX`).  Only files \
                         that are open or that SuperBOL has already analyzed \
                         are searched.";
    input_schema = schema ~required:["query"] [
        "query", "string",
        "Name, or part of a name, of the symbols to look for.";
        "limit", "integer",
        "Maximum number of results (50 by default).";
      ];
  };
  {
    name = "superbol_cobol_diagnostics";
    display_name = "COBOL Problems";
    reference_name = "cobolProblems";
    icon = "$(warning)";
    user_description = "Errors and warnings that SuperBOL reports on COBOL \
                        files.";
    model_description = "List the errors and warnings that the SuperBOL \
                         language server reports on COBOL source files \
                         (syntax errors, unknown or ambiguous names, invalid \
                         clauses, missing copybooks, ...), with their 1-based \
                         line and column.  Use it after editing COBOL code to \
                         check that it is still valid, or to find what to \
                         fix.  Without a file, it lists the problems of every \
                         COBOL file of the workspace that SuperBOL has \
                         analyzed.";
    input_schema = schema [
        file_path_property ~default:"every analyzed file of the workspace";
      ];
  };
  {
    name = "superbol_cobol_project_settings";
    display_name = "COBOL Project Settings";
    reference_name = "cobolSettings";
    icon = "$(settings-gear)";
    user_description = "SuperBOL settings of a COBOL project: dialect, source \
                        format, copybook paths.";
    model_description = "Get the SuperBOL settings of the project that holds \
                         a COBOL file: COBOL dialect, source format (e.g. \
                         `fixed`, with code in columns 8-72, or `free`), \
                         source formats by file extension, directories and \
                         extensions used to find copybooks, and excluded \
                         directories.  Use it before writing COBOL code, so \
                         that it follows the expected format and dialect, or \
                         to find where the copybooks of a program are.";
    input_schema = schema [
        file_path_property ~default:"the file shown in the active editor, or \
                                    else the first workspace folder";
      ];
  };
  {
    name = "superbol_cobol_control_flow";
    display_name = "COBOL Control Flow";
    reference_name = "cobolControlFlow";
    icon = "$(type-hierarchy)";
    user_description = "Control flow between the paragraphs and sections of \
                        a COBOL program.";
    model_description = "Get the control flow graph of a COBOL program, or \
                         of one of its sections, between paragraphs and \
                         sections: for each one, which paragraphs or sections \
                         it PERFORMs, which it reaches with GO TO, and which \
                         it falls through to, with 1-based line ranges.  Use \
                         it to understand the overall logic of a program, or \
                         what may run before or after a given paragraph, \
                         without reading every statement.  When a file holds \
                         several programs, or to get the graph of one \
                         section, the tool lists the graph names that `name` \
                         accepts.";
    input_schema = schema [
        file_path_property ~default:"the file shown in the active editor";
        "name", "string",
        "Name of the graph: a program name, or a section name followed by \
         its program name in parentheses, e.g. `MAIN-SECTION (PAYROLL)`.  \
         Defaults to the program of the file, when it holds only one.";
      ];
  };
]

let implementations = [
  "superbol_cobol_outline", outline;
  "superbol_cobol_find_symbol", find_symbol;
  "superbol_cobol_diagnostics", diagnostics;
  "superbol_cobol_project_settings", project_settings;
  "superbol_cobol_control_flow", control_flow;
]

let call registry ~name ~arguments =
  match List.assoc_opt name implementations with
  | None ->
      { text = Fmt.str "Unknown tool: %s" name; is_error = true }
  | Some f ->
      match f registry arguments with
      | text ->
          { text; is_error = false }
      | exception Tool_error text ->
          { text; is_error = true }
      | exception (Jsonrpc.Response.Error.E { message; _ }) ->
          { text = message; is_error = true }
      | exception e ->
          Lsp_io.log_error "Tool %s failed: %a" name Fmt.exn e;
          { text = Fmt.str "Internal error of tool %s." name; is_error = true }
