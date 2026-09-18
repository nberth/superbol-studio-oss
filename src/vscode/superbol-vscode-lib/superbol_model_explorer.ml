(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module VS = Vscode
module ME = Model_explorer
module Dot = Superbol_dot

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  Fun.protect (fun () -> really_input_string ch (in_channel_length ch))
    ~finally: (fun () -> close_in ch)

(* DEMO DATA *)
(* Used as a fallback by [open_model_explorer] when there is no active COBOL
   editor to pull a real graph from -- see the "DOT -> MODEL EXPLORER
   CONVERSION" and "GRAPH FROM LSP" sections below for the real path, which
   mirrors [Superbol_cfg_explorer]'s [superbol/getPossibleCFG] /
   [superbol/getCFG] request pair. *)

let demo_graph_collections () =
  let node ?(incomingEdges = []) ~id ~label () =
    ME.GraphNode.make ~id ~label ~namespace:"" ~incomingEdges ()
  in
  let edge_from sourceNodeId =
    ME.IncomingEdge.make ~sourceNodeId ~sourceNodeOutputId:"0"
      ~targetNodeInputId:"0" ()
  in
  let nodes =
    [ node ~id:"main" ~label:"MAIN" ()
    ; node ~id:"para-a" ~label:"PARA-A" ~incomingEdges:[edge_from "main"] ()
    ; node ~id:"para-b" ~label:"PARA-B" ~incomingEdges:[edge_from "para-a"] ()
    ]
  in
  [ ME.GraphCollection.make ~label:"Demo"
      ~graphs:[ME.Graph.make ~id:"demo" ~nodes ()] () ]

(* DOT -> MODEL EXPLORER CONVERSION *)
(* [Lsp_cfg.to_dot_string] (see [src/lsp/cobol_lsp/lsp_cfg.ml]) renders a
   COBOL CFG as a graphviz dot string, using [ocamlgraph]'s
   [Graph.Graphviz.Dot] functor; [Superbol_dot] parses the (flat,
   subgraph-free) subset of dot that printer emits, and the functions below
   turn the result into the [Model_explorer.GraphCollection.t] shape the
   visualizer expects. *)

(* Turns the record-shaped label (e.g. ["{PARA-A|PARA-B|PARA-C}"]) that
   [Lsp_cfg]'s dot printer emits for a [Collapsed] CFG node into a plain,
   multi-line label -- the model-explorer visualizer has no notion of dot's
   record shapes or ports. *)
let plain_label_of_dot_label label =
  let len = String.length label in
  if len >= 2 && label.[0] = '{' && label.[len - 1] = '}'
  then String.sub label 1 (len - 2) |> String.map (function '|' -> '\n' | c -> c)
  else label

(* All dot attributes but [label] are surfaced as-is (e.g. ["shape"],
   ["style"]) as node attributes, so they remain visible (in the info panel)
   even though the visualizer doesn't interpret them the way dot would. *)
let node_attrs attrs =
  List.filter_map
    (fun (key, value) ->
       if String.equal key "label" then None
       else Some (ME.NodeAttribute.make ~key ~value:(`String value)))
    attrs

let edge_metadata = function
  | [] -> None
  | attrs -> Some (ME.Dict.of_alist attrs)

(* Model-explorer graphs record edges as the [incomingEdges] of their target
   node, unlike dot's flat edge-statement list: index dot edges by their
   destination first. *)
let incoming_edges_by_dst (dot : Dot.t) =
  let table = Hashtbl.create 16 in
  List.iter
    (fun { Dot.src; dst; attrs } ->
       let edge =
         ME.IncomingEdge.make ~sourceNodeId:src ~sourceNodeOutputId:"0"
           ~targetNodeInputId:"0" ?metadata:(edge_metadata attrs) ()
       in
       Hashtbl.replace table dst
         (edge :: Option.value ~default:[] (Hashtbl.find_opt table dst)))
    dot.edges;
  table

let graph_node_of_dot_node ~incoming_edges ({ id; attrs } : Dot.node) =
  let label =
    match List.assoc_opt "label" attrs with
    | Some label -> plain_label_of_dot_label label
    | None -> id
  in
  let incomingEdges =
    List.rev (Option.value ~default:[] (Hashtbl.find_opt incoming_edges id))
  in
  ME.GraphNode.make ~id ~label ~namespace:"" ~attrs:(node_attrs attrs)
    ~incomingEdges ()

let graph_of_dot ~id (dot : Dot.t) =
  let incoming_edges = incoming_edges_by_dst dot in
  ME.Graph.make ~id
    ~nodes:(List.map (graph_node_of_dot_node ~incoming_edges) dot.nodes) ()

(* [Graph.Dot.parse_dot_ast] (which {!Superbol_dot.parse_file} wraps) only
   reads from disk, so a dot string obtained over the LSP (as
   [string_repr_dot]) is written to a scratch file, under the extension's own
   storage directory, before being parsed. *)
let write_temp_dot_file ~context dot_string =
  let dir = VS.Uri.fsPath (VS.ExtensionContext.globalStorageUri context) in
  Node.Fs.mkdirSync dir ~recursive:true;
  let path = Node.Path.join [dir; "model-explorer-cfg.dot"] in
  Node.Fs.writeFileSync path dot_string;
  path

let graph_collections_of_dot ~context ~label ~id dot_string =
  let path = write_temp_dot_file ~context dot_string in
  Fun.protect
    ~finally:(fun () -> try Node.Fs.unlinkSync path with _ -> ())
    (fun () ->
       [ME.GraphCollection.make ~label
          ~graphs:[graph_of_dot ~id (Dot.parse_file path)] ()])

(* HTML / VENDORED ASSETS *)

let html_template = ref None

let get_html_template ~extension_uri =
  match !html_template with
  | Some html -> Ok html
  | None ->
      let html_uri =
        VS.Uri.joinPath extension_uri
          ~pathSegments:["assets"; "model-explorer-renderer.html"]
      in
      (try
         let html = read_whole_file (VS.Uri.fsPath html_uri) in
         html_template := Some html;
         Ok html
       with Sys_error e -> Error e
          | End_of_file -> Error "End_of_file")

let vendored ~extension_uri segments =
  VS.Uri.joinPath extension_uri
    ~pathSegments:(["assets"; "vendor"; "model-explorer"] @ segments)

let build_html ~webview ~extension_uri html_template =
  let webview_uri uri =
    VS.Uri.toString (VS.WebView.asWebviewUri webview ~localResource:uri) ()
  in
  let worker_js =
    Js_escaper.js_string_literal_for_html @@
    read_whole_file @@ VS.Uri.fsPath @@ vendored ~extension_uri ["worker.js"]
  in
  Printf.sprintf
    "%s\n\
     <script src=\"%s\"></script>\n\
     <script>
       window.modelExplorer = {};
       window.modelExplorer.assetFilesBaseUrl = \"%s\";
       //fetch (\"%s\")
       //   .then(result => result.blob())
       //   .then(result => {
       //      window.modelExplorer.workerScriptPath = URL.createObjectURL(result);
       //   });
       const worker_blob = new Blob([\"%s\"], { type: 'text/plain' });
       window.modelExplorer.workerScriptPath = URL.createObjectURL(worker_blob);
     </script>\n\
     <script src=\"%s\"></script>\n"
    html_template
    (* Superbol_model_explorer_worker.js *)
    (webview_uri (vendored ~extension_uri ["main_browser.js"]))
    (webview_uri (vendored ~extension_uri ["static_files"]))
    (webview_uri (vendored ~extension_uri ["worker.js"]))
    worker_js
    (* modelExplorer_config *)
    (webview_uri
       (VS.Uri.joinPath extension_uri ~pathSegments:["assets"; "model-explorer.js"]))

(* WEBVIEW MANAGEMENT *)
(* Unlike [Superbol_cfg_explorer], there is a single panel: the graph shown
   isn't tied to any particular COBOL source file, only to whichever program
   was last picked (or the demo graph, absent one). *)

type stored_data =
  { webview_panel: VS.WebviewPanel.t;
    graph_collections: ME.GraphCollection.t list; }

let panel : stored_data option ref = ref None

let post_graph_collections webview graph_collections =
  let ojs = Ojs.empty_obj () in
  Ojs.set_prop_ascii ojs "type" (Ojs.string_to_js "set_graph_collections");
  Ojs.set_prop_ascii ojs "graphCollections"
    ([%js.of: ME.GraphCollection.t list] graph_collections);
  let _ : bool Promise.t = VS.WebView.postMessage webview ojs in
  ()

let on_message ~graph_collections webview arg =
  match Ojs.get_prop_ascii arg "type" |> Ojs.string_of_js with
  | "ready" ->
      post_graph_collections webview graph_collections
  | "node_selected" | "node_double_clicked" | "model_graph_processed" ->
      Superbol_printer.log_error "Node interactions!";
      (* TODO(model-explorer): react to node selection/hovering, e.g. by
         revealing the corresponding COBOL source location once a graph node
         carries one (see [Superbol_cfg_explorer.on_click] for the pattern). *)
      ()
  | _ ->
      ()

let create_or_reveal ~extension_uri =
  match !panel with
  | Some { webview_panel; _ } ->
      VS.WebviewPanel.reveal webview_panel ();
      webview_panel, false
  | None ->
      let webview_panel =
        VS.Window.createWebviewPanel ~viewType:"superbol.modelExplorer"
          ~title:"SuperBOL Model Explorer" ~showOptions:VS.ViewColumn.Beside
      in
      let _ : VS.Disposable.t =
        VS.WebviewPanel.onDidDispose webview_panel ()
          ~listener:(fun () -> panel := None)
          ~thisArgs:Ojs.null ~disposables:[]
      in
      let webview = VS.WebviewPanel.webview webview_panel in
      VS.WebView.set_options webview @@
      VS.WebviewOptions.create ~enableScripts:true ()
        ~localResourceRoots:[VS.Uri.joinPath extension_uri
                               ~pathSegments:["assets"]];
      webview_panel, true

let open_with_graph_collections ~extension_uri html_template graph_collections =
  let webview_panel, is_new = create_or_reveal ~extension_uri in
  let webview = VS.WebviewPanel.webview webview_panel in
  panel := Some { webview_panel; graph_collections };
  let _ : VS.Disposable.t =
    VS.WebView.onDidReceiveMessage webview ()
      ~listener:(on_message ~graph_collections webview)
      ~thisArgs:Ojs.null ~disposables:[]
  in
  if is_new
  then VS.WebView.set_html webview
      (build_html ~webview ~extension_uri html_template)
  else post_graph_collections webview graph_collections

(* GRAPH FROM LSP *)
(* Mirrors [Superbol_cfg_explorer]'s [superbol/getPossibleCFG] /
   [superbol/getCFG] request pair, using [Superbol_instance.lsp_request]
   (which reports [Client_not_running] as an [Error], rather than letting the
   request promise reject, when there is no LSP client running). *)

let get_possible_cfg_names instance ~uri =
  Superbol_instance.lsp_request instance ~meth:"superbol/getPossibleCFG"
    ~data:Jsonoo.Encode.(object_ ["uri", string (VS.Uri.toString uri ())])
  |> Promise.Result.map Jsonoo.Decode.(list string)

let get_cfg_dot instance ~uri ~name =
  Superbol_instance.lsp_request instance ~meth:"superbol/getCFG"
    ~data:Jsonoo.Encode.(object_
        [ "uri", string (VS.Uri.toString uri ());
          "name", string name ])
  |> Promise.Result.map Jsonoo.Decode.(field "string_repr_dot" string)

let open_model_explorer instance =
  let extension_uri =
    VS.ExtensionContext.extensionUri (Superbol_instance.context instance)
  in
  match get_html_template ~extension_uri with
  | Error e ->
      let _ : _ option Promise.t =
        VS.Window.showErrorMessage
          ~message:("Unable to display Model Explorer: " ^ e) ()
      in Promise.return ()
  | Ok html_template ->
      let show graph_collections =
        open_with_graph_collections ~extension_uri html_template graph_collections;
        Promise.return ()
      in
      match Superbol_instance.current_document_uri () with
      | None ->
          (* Nothing to pull a real graph from: fall back to the demo one, so
             the panel still shows something meaningful. *)
          show (demo_graph_collections ())
      | Some uri ->
          get_possible_cfg_names instance ~uri |>
          Promise.then_ ~fulfilled:begin function
            | Error _ -> show (demo_graph_collections ())
            | Ok names ->
                VS.Window.showQuickPick ~items:names () |>
                Promise.then_ ~fulfilled:begin function
                  | None -> Promise.return ()
                  | Some name ->
                      get_cfg_dot instance ~uri ~name |>
                      Promise.then_ ~fulfilled:begin function
                        | Error error ->
                            Superbol_printer.show_error_message (Error error)
                        | Ok dot_string ->
                            let context = Superbol_instance.context instance in
                            match
                              graph_collections_of_dot
                                ~context ~label:name ~id:name dot_string
                            with
                            | graph_collections -> show graph_collections
                            | exception Superbol_dot.Parse_error message ->
                                let _ : _ option Promise.t =
                                  VS.Window.showErrorMessage
                                    ~message:("Unable to render Model Explorer \
                                               graph: " ^ message) ()
                                in Promise.return ()
                      end
                end
          end
