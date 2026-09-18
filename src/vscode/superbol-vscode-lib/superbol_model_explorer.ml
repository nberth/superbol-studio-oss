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

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  Fun.protect (fun () -> really_input_string ch (in_channel_length ch))
    ~finally: (fun () -> close_in ch)

(* DEMO DATA *)
(* TODO(model-explorer): replace with a real graph obtained from the LSP
   server, once a request analogous to [superbol/getCFG] exists to produce
   [Model_explorer.GraphCollection.t] from an actual COBOL program. *)

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
(* Unlike [Superbol_cfg_explorer], there is a single panel: the demo graph
   isn't tied to any particular COBOL source file. *)

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

let open_model_explorer _instance =
  let extension_uri =
    VS.ExtensionContext.extensionUri (Superbol_instance.context _instance)
  in
  match get_html_template ~extension_uri with
  | Error e ->
      let _ : _ option Promise.t =
        VS.Window.showErrorMessage
          ~message:("Unable to display Model Explorer: " ^ e) ()
      in Promise.return ()
  | Ok html_template ->
      let graph_collections = demo_graph_collections () in
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
      else post_graph_collections webview graph_collections;
      Promise.return ()
