(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the ISC license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Webview entry point driving a [Model_explorer.Visualizer.t]. Compiled to
    JS ([model_explorer_webview.bc.js]) and meant to be loaded, as a plain
    [<script>] tag, by an HTML page served inside a VS Code webview -- the
    same role [assets/cfg-dot.js] plays for the CFG explorer (see
    [Superbol_cfg_explorer]), and the same [postMessage] bridge pattern.

    This module only scaffolds that bridge: it wires up the element, forwards
    a handful of events, and accepts a couple of message types from the
    extension host. Hooking it into an actual HTML page (vendoring
    "ai-edge-model-explorer-visualizer", its [worker.js]/[static_files/], and
    building a [model-explorer-renderer.html] alongside a
    [Superbol_model_explorer] extension-host module) is a separate,
    follow-up step -- mirroring how [Superbol_cfg_explorer] drives
    [assets/cfg-dot-renderer.html]. *)

module ME = Model_explorer

(** {2 Minimal VS Code webview messaging}

    Just enough to call [acquireVsCodeApi()] and exchange JSON-like messages
    with the extension host -- not a general binding, since nothing else here
    needs the rest of the DOM/webview surface. *)
module Vscode_webview_api = struct
  include [%js: val acquireVsCodeApi : unit -> Ojs.t [@@js.global "acquireVsCodeApi"]]

  let api = lazy (acquireVsCodeApi ())

  let post_message msg =
    ignore (Ojs.call (Lazy.force api) "postMessage" [| msg |] : Ojs.t)

  let send ~type_ fields =
    post_message (Ojs.obj (Array.of_list (("type", Ojs.string_to_js type_) :: fields)))

  let on_message f =
    let window = Ojs.variable "window" in
    let listener =
      [%js.of: Ojs.t -> unit] (fun event -> f (Ojs.get_prop_ascii event "data"))
    in
    ignore
      (Ojs.call window "addEventListener"
         [| Ojs.string_to_js "message"; listener |]
        : Ojs.t)
end

let document_body () = Ojs.get_prop_ascii (Ojs.variable "document") "body"

(** {2 Visualizer configuration}

    The visualizer's own vocabulary ("op", "layer", "inputs"/"outputs") comes
    from its neural-network-model origins and doesn't fit a COBOL CFG: "op"
    nodes and "layer" (group) nodes are renamed to something CFG-appropriate,
    and the inputs/outputs sections are hidden outright, since
    {!Superbol_model_explorer}'s dot-to-graph converter never populates
    per-node port metadata. *)
let visualizer_config () =
  let legendConfig =
    ME.LegendConfig.make ()
      ~renameOpTo:"SECTION"
      ~renameLayerTo:"SECTION OR PARAGRAPH"
      ~hideInputs:true
      ~hideOutputs:true
  in
  let viewOnNodeConfig =
    ME.ViewOnNodeConfig.make ()
      ~renameOpNodeIdTo:"SECTION NAME"
      ~renameOpNodeAttributesTo:"Attributes"
      ~hideOpNodeInputs:true
      ~hideOpNodeOutputs:true
      ~hideOpNodeAttributes:true
      ~hideLayerNodeAttributes:true
  in
  ME.VisualizerConfig.make ~legendConfig ~viewOnNodeConfig ()
    ~renameNodeInfoOpNameTo:"SECTION"
    ~hideInfoPanel:true

(** {2 Outgoing messages} (webview -> extension host) *)

let send_ready () =
  Vscode_webview_api.send ~type_:"ready" []

let send_node_info ~type_ (info : ME.NodeInfo.t) =
  Vscode_webview_api.send ~type_
    [ ("nodeId", Ojs.string_to_js (ME.NodeInfo.nodeId info))
    ; ("graphId", Ojs.string_to_js (ME.NodeInfo.graphId info))
    ; ("collectionLabel", Ojs.string_to_js (ME.NodeInfo.collectionLabel info))
    ]

(** {2 Incoming messages} (extension host -> webview)

    - [{type: "set_graph_collections", graphCollections: [...]}]: replaces
      the visualizer's [graphCollections]. [graphCollections] is passed as a
      plain (structured-clonable) JS value shaped like
      [Model_explorer.GraphCollection.t list], not JSON-encoded -- VS Code's
      webview [postMessage] already round-trips plain objects/arrays.
    - [{type: "select_node", nodeId, graphId}]: forwards to
      {!Model_explorer.Visualizer.selectNode}. *)
let on_extension_message visualizer data =
  match [%js.to: string] (Ojs.get_prop_ascii data "type") with
  | "set_graph_collections" ->
    let graphCollections =
      [%js.to: ME.GraphCollection.t list]
        (Ojs.get_prop_ascii data "graphCollections")
    in
    ME.Visualizer.set_graphCollections visualizer graphCollections
  | "select_node" ->
    let nodeId = [%js.to: string] (Ojs.get_prop_ascii data "nodeId") in
    let graphId = [%js.to: string] (Ojs.get_prop_ascii data "graphId") in
    ME.Visualizer.selectNode visualizer ~nodeId ~graphId ()
  | _ | (exception _) -> ()

let main () =
  let visualizer = ME.Visualizer.create () in
  ME.Visualizer.set_config visualizer (visualizer_config ());
  ME.Visualizer.append_to ~parent:(document_body ()) visualizer;
  ME.Visualizer.on visualizer
    (`SelectedNodeChanged (send_node_info ~type_:"node_selected"));
  ME.Visualizer.on visualizer
    (`DoubleClickedNodeChanged (send_node_info ~type_:"node_double_clicked"));
  ME.Visualizer.on visualizer
    (`ModelGraphProcessed
      (fun ~paneIndex ->
        Vscode_webview_api.send ~type_:"model_graph_processed"
          [ ("paneIndex", Ojs.int_to_js paneIndex) ]));
  Vscode_webview_api.on_message (on_extension_message visualizer);
  send_ready ()

let () = main ()
