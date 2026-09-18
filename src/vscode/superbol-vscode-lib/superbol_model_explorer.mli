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

(** Extension-host counterpart of {!Model_explorer_webview}: opens a webview
    panel hosting a [<model-explorer-visualizer>] element (see
    [assets/model-explorer-renderer.html] and [assets/vendor/model-explorer/]),
    following the same [WebviewPanel] + [postMessage] architecture as
    {!Superbol_cfg_explorer}.

    There is, as of now, no LSP request producing a
    [Model_explorer.GraphCollection.t] from an actual COBOL program (unlike
    {!Superbol_cfg_explorer}, which calls [superbol/getCFG]): this feeds the
    webview {!demo_graph_collections} instead, to exercise the whole
    pipeline end to end. Wiring in a real graph source is the natural
    follow-up once such a request exists. *)

val demo_graph_collections : unit -> Model_explorer.GraphCollection.t list
(** A small, hardcoded 3-node graph, standing in for real graph data. *)

val open_model_explorer : Superbol_instance.t -> unit Promise.t
(** Opens (creating it if needed, else just revealing/refreshing it) the
    Model Explorer webview panel, feeding it {!demo_graph_collections}. *)
