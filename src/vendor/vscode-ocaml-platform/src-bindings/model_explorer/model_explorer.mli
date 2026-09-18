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

(** gen_js_api bindings for the [<model-explorer-visualizer>] custom element,
    published as the ["ai-edge-model-explorer-visualizer"] npm package (see
    {{:https://github.com/google-ai-edge/model-explorer}
    google-ai-edge/model-explorer}).

    Unlike {!Vscode} or {!Node}, this custom element only exists in a DOM
    (browser) context, so this module is meant to be [open]ed from OCaml
    compiled with [js_of_ocaml] and run as *webview content*, not from
    extension-host code. The vendored visualizer script (loaded via a
    [<script src=...>] tag, see the package's README) must already be present
    on the page before any of these bindings are used: it is what registers
    the [model-explorer-visualizer] custom element and the [modelExplorer]
    global object.

    Bound against version [0.1.2] of the npm package (its
    [src/custom_element/index.d.ts] and the [common/*.ts] type files it
    re-exports). Some of the more advanced, less commonly needed corners of
    {!VisualizerConfig} (node styler rules, sync-navigation data, edge overlay
    data) are intentionally left as opaque {!Ojs.t} passthroughs for now
    rather than modelled field-by-field -- see the comments below. *)

open Interop

(** {!Ojs.t}-keyed dictionaries, as used for TypeScript's
    [Record<string, V>]. *)
module Dict = Interop.Dict

(** {2 Shared leaf types}

    These mirror the small, mostly-orthogonal interfaces found across
    [common/types.ts]. *)

module KeyValue : sig
  include Js.T

  val make : key:string -> value:string -> t
  val key : t -> string
  val value : t -> string
end

(** A single node attribute value: either a plain string, or a "node ids"
    value (clicking on one of the ids jumps to the corresponding node). *)
module NodeAttributeValue : sig
  type t =
    [ `String of string
    | `NodeIds of string list
    ]

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t
end

module NodeAttribute : sig
  include Js.T

  val make : key:string -> value:NodeAttributeValue.t -> t
  val key : t -> string
  val value : t -> NodeAttributeValue.t
end

module MetadataItem : sig
  include Js.T

  val make : id:string -> attrs:KeyValue.t list -> t
  val id : t -> string
  val attrs : t -> KeyValue.t list
end

module IncomingEdge : sig
  include Js.T

  val make :
       sourceNodeId:string
    -> sourceNodeOutputId:string
    -> targetNodeInputId:string
    -> ?metadata:string Dict.t
    -> unit
    -> t

  val sourceNodeId : t -> string
  val sourceNodeOutputId : t -> string
  val targetNodeInputId : t -> string
  val metadata : t -> string Dict.t or_undefined
end

(** [GroupNodeAttributes]: from a group's namespace to its attributes
    (key/value pairs); the empty namespace holds model-level attributes. *)
module GroupNodeAttributes : sig
  type t = string Dict.t Dict.t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t
end

module GraphNodeStyle : sig
  include Js.T

  val make :
       ?backgroundColor:string
    -> ?borderColor:string
    -> ?hoveredBorderColor:string
    -> unit
    -> t

  val backgroundColor : t -> string or_undefined
  val borderColor : t -> string or_undefined
  val hoveredBorderColor : t -> string or_undefined
end

module GraphNodeConfig : sig
  include Js.T

  val make : ?pinToGroupTop:bool -> unit -> t
  val pinToGroupTop : t -> bool or_undefined
end

(** {2 Input graph}

    Mirrors [common/input_graph.ts] -- this is the data clients feed into the
    visualizer via {!Visualizer.set_graphCollections}. *)

module GraphNode : sig
  include Js.T

  val make :
       id:string
    -> label:string
    -> namespace:string
    -> ?subgraphIds:string list
    -> ?attrs:NodeAttribute.t list
    -> ?incomingEdges:IncomingEdge.t list
    -> ?inputsMetadata:MetadataItem.t list
    -> ?outputsMetadata:MetadataItem.t list
    -> ?style:GraphNodeStyle.t
    -> ?config:GraphNodeConfig.t
    -> unit
    -> t

  val id : t -> string
  val label : t -> string
  val namespace : t -> string
  val subgraphIds : t -> string list or_undefined
  val attrs : t -> NodeAttribute.t list or_undefined
  val incomingEdges : t -> IncomingEdge.t list or_undefined
  val inputsMetadata : t -> MetadataItem.t list or_undefined
  val outputsMetadata : t -> MetadataItem.t list or_undefined
  val style : t -> GraphNodeStyle.t or_undefined
  val config : t -> GraphNodeConfig.t or_undefined
end

module Graph : sig
  include Js.T

  val make :
       id:string
    -> nodes:GraphNode.t list
    -> ?groupNodeAttributes:GroupNodeAttributes.t
    -> unit
    -> t

  val id : t -> string
  val nodes : t -> GraphNode.t list
  val groupNodeAttributes : t -> GroupNodeAttributes.t or_undefined

  (** {3 Fields set by the visualizer itself}

      Present (and meaningful) only on graphs handed back by the visualizer
      (e.g. in event payloads), not on graphs a client constructs. *)

  val collectionLabel : t -> string or_undefined
  val subGraphIds : t -> string list or_undefined
  val parentGraphIds : t -> string list or_undefined
end

module GraphCollection : sig
  include Js.T

  val make : label:string -> graphs:Graph.t list -> unit -> t
  val label : t -> string
  val graphs : t -> Graph.t list
end

(** {2 Node info}

    Payload of the [selectedNodeChanged], [hoveredNodeChanged] and
    [doubleClickedNodeChanged] events. *)
module NodeInfo : sig
  include Js.T

  val nodeId : t -> string
  val graphId : t -> string
  val collectionLabel : t -> string

  (** The visualizer's own (internal, processed) representation of the node,
      when available. Left opaque: {!ModelGraph}/{!ModelNode} are an
      internal, richer graph representation not meant to be built by
      clients; bind into it on demand if a concrete need arises. *)
  val node : t -> Ojs.t or_undefined
end

(** {2 Node data provider}

    Mirrors the subset of [common/types.ts] needed by
    {!Visualizer.addNodeDataProviderData} and
    {!Visualizer.addNodeDataProviderDataWithGraphIndex}. *)

module ThresholdItem : sig
  include Js.T

  val make : value:float -> bgColor:string -> ?textColor:string -> unit -> t
  val value : t -> float
  val bgColor : t -> string
  val textColor : t -> string or_undefined
end

module GradientItem : sig
  include Js.T

  val make : stop:float -> ?bgColor:string -> ?textColor:string -> unit -> t
  val stop : t -> float
  val bgColor : t -> string or_undefined
  val textColor : t -> string or_undefined
end

module AggregatedStat : sig
  type t =
    | Min
    | Max
    | Sum
    | Avg
end

module NodeDataProviderResultData : sig
  include Js.T

  val make : value:Ojs.t -> ?bgColor:string -> ?textColor:string -> unit -> t
  val value : t -> Ojs.t
  val bgColor : t -> string or_undefined
  val textColor : t -> string or_undefined
end

module NodeDataProviderGraphData : sig
  include Js.T

  val make :
       results:NodeDataProviderResultData.t Dict.t
    -> ?name:string
    -> ?thresholds:ThresholdItem.t list
    -> ?gradient:GradientItem.t list
    -> ?hideInAggregatedStatsTable:bool
    -> ?hideInChildrenStatsTable:bool
    -> ?hideAggregatedStats:AggregatedStat.t list
    -> ?showExpandedSummaryOnGroupNode:bool
    -> ?showLabelCountColumnsInChildrenStatsTable:bool
    -> unit
    -> t

  val name : t -> string or_undefined
  val results : t -> NodeDataProviderResultData.t Dict.t
  val thresholds : t -> ThresholdItem.t list or_undefined
  val gradient : t -> GradientItem.t list or_undefined
end

(** [NodeDataProviderData]: the top-level node data provider data, indexed by
    graph id. *)
module NodeDataProviderData : sig
  type t = NodeDataProviderGraphData.t Dict.t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t
end

(** {2 UI state}

    Mirrors [common/visualizer_ui_state.ts]. Used both as an *input*
    ({!Visualizer.set_initialUiState}) and as the payload of the
    [uiStateChanged] event. *)

module PaneState : sig
  include Js.T

  val make :
       deepestExpandedGroupNodeIds:string list
    -> selectedNodeId:string
    -> selectedGraphId:string
    -> selectedCollectionLabel:string
    -> widthFraction:float
    -> ?selected:bool
    -> ?flattenLayers:bool
    -> unit
    -> t

  val deepestExpandedGroupNodeIds : t -> string list
  val selectedNodeId : t -> string
  val selectedGraphId : t -> string
  val selectedCollectionLabel : t -> string
  val widthFraction : t -> float
  val selected : t -> bool or_undefined
  val flattenLayers : t -> bool or_undefined
end

module VisualizerUiState : sig
  include Js.T

  val make : paneStates:PaneState.t list -> unit -> t
  val paneStates : t -> PaneState.t list
end

(** {2 Visualizer configuration}

    Mirrors [common/visualizer_config.ts]. This is set once via
    {!Visualizer.set_config} before the element is attached to the DOM. *)

module LegendConfig : sig
  include Js.T

  val make :
       ?hideOp:bool
    -> ?hideLayer:bool
    -> ?hideArtificialLayers:bool
    -> ?hideSelectedOp:bool
    -> ?hideSelectedLayer:bool
    -> ?hideIdenticalLayers:bool
    -> ?hideInputs:bool
    -> ?hideOutputs:bool
    -> ?hideShortcuts:bool
    -> ?renameOpTo:string
    -> ?renameLayerTo:string
    -> ?renameInputsTo:string
    -> ?renameOutputsTo:string
    -> unit
    -> t
end

module ViewOnNodeConfig : sig
  include Js.T

  val make :
       ?hideOpNodeId:bool
    -> ?hideOpNodeAttributes:bool
    -> ?hideOpNodeInputs:bool
    -> ?hideOpNodeOutputs:bool
    -> ?hideLayerNodeChildrenCount:bool
    -> ?hideLayerNodeDescendantsCount:bool
    -> ?hideLayerNodeAttributes:bool
    -> ?hideViewOnEdgesSection:bool
    -> ?renameOpNodeIdTo:string
    -> ?renameOpNodeAttributesTo:string
    -> ?renameOpNodeInputsTo:string
    -> ?renameOpNodeOutputsTo:string
    -> unit
    -> t
end

module ToolbarConfig : sig
  include Js.T

  val make :
       ?hideExpandCollapseAllLayers:bool
    -> ?hideFlattenAllLayers:bool
    -> ?hideCustomEdgeOverlays:bool
    -> unit
    -> t
end

module VisualizerConfig : sig
  include Js.T

  val make :
       ?nodeLabelsToHide:string list
    -> ?nodeAttrsToHide:string Dict.t
    -> ?artificialLayerNodeCountThreshold:int
    -> ?edgeLabelFontSize:float
    -> ?edgeColor:string
    -> ?maxConstValueCount:int
    -> ?disallowVerticalEdgeLabels:bool
    -> ?enableSubgraphSelection:bool
    -> ?enableExportToResource:bool
    -> ?enableExportSelectedNodes:bool
    -> ?exportSelectedNodesButtonLabel:string
    -> ?exportSelectedNodesButtonIcon:string
    -> ?keepLayersWithASingleChild:bool
    -> ?showOpNodeOutOfLayerEdgesWithoutSelecting:bool
    -> ?highlightLayerNodeInputsOutputs:bool
    -> ?hideEmptyNodeDataEntries:bool
    -> ?hideTitleBar:bool
    -> ?hideToolBar:bool
    -> ?hideInfoPanel:bool
    -> ?hideNodeDataInInfoPanel:bool
    -> ?hideLegends:bool
    -> ?nodeInfoKeysToHide:string list
    -> ?inputMetadataKeysToHide:string list
    -> ?outputMetadataKeysToHide:string list
    -> ?renameNodeInfoOpNameTo:string
    -> ?legendConfig:LegendConfig.t
    -> ?viewOnNodeConfig:ViewOnNodeConfig.t
    -> ?toolbarConfig:ToolbarConfig.t
    (* TODO(model-explorer-bindings): the following [VisualizerConfig] fields
       are deliberately left as raw [Ojs.t] passthroughs -- each involves a
       discriminated-union or otherwise non-trivial shape
       ([NodeStylerRule]'s [NodeQuery] union, [SyncNavigationData] /
       [EdgeOverlaysData]'s [TaskData] tag). Model them properly once a
       concrete use case needs them; until then, callers can still set them
       by hand via {!Ojs}. *)
    -> ?nodeStylerRules:Ojs.t
    -> ?syncNavigationData:Ojs.t
    -> ?edgeOverlaysDataListLeftPane:Ojs.t
    -> ?edgeOverlaysDataListRightPane:Ojs.t
    -> unit
    -> t
end

(** {2 Global configuration}

    The [modelExplorer] global object (see the package's README), used to
    customize where [worker.js] and [static_files/] are served from. Set
    these, if needed, before {!Visualizer.create}. *)
module Global : sig
  val assetFilesBaseUrl : unit -> string or_undefined
  val set_assetFilesBaseUrl : string -> unit
  val workerScriptPath : unit -> string or_undefined
  val set_workerScriptPath : string -> unit
end

(** {2 The [<model-explorer-visualizer>] element itself} *)
module Visualizer : sig
  include Js.T

  val create : unit -> t
  (** [document.createElement "model-explorer-visualizer"], cast to {!t}. Set
      {!set_graphCollections}, {!set_config} and {!set_initialUiState} (as
      needed) before attaching the returned element to the DOM, then
      {!append_to} it. *)

  val append_to : parent:Ojs.t -> t -> unit
  (** [parent.appendChild(t)] -- e.g. [parent] can be
      [Ojs.variable "document.body"], or any other DOM node/element you
      already hold as raw {!Ojs.t}. Generic DOM node/element traversal isn't
      modelled here; reach for it if this grows more DOM-side glue code. *)

  val graphCollections : t -> GraphCollection.t list
  val set_graphCollections : t -> GraphCollection.t list -> unit
  val config : t -> VisualizerConfig.t or_undefined
  val set_config : t -> VisualizerConfig.t -> unit
  val initialUiState : t -> VisualizerUiState.t or_undefined
  val set_initialUiState : t -> VisualizerUiState.t -> unit
  val benchmark : t -> bool or_undefined
  val set_benchmark : t -> bool -> unit

  val selectNode :
       t
    -> nodeId:string
    -> graphId:string
    -> ?collectionLabel:string
    -> ?paneIndex:int
    -> unit
    -> unit

  val addNodeDataProviderData :
       t
    -> name:string
    -> data:NodeDataProviderGraphData.t
    -> ?paneIndex:int
    -> ?clearExisting:bool
    -> unit
    -> unit

  val addNodeDataProviderDataWithGraphIndex :
       t
    -> name:string
    -> data:NodeDataProviderData.t
    -> ?paneIndex:int
    -> ?clearExisting:bool
    -> unit
    -> unit

  (** Subscribes to one of the element's [CustomEvent]s (see the package's
      README "Events" section; these aren't part of the shipped [.d.ts], only
      documented). Each case reads the event's [detail] field, typed
      accordingly. There is currently no way to unsubscribe (no [Disposable]
      is returned) -- add one (backed by [removeEventListener]) if a caller
      needs to detach a listener before the element itself is torn down. *)
  val on :
       t
    -> [ `TitleClicked of unit -> unit
       | `UiStateChanged of VisualizerUiState.t -> unit
       | `ModelGraphProcessed of paneIndex:int -> unit
       | `SelectedNodeChanged of NodeInfo.t -> unit
       | `HoveredNodeChanged of NodeInfo.t -> unit
       | `DoubleClickedNodeChanged of NodeInfo.t -> unit
       ]
    -> unit
end
