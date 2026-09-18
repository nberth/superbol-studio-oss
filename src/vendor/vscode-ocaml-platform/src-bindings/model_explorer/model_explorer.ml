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

open Interop

module Dict = Interop.Dict

module KeyValue = struct
  include Interface.Make ()

  include
    [%js:
    val make : key:string -> value:string -> t [@@js.builder]

    val key : t -> string [@@js.get]

    val value : t -> string [@@js.get]]
end

module NodeAttributeValue = struct
  type t =
    [ `String of string
    | `NodeIds of string list
    ]

  let t_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String ([%js.to: string] js_val)
    | _ -> `NodeIds ([%js.to: string list] (Ojs.get_prop_ascii js_val "nodeIds"))

  let t_to_js = function
    | `String s -> [%js.of: string] s
    | `NodeIds ids ->
      let js_val = Ojs.empty_obj () in
      Ojs.set_prop_ascii js_val "type" (Ojs.string_to_js "node_ids");
      Ojs.set_prop_ascii js_val "nodeIds" ([%js.of: string list] ids);
      js_val
end

module NodeAttribute = struct
  include Interface.Make ()

  include
    [%js:
    val make : key:string -> value:NodeAttributeValue.t -> t [@@js.builder]

    val key : t -> string [@@js.get]

    val value : t -> NodeAttributeValue.t [@@js.get]]
end

module MetadataItem = struct
  include Interface.Make ()

  include
    [%js:
    val make : id:string -> attrs:KeyValue.t list -> t [@@js.builder]

    val id : t -> string [@@js.get]

    val attrs : t -> KeyValue.t list [@@js.get]]
end

module IncomingEdge = struct
  include Interface.Make ()

  include
    [%js:
    val make :
         sourceNodeId:string
      -> sourceNodeOutputId:string
      -> targetNodeInputId:string
      -> ?metadata:string Dict.t
      -> unit
      -> t
      [@@js.builder]

    val sourceNodeId : t -> string [@@js.get]

    val sourceNodeOutputId : t -> string [@@js.get]

    val targetNodeInputId : t -> string [@@js.get]

    val metadata : t -> string Dict.t or_undefined [@@js.get]]
end

module GroupNodeAttributes = struct
  type t = string Dict.t Dict.t

  let t_of_js js_val = Dict.t_of_js (Dict.t_of_js Ojs.string_of_js) js_val
  let t_to_js t = Dict.t_to_js (Dict.t_to_js Ojs.string_to_js) t
end

module GraphNodeStyle = struct
  include Interface.Make ()

  include
    [%js:
    val make :
         ?backgroundColor:string
      -> ?borderColor:string
      -> ?hoveredBorderColor:string
      -> unit
      -> t
      [@@js.builder]

    val backgroundColor : t -> string or_undefined [@@js.get]

    val borderColor : t -> string or_undefined [@@js.get]

    val hoveredBorderColor : t -> string or_undefined [@@js.get]]
end

module GraphNodeConfig = struct
  include Interface.Make ()

  include
    [%js:
    val make : ?pinToGroupTop:bool -> unit -> t [@@js.builder]

    val pinToGroupTop : t -> bool or_undefined [@@js.get]]
end

module GraphNode = struct
  include Interface.Make ()

  include
    [%js:
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
      [@@js.builder]

    val id : t -> string [@@js.get]

    val label : t -> string [@@js.get]

    val namespace : t -> string [@@js.get]

    val subgraphIds : t -> string list or_undefined [@@js.get]

    val attrs : t -> NodeAttribute.t list or_undefined [@@js.get]

    val incomingEdges : t -> IncomingEdge.t list or_undefined [@@js.get]

    val inputsMetadata : t -> MetadataItem.t list or_undefined [@@js.get]

    val outputsMetadata : t -> MetadataItem.t list or_undefined [@@js.get]

    val style : t -> GraphNodeStyle.t or_undefined [@@js.get]

    val config : t -> GraphNodeConfig.t or_undefined [@@js.get]]
end

module Graph = struct
  include Interface.Make ()

  include
    [%js:
    val make :
         id:string
      -> nodes:GraphNode.t list
      -> ?groupNodeAttributes:GroupNodeAttributes.t
      -> unit
      -> t
      [@@js.builder]

    val id : t -> string [@@js.get]

    val nodes : t -> GraphNode.t list [@@js.get]

    val groupNodeAttributes : t -> GroupNodeAttributes.t or_undefined
      [@@js.get]

    val collectionLabel : t -> string or_undefined [@@js.get]

    val subGraphIds : t -> string list or_undefined [@@js.get]

    val parentGraphIds : t -> string list or_undefined [@@js.get]]
end

module GraphCollection = struct
  include Interface.Make ()

  include
    [%js:
    val make : label:string -> graphs:Graph.t list -> unit -> t
      [@@js.builder]

    val label : t -> string [@@js.get]

    val graphs : t -> Graph.t list [@@js.get]]
end

module NodeInfo = struct
  include Interface.Make ()

  include
    [%js:
    val nodeId : t -> string [@@js.get]

    val graphId : t -> string [@@js.get]

    val collectionLabel : t -> string [@@js.get]

    val node : t -> Ojs.t or_undefined [@@js.get]]
end

module ThresholdItem = struct
  include Interface.Make ()

  include
    [%js:
    val make : value:float -> bgColor:string -> ?textColor:string -> unit -> t
      [@@js.builder]

    val value : t -> float [@@js.get]

    val bgColor : t -> string [@@js.get]

    val textColor : t -> string or_undefined [@@js.get]]
end

module GradientItem = struct
  include Interface.Make ()

  include
    [%js:
    val make : stop:float -> ?bgColor:string -> ?textColor:string -> unit -> t
      [@@js.builder]

    val stop : t -> float [@@js.get]

    val bgColor : t -> string or_undefined [@@js.get]

    val textColor : t -> string or_undefined [@@js.get]]
end

module AggregatedStat = struct
  type t =
    | Min [@js "min"]
    | Max [@js "max"]
    | Sum [@js "sum"]
    | Avg [@js "avg"]
  [@@js.enum] [@@js]
end

module NodeDataProviderResultData = struct
  include Interface.Make ()

  include
    [%js:
    val make : value:Ojs.t -> ?bgColor:string -> ?textColor:string -> unit -> t
      [@@js.builder]

    val value : t -> Ojs.t [@@js.get]

    val bgColor : t -> string or_undefined [@@js.get]

    val textColor : t -> string or_undefined [@@js.get]]
end

module NodeDataProviderGraphData = struct
  include Interface.Make ()

  include
    [%js:
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
      [@@js.builder]

    val name : t -> string or_undefined [@@js.get]

    val results : t -> NodeDataProviderResultData.t Dict.t [@@js.get]

    val thresholds : t -> ThresholdItem.t list or_undefined [@@js.get]

    val gradient : t -> GradientItem.t list or_undefined [@@js.get]]
end

module NodeDataProviderData = struct
  type t = NodeDataProviderGraphData.t Dict.t

  let t_of_js = Dict.t_of_js NodeDataProviderGraphData.t_of_js
  let t_to_js = Dict.t_to_js NodeDataProviderGraphData.t_to_js
end

module PaneState = struct
  include Interface.Make ()

  include
    [%js:
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
      [@@js.builder]

    val deepestExpandedGroupNodeIds : t -> string list [@@js.get]

    val selectedNodeId : t -> string [@@js.get]

    val selectedGraphId : t -> string [@@js.get]

    val selectedCollectionLabel : t -> string [@@js.get]

    val widthFraction : t -> float [@@js.get]

    val selected : t -> bool or_undefined [@@js.get]

    val flattenLayers : t -> bool or_undefined [@@js.get]]
end

module VisualizerUiState = struct
  include Interface.Make ()

  include
    [%js:
    val make : paneStates:PaneState.t list -> unit -> t [@@js.builder]

    val paneStates : t -> PaneState.t list [@@js.get]]
end

module LegendConfig = struct
  include Interface.Make ()

  include
    [%js:
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
      [@@js.builder]]
end

module ViewOnNodeConfig = struct
  include Interface.Make ()

  include
    [%js:
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
      [@@js.builder]]
end

module ToolbarConfig = struct
  include Interface.Make ()

  include
    [%js:
    val make :
         ?hideExpandCollapseAllLayers:bool
      -> ?hideFlattenAllLayers:bool
      -> ?hideCustomEdgeOverlays:bool
      -> unit
      -> t
      [@@js.builder]]
end

module VisualizerConfig = struct
  include Interface.Make ()

  include
    [%js:
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
      -> ?nodeStylerRules:Ojs.t
      -> ?syncNavigationData:Ojs.t
      -> ?edgeOverlaysDataListLeftPane:Ojs.t
      -> ?edgeOverlaysDataListRightPane:Ojs.t
      -> unit
      -> t
      [@@js.builder]]
end

module Global = struct
  include [%js: val modelExplorer : Ojs.t [@@js.global "modelExplorer"]]

  let assetFilesBaseUrl () =
    [%js.to: string or_undefined]
      (Ojs.get_prop_ascii modelExplorer "assetFilesBaseUrl")

  let set_assetFilesBaseUrl v =
    Ojs.set_prop_ascii modelExplorer "assetFilesBaseUrl" ([%js.of: string] v)

  let workerScriptPath () =
    [%js.to: string or_undefined]
      (Ojs.get_prop_ascii modelExplorer "workerScriptPath")

  let set_workerScriptPath v =
    Ojs.set_prop_ascii modelExplorer "workerScriptPath" ([%js.of: string] v)
end

module Visualizer = struct
  include Class.Make ()

  include
    [%js:
    val createElement : string -> t [@@js.global "document.createElement"]

    val graphCollections : t -> GraphCollection.t list [@@js.get]

    val set_graphCollections : t -> GraphCollection.t list -> unit
      [@@js.set "graphCollections"]

    val config : t -> VisualizerConfig.t or_undefined [@@js.get]

    val set_config : t -> VisualizerConfig.t -> unit [@@js.set "config"]

    val initialUiState : t -> VisualizerUiState.t or_undefined [@@js.get]

    val set_initialUiState : t -> VisualizerUiState.t -> unit
      [@@js.set "initialUiState"]

    val benchmark : t -> bool or_undefined [@@js.get]

    val set_benchmark : t -> bool -> unit [@@js.set "benchmark"]

    val selectNode :
         t
      -> nodeId:string
      -> graphId:string
      -> ?collectionLabel:string
      -> ?paneIndex:int
      -> unit
      -> unit
      [@@js.call]

    val addNodeDataProviderData :
         t
      -> name:string
      -> data:NodeDataProviderGraphData.t
      -> ?paneIndex:int
      -> ?clearExisting:bool
      -> unit
      -> unit
      [@@js.call]

    val addNodeDataProviderDataWithGraphIndex :
         t
      -> name:string
      -> data:NodeDataProviderData.t
      -> ?paneIndex:int
      -> ?clearExisting:bool
      -> unit
      -> unit
      [@@js.call]

    val addEventListener : t -> string -> Ojs.t -> unit [@@js.call]]

  let create () = createElement "model-explorer-visualizer"

  let append_to ~parent t =
    ignore (Ojs.call parent "appendChild" [| t_to_js t |] : Ojs.t)

  let detail_of event = Ojs.get_prop_ascii event "detail"

  let on t = function
    | `TitleClicked f ->
      addEventListener t "titleClicked" ([%js.of: unit -> unit] f)
    | `UiStateChanged f ->
      addEventListener t "uiStateChanged"
        ([%js.of: Ojs.t -> unit] (fun event ->
             f ([%js.to: VisualizerUiState.t] (detail_of event))))
    | `ModelGraphProcessed f ->
      addEventListener t "modelGraphProcessed"
        ([%js.of: Ojs.t -> unit] (fun event ->
             let detail = detail_of event in
             f ~paneIndex:([%js.to: int] (Ojs.get_prop_ascii detail "paneIndex"))))
    | `SelectedNodeChanged f ->
      addEventListener t "selectedNodeChanged"
        ([%js.of: Ojs.t -> unit] (fun event ->
             f ([%js.to: NodeInfo.t] (detail_of event))))
    | `HoveredNodeChanged f ->
      addEventListener t "hoveredNodeChanged"
        ([%js.of: Ojs.t -> unit] (fun event ->
             f ([%js.to: NodeInfo.t] (detail_of event))))
    | `DoubleClickedNodeChanged f ->
      addEventListener t "doubleClickedNodeChanged"
        ([%js.of: Ojs.t -> unit] (fun event ->
             f ([%js.to: NodeInfo.t] (detail_of event))))
end
