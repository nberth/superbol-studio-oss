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

module Ast = Graph.Dot_ast

type attrs = (string * string) list
type node = { id: string; attrs: attrs }
type edge = { src: string; dst: string; attrs: attrs }
type t = { nodes: node list; edges: edge list }

exception Parse_error of string

let string_of_id : Ast.id -> string = function
  | Ast.Ident s | Ast.Number s | Ast.String s | Ast.Html s -> s

let attrs_of_attr_list (attr_list: Ast.attr list) : attrs =
  List.concat_map
    (List.map (fun (key, value) ->
         string_of_id key,
         match value with Some value -> string_of_id value | None -> ""))
    attr_list

(* [merge ~defaults attrs] gives explicitly-set [attrs] precedence over
   same-named [defaults], as per the `node [...]`/`edge [...]` default
   attribute statements of the DOT language. *)
let merge ~defaults attrs =
  List.fold_left
    (fun acc (key, value) -> (key, value) :: List.remove_assoc key acc)
    defaults attrs

(* Only a flat digraph, made of node and edge statements plus `node [...]`/
   `edge [...]` default-attribute statements, is turned into a {!t}: this
   covers everything [Lsp_cfg]'s [ocamlgraph]-based dot printer ever emits
   (see [Cobol_cfg]/[Lsp_cfg.to_dot_string]).  Subgraphs (as statements, or as
   edge endpoints), ports, and graph-level attributes/[id = id] statements are
   ignored, since that printer never produces any: [Graph.Graphviz.Dot]'s
   [get_subgraph] always answers [None], and its [graph_attributes] is always
   empty. *)
let of_ast (file: Ast.file) =
  let node_defaults = ref [] and edge_defaults = ref [] in
  let node_attrs = Hashtbl.create 16 and node_order = ref [] in
  let edges = ref [] in
  let add_node id attrs =
    let attrs = merge ~defaults:!node_defaults attrs in
    match Hashtbl.find_opt node_attrs id with
    | Some existing -> Hashtbl.replace node_attrs id (merge ~defaults:existing attrs)
    | None -> Hashtbl.add node_attrs id attrs; node_order := id :: !node_order
  in
  let ensure_node id = if not (Hashtbl.mem node_attrs id) then add_node id [] in
  let node_id_string ((id, _port): Ast.node_id) = string_of_id id in
  let ids_of_node = function
    | Ast.NodeId node_id -> [node_id_string node_id]
    | Ast.NodeSub _ -> []
  in
  let handle_stmt = function
    | Ast.Node_stmt (node_id, attr_list) ->
        add_node (node_id_string node_id) (attrs_of_attr_list attr_list)
    | Ast.Edge_stmt (n0, ns, attr_list) ->
        let attrs = merge ~defaults:!edge_defaults (attrs_of_attr_list attr_list) in
        let chain = List.concat_map ids_of_node (n0 :: ns) in
        List.iter ensure_node chain;
        let rec pair = function
          | src :: (dst :: _ as rest) ->
              edges := { src; dst; attrs } :: !edges; pair rest
          | [] | [_] -> ()
        in
        pair chain
    | Ast.Attr_node attr_list -> node_defaults := attrs_of_attr_list attr_list
    | Ast.Attr_edge attr_list -> edge_defaults := attrs_of_attr_list attr_list
    | Ast.Attr_graph _ | Ast.Equal _ | Ast.Subgraph _ -> ()
  in
  List.iter handle_stmt file.Ast.stmts;
  { nodes =
      List.rev_map (fun id -> { id; attrs = Hashtbl.find node_attrs id })
        !node_order;
    edges = List.rev !edges }

(* [Graph.Dot]'s only public entry point, [parse_dot_ast], reads from a file
   (its lexer/parser proper, [Graph.Dot_lexer]/[Graph.Dot_parser], aren't
   re-exported by ocamlgraph's top-level [Graph] module) -- so [parse_file]
   takes a path rather than the dot source itself; callers holding a dot
   string in memory (e.g. from an LSP response) are expected to write it to a
   scratch file first. [Dot.parse_dot_ast] already turns any parse failure
   into a plain [Failure _], rather than letting [Parsing.Parse_error] escape. *)
let parse_file path =
  match Graph.Dot.parse_dot_ast path with
  | file -> of_ast file
  | exception Failure msg -> raise (Parse_error msg)
  | exception Sys_error msg -> raise (Parse_error msg)
