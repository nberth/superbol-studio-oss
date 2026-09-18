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

(** Turns a Graphviz DOT file into a flat node/edge list, using
    [ocamlgraph]'s own DOT parser ({!Graph.Dot.parse_dot_ast}) -- the same
    grammar {!Cobol_cfg}'s [Graph.Graphviz.Dot] functor instantiation prints
    from (see [Lsp_cfg.to_dot_string], in [src/lsp/cobol_lsp/lsp_cfg.ml]).

    Only a flat digraph is exposed: node statements, edge statements (edge
    chains, e.g. [a -> b -> c], are expanded into one {!edge} per
    consecutive pair), and [node [...];] / [edge [...];] default-attribute
    statements. Subgraphs (whether as statements, or as edge endpoints),
    ports, and graph-level attributes are ignored -- {!Cobol_cfg}'s printer
    never emits any of these, so this covers the whole subset it produces,
    without pulling in the full generality of the DOT language.

    [Graph.Dot]'s only public entry point reads a dot file from disk rather
    than from a string held in memory: {!parse_file} therefore takes a path,
    and a caller starting from a dot {e string} (e.g. an LSP response) is
    expected to write it to a scratch file of its own first. *)

type attrs = (string * string) list

type node = { id: string; attrs: attrs }
type edge = { src: string; dst: string; attrs: attrs }
type t = { nodes: node list; edges: edge list }

exception Parse_error of string

val parse_file: string -> t
(** [parse_file path] parses the DOT digraph found at [path] and flattens it
    as described above.  Attributes on a returned {!node}/{!edge} already
    reflect whichever default-attribute statement was in effect when it was
    declared, overridden by whatever it set explicitly.  Nodes appear in
    {!t.nodes} in the order of their first mention (whether as an explicit
    node statement, or, absent one, as an edge endpoint); edges appear in
    {!t.edges} in source order.
    @raise Parse_error if [path] can't be read, or isn't syntactically valid
    DOT. *)
