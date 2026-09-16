(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* open MenhirSdk *)

module Gu =
  Grammarware_utils.Make (struct
    let name = "printer"
    let filename = Sys.argv.(1)
  end)

open Gu.Grammar

let menhir =
  "Grammar.MenhirInterpreter"

(** Printer from attributes *)

let is_attribute names attr =
  List.exists (fun l -> Attribute.has_label l attr) names

let symbol_printer ppf (default, attribs) =
  match List.find (is_attribute ["symbol"]) attribs with
  | attr ->
      Fmt.string ppf (Attribute.payload attr)
  | exception Not_found ->
      Fmt.pf ppf "%S" default

let print_symbol ppf =
  let case_t t =
    match Terminal.kind t with
    | `REGULAR | `ERROR | `EOF ->
        Fmt.pf ppf "    | X T T_%s -> %a\n"
          (Terminal.name t)
          symbol_printer (Terminal.name t, Terminal.attributes t)
    | `PSEUDO -> ()
  and case_n n =
    match Nonterminal.kind n with
    | `REGULAR ->
        Fmt.pf ppf "    | X N N_%s -> %a\n"
          (Nonterminal.mangled_name n)
          symbol_printer (Nonterminal.mangled_name n, Nonterminal.attributes n)
    | `START -> ()
  in
  Fmt.pf ppf "let print_symbol: %s.xsymbol -> _ = function\n" menhir;
  Terminal.iter case_t;
  Nonterminal.iter case_n

let value_printer ppf (default, attribs) =
  match List.find (is_attribute ["printer"]) attribs with
  | attr ->
      Fmt.pf ppf "(%s)" (Attribute.payload attr)
  | exception Not_found ->
      Fmt.pf ppf "(fun _ -> %a)" symbol_printer (default, attribs)

let print_value ppf =
  let case_t t =
    match Terminal.kind t with
    | `REGULAR | `ERROR | `EOF->
        Fmt.pf ppf "    | T T_%s -> %a\n"
          (Terminal.name t)
          value_printer (Terminal.name t, Terminal.attributes t)
    | `PSEUDO -> ()
  and case_n n =
    match Nonterminal.kind n with
    | `REGULAR ->
        Fmt.pf ppf "    | N N_%s -> %a\n"
          (Nonterminal.mangled_name n)
          value_printer (Nonterminal.mangled_name n, Nonterminal.attributes n)
    | `START -> ()
  in
  Fmt.pf ppf "let print_value (type a) : a %s.symbol -> a -> string = function\n"
    menhir;
  Terminal.iter case_t;
  Nonterminal.iter case_n

let print_token ppf =
  let case t =
    match Terminal.kind t with
    | `REGULAR | `EOF ->
      Fmt.pf ppf "    | %s%s -> print_value (T T_%s) %s\n"
        (Terminal.name t)
        (match Terminal.typ t with | None -> "" | Some _typ -> " v")
        (Terminal.name t)
        (match Terminal.typ t with | None -> "()" | Some _typ -> "v")
    | `PSEUDO | `ERROR -> ()
  in
  Fmt.pf ppf "let print_token = function\n";
  Terminal.iter case

let print_token_of_terminal ppf =
  let case t =
    match Terminal.kind t with
    | `REGULAR | `EOF ->
      Fmt.pf ppf "    | T_%s -> %s%s\n"
        (Terminal.name t)
        (Terminal.name t) (if Terminal.typ t <> None then " v" else "")
    | `ERROR ->
      Fmt.pf ppf "    | T_%s -> assert false\n"
        (Terminal.name t)
    | `PSEUDO -> ()
  in
  Fmt.pf ppf
    "let token_of_terminal (type a) (t : a %s.terminal) (v : a) : token =\n\
    \    match t with\n"
    menhir;
  Terminal.iter case

let emit ppf =
  Gu.pp_extension_module ppf begin fun ppf ->
    Fmt.pf ppf "%t@\n" Gu.pp_grammar_open;
    Fmt.pf ppf "%t@\n" Gu.pp_header;
    print_symbol ppf;
    Fmt.cut ppf ();
    print_value ppf;
    Fmt.cut ppf ();
    print_token ppf;
    Fmt.cut ppf ();
    print_token_of_terminal ppf;
  end

let () =
  emit Fmt.stdout
