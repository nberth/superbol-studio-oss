(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for converting messages from and to JSON.  Decoding failures raise
    {!Jsonrpc.Json.Of_json}, as the converters of the [lsp] library do. *)

type t = Jsonrpc.Json.t

let error msg json =
  raise (Jsonrpc.Json.Of_json (msg, json))

(* --- Decoding --- *)

let assoc : t -> (string * t) list = function
  | `Assoc fields -> fields
  | json -> error "object expected" json

(** [field_opt name json] is the value of field [name] of object [json], if it
    is present and not [null]. *)
let field_opt name json =
  match List.assoc_opt name (assoc json) with
  | None | Some `Null -> None
  | Some v -> Some v

let field name json =
  match field_opt name json with
  | Some v -> v
  | None -> error ("missing field " ^ name) json

let string : t -> string = function
  | `String s -> s
  | json -> error "string expected" json

let bool : t -> bool = function
  | `Bool b -> b
  | json -> error "boolean expected" json

let int : t -> int = function
  | `Int i -> i
  | `Intlit s as json ->
      (try int_of_string s with Failure _ -> error "integer expected" json)
  | json -> error "integer expected" json

let list f : t -> 'a list = function
  | `List l -> List.map f l
  | json -> error "array expected" json

(** Decoders of optional fields. *)
let string_opt name json = Option.map string (field_opt name json)
let bool_opt name json = Option.map bool (field_opt name json)

(* --- Encoding --- *)

(** [obj fields] is an object with every field of [fields] whose value is
    given. *)
let obj (fields: (string * t option) list) : t =
  `Assoc (List.filter_map (fun (k, v) -> Option.map (fun v -> k, v) v) fields)

let str s : t = `String s
let opt f = Option.map f
