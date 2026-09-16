(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

module Make (Overlay_manager: Cobol_preproc.Src_overlay.MANAGER) : sig
  open Grammar.Make (Overlay_manager)
  open MenhirInterpreter

  type action =
    | Abort
    | R of int
    | S : 'a symbol -> action
    | Sub of action list

  type decision =
    | Nothing
    | One of action list
    | Select of (int -> action list)

  val can_pop : 'a terminal -> bool
  val depth : int array
  val recover : int -> decision
  val default_value :
    pos:Lexing.position
    -> 'a symbol
    -> 'a
end
