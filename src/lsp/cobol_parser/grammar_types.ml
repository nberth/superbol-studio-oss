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

module Overlay_manager = Grammar_utils.Overlay_manager

module Grammar = Grammar.Make (Overlay_manager)
module Grammar_context = Grammar_context.Make (Overlay_manager)
module Grammar_printer = Grammar_printer.Make (Overlay_manager)
module Grammar_recover = Grammar_recover.Make (Overlay_manager)
module Grammar_interpr = Grammar.MenhirInterpreter
module Grammar_recovery =
  Recovery.Make (Grammar_interpr) (struct
    include Grammar_recover
    include Grammar_printer
  end)
module Grammar_post_actions = Grammar_post_actions.Make (Overlay_manager)
