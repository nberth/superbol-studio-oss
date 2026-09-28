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

(** Note: fields of this record are further initialized when entering
    `Lsp_server_loop.run` *)
let record = Cobol_common.Platform.innocuous

(** How a cache directory is opened.  Like {!record}, this is set when entering
    [Lsp_server_loop.run]; until then --- and whenever the application does not
    provide one --- no cache can be opened, and every lookup misses. *)
let cache_store_for
  : (root:string -> Superbol_cache.Store.TYPES.store) ref
  = ref (fun ~root:_ -> Superbol_cache.Store.unavailable)
