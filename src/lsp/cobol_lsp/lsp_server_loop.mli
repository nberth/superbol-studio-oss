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

module TYPES: sig
  type extensions =
    {
      alternate_request_handers: Lsp_request.TYPES.alternate_handler list;
    }
end
include module type of TYPES
  with type extensions = TYPES.extensions

val default_extensions: extensions

(** [config ~project_layout ...] gathers the server's settings.

    [cache_max_age] (in seconds) and [cache_max_size] (in bytes) bound what a
    project's cache keeps; with neither, it grows without limit. *)
val config
  : project_layout: Superbol_project.layout
  -> ?enable_caching: bool
  -> ?cache_max_age: float
  -> ?cache_max_size: int64
  -> ?enable_client_configs: bool
  -> ?force_syntax_diagnostics: bool
  -> ?fallback_storage_directory: string
  -> unit
  -> Lsp_types.config

(** [run ~platform ~cache_store_for ~config ~extensions] runs the server.

    [cache_store_for] is how a cache directory is opened.  An application with
    no file-system at hand passes {!Superbol_cache.Store.unavailable}, which
    disables caching: every lookup misses and nothing is stored. *)
val run
  : platform: Cobol_common.Platform.TYPES.platform
  -> cache_store_for: (root:string -> Superbol_cache.Store.TYPES.store)
  -> config: Lsp_types.config
  -> extensions: extensions
  -> Lsp_server.exit_status
