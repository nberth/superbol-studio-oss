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

open Ezcmd.V2
open EZCMD.TYPES

let platform = Superbol_platform.record

let cache_for ~cache_storage_location:storage_location ~rootdir
  : Superbol_cache.Cache.t =
  match Superbol_project.Cache.for_ ~rootdir ~storage_location with
  | Some (_, cache) ->
      cache
  | None ->
      Superbol_cache.Cache.disabled

(** Options of the server, common to the commands that run it. *)
type server_options =
  {
    enable_caching: bool;
    force_syntax_diagnostics: bool;
    storage_directory: string option;
  }

(** [server_args ()] gives the arguments of the options of the server, along
    with a function that gives their values once arguments are parsed. *)
let server_args () =
  let caching, caching_args =
    Arg_utils.switch `enable_disable ~name:"caching" ~default:true
  and syntax_diagnostics, syntax_diagnostics_args =
    Arg_utils.switch `force ~name:"syntax-diagnostics" ~default:false
      ~descr:"reporting of syntax error and hint diagnostics for dialects other \
              than COBOL85 (for which they are always enabled)"
  and storage_directory = ref None in
  (fun () -> { enable_caching = !caching;
               force_syntax_diagnostics = !syntax_diagnostics;
               storage_directory = !storage_directory }),
  caching_args @ syntax_diagnostics_args @ [
    ["storage-directory"], Arg.String (fun s -> storage_directory := Some s),
    EZCMD.info ~docv:"DIR"
      "Directory under which to store cache data --- prevents the creation \
       of a \"_superbol\" storage directory at the root of project trees.";
  ]

(** [lsp_config ~enable_client_configs ~verbose ?cache_pruning_policy options]
    is the configuration of a server run with [options]. *)
let lsp_config ~enable_client_configs ~verbose
    ?(cache_pruning_policy = Superbol_cache.Config.pruning_policy ())
    { enable_caching; force_syntax_diagnostics; storage_directory } =
  let setup =
    Project_commands.lsp_like_setup () ~enable_caching ?storage_directory
  in
  Cobol_lsp.Types.{
    lsp_project_layout = setup.layout;
    lsp_cache_for = cache_for ~cache_storage_location:setup.storage_location;
    lsp_cache_pruning_policy = cache_pruning_policy;
    lsp_cache_verbose = verbose;
    enable_client_configs;
    force_syntax_diagnostics;
  }

let run_lsp ?cache_max_age ?cache_max_size
    ?(extensions = Cobol_lsp.default_extensions) options =
  Cobol_preproc.Src_overlay.debug_oc := !Cobol_lsp.INTERNAL.Debug.debug_oc;
  let lsp_config =
    lsp_config options ~enable_client_configs:true ~verbose:true
      ~cache_pruning_policy:(Superbol_cache.Config.pruning_policy ()
                               ?cache_max_age ?cache_max_size)
  in
  match Cobol_lsp.run ~platform ~config:lsp_config ~extensions with
  | Ok () -> ()
  | Error exit_msg -> Pretty.error "%s@." exit_msg; exit 1

let js_specific_args =
  match Sys.backend_type with
  | Other "js_of_ocaml" ->
      let compat_info =
        EZCMD.info "(Ignored, passed by Visual Studio Code to LSP servers that \
                    are implemented as node modules)"
      in
      [ ["stdio"],           Arg.Bool   ignore, compat_info;
        ["node-ipc"],        Arg.Bool   ignore, compat_info;
        ["clientProcessId"], Arg.String ignore, compat_info ]
  | Native | Bytecode | Other _ ->
      []

let make_cmd ?extensions () =
  let server_options, server_args = server_args ()
  and cache_max_age = ref None
  and cache_max_size = ref None in
  EZCMD.sub "lsp"
    (fun () ->
       run_lsp (server_options ())
         ?cache_max_age:!cache_max_age
         ?cache_max_size:!cache_max_size
         ?extensions)
    ~doc:"run LSP server"
    ~args: (server_args @ js_specific_args @ [
        ["cache-size-limit"],
        Arg.String begin fun s -> match Arg_utils.size_of_string s with
          | Ok size -> cache_max_size := Some size
          | Error () -> Cobol_common.fatal "Not a size: %s" s
        end,
        EZCMD.info ~docv:"SIZE"
          "Keep each project's cache under that size, reclaiming the entries \
           least recently used whenever it is written out.  Accepts a suffix: \
           500M, 2G, 64k.  Unbounded by default.";

        ["cache-max-age"],
        Arg.Float (fun days -> cache_max_age := Some (days *. 86400.)),
        EZCMD.info ~docv:"DAYS"
          "Reclaim cache entries that have not been used for that many days. \
           Kept indefinitely by default.";
      ])
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Start a COBOL LSP server"
      ];
    ]

let cmd =
  make_cmd ()
