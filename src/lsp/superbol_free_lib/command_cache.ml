(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)


(** Inspecting and reclaiming the caches the LSP server writes. *)

open Ezcmd.V2
open EZCMD.TYPES

module Cache = Superbol_cache.Cache
module Types = Superbol_cache.Types

(* The very configuration the server runs with, so that these commands look
   exactly where it writes. *)
let cache_config =
  (Cobol_lsp.config ~project_layout:Project.layout ()).cache_config

let cache_for dir =
  let rootdir =
    Superbol_project.rootdir_for ~layout:Project.layout
      ~filename:(Filename.concat dir "_")
  in
  match Cobol_lsp.Project_cache.cache_root ~config:cache_config ~rootdir with
  | None ->
      Pretty.error "Caching is turned off@."; None
  | Some root ->
      Some (root,
            Cache.create ~store:(Superbol_platform.Cache_store.create ~root ())
              ())

let pp_size ppf bytes =
  let rec pick n = function
    | _ :: (_ :: _ as units) when n >= 1024. -> pick (n /. 1024.) units
    | suffix :: _ -> n, suffix
    | [] -> n, "B"
  in
  let n, suffix = pick (Int64.to_float bytes) ["B"; "kiB"; "MiB"; "GiB"] in
  Pretty.print ppf "%.1f %s" n suffix

let pp_age ppf seconds =
  let days = seconds /. 86400. in
  if days >= 1. then Pretty.print ppf "%.0f day(s) ago"  days
  else if seconds >= 3600. then Pretty.print ppf "%.0f hour(s) ago" (seconds /. 3600.)
  else Pretty.print ppf "%.0f minute(s) ago" (seconds /. 60.)

(* --- *)

let status ~verbose dir =
  match cache_for dir with
  | None -> ()
  | Some (root, cache) ->
      match Cache.stats cache, Cache.entries cache with
      | Ok stats, Ok entries ->
          Pretty.out "Cache: %s@." root;
          Pretty.out "Format: %s, producer: %s@."
            stats.Types.stats_format_version stats.Types.stats_producer;
          Pretty.out "%d entries, %d values, %a@."
            stats.Types.stats_entries stats.Types.stats_objects
            pp_size stats.Types.stats_disk_usage;
          if verbose then begin
            let now = Unix.gettimeofday () in
            List.iter begin fun e ->
              Pretty.out "  %-40s %a  %d dependencies, used %a@."
                e.Types.info_label pp_size e.Types.info_disk_usage
                (List.length e.Types.info_witnesses)
                pp_age (now -. e.Types.info_last_hit)
            end entries
          end
      | _ ->
          Pretty.error "Could not read the cache at %s@." root

(** Accepts a plain number of bytes, or one with the usual suffix: [500M],
    [2G], [64k].  Powers of 1024, as the sizes this reports are. *)
let size_of_string s =
  let scale, digits = match s.[String.length s - 1] with
    | 'k' | 'K' -> 1024L, String.length s - 1
    | 'M' | 'm' -> 1048576L, String.length s - 1
    | 'G' | 'g' -> 1073741824L, String.length s - 1
    | _ -> 1L, String.length s
  in
  match Int64.of_string_opt (String.sub s 0 digits) with
  | Some n when n >= 0L -> Ok (Int64.mul n scale)
  | Some _ | None -> Error ()

let trim ~older_than ~max_size dir =
  match cache_for dir with
  | None -> ()
  | Some (root, cache) ->
      let policy =
        if older_than = None && max_size = None then Types.Never
        else Types.Bounded { max_age = Option.map (( *. ) 86400.) older_than;
                             max_size }
      in
      match Cache.prune cache ~policy with
      | Ok removed -> Pretty.out "Reclaimed %d entries in %s@." removed root
      | Error _ -> Pretty.error "Could not prune the cache at %s@." root

(* --- *)

let dir_arg dir =
  [ [], Arg.Anon (0, fun s -> dir := s),
    EZCMD.info ~docv:"DIR" "Directory within the project (default: `.')" ]

let status_cmd =
  let dir = ref "." and verbose = ref false in
  EZCMD.sub "cache status"
    (fun () -> status ~verbose:!verbose !dir)
    ~args:(dir_arg dir @
           [ ["l"], Arg.Set verbose,
             EZCMD.info "List every entry, not just the totals" ])
    ~doc:"Report on the cache of a project"
    ~man:[ `S "DESCRIPTION";
           `Blocks [ `P "Show where the cache of the project containing DIR \
                         lives, how much room it takes, and -- with $(b,-l) -- \
                         what each entry holds and when it was last used." ]]

let trim_cmd =
  let dir = ref "." and older_than = ref None and max_size = ref None in
  EZCMD.sub "cache trim"
    (fun () -> trim ~older_than:!older_than ~max_size:!max_size !dir)
    ~args:(dir_arg dir @
           [ ["older-than"],
             Arg.Float (fun d -> older_than := Some d),
             EZCMD.info ~docv:"DAYS"
               "Reclaim entries unused for that many days";
             ["max-size"],
             Arg.String begin fun s -> match size_of_string s with
               | Ok size -> max_size := Some size
               | Error () -> Pretty.failwith "Not a size: %s" s
             end,
             EZCMD.info ~docv:"SIZE"
               "Keep the cache under that size, reclaiming the entries least \
                recently used.  Accepts a suffix: 500M, 2G, 64k" ])
    ~doc:"Reclaim entries a project's cache no longer needs"
    ~man:[ `S "DESCRIPTION";
           `Blocks [ `P "Reclaim entries of the project containing DIR, along \
                         with every value they alone held.";
                     `P "$(b,--older-than) removes those unused for that many \
                         days; $(b,--max-size) removes the least recently used \
                         until the cache fits.  Both may be given, and with \
                         neither nothing is reclaimed but the leftovers of \
                         instances that died mid-write.  $(b,--older-than 0) \
                         empties the cache." ]]
