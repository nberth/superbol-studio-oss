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


open Lsp_imports
open Lsp_types

open Ez_file.V1.EzFile.OP

module CACHE = Superbol_cache.Cache
module CACHING = Superbol_cache.Caching
module WITNESS = Superbol_cache.Witness

(* --- *)

(** What is held for a project: the project itself, and which documents were
    open against it.  The documents' own entries are separate, so that editing
    one of them costs a miss on that one alone. *)
type project_index =
  {
    index_project: Lsp_project.cached;
    (* index_docs: string list;          (\* filenames, relative to the rootdir *\) *)
  }

(* Code: *)

(** Where a project's cache lives.  Both storage modes name a directory; the
    shared one keeps projects apart by the digest of their root. *)
let cache_root ~config ~rootdir =
  match config.cache_storage with
  | No_storage ->
      None
  | Store_in_file { relative_filename } ->
      Some (Lsp_project.string_of_rootdir rootdir // relative_filename)
  | Store_in_shared_dir { dirname } ->
      Some (dirname // Digest.(to_hex @@
                               string @@ Lsp_project.string_of_rootdir rootdir))

let cache_for ~config ~rootdir =
  match cache_root ~config ~rootdir with
  | None -> CACHE.disabled
  | Some root -> CACHE.create ~store:(!Lsp_platform.cache_store_for ~root) ()

(* --- Keys --- *)

(* Everything about the project that bears on how a document is preprocessed and
   parsed.  It takes part in every document key rather than being witnessed,
   because settings pushed by the client never reach a file we could witness. *)
let config_digest project =
  let config = Lsp_project.config project in
  match
    Marshal.to_string
      (Cobol_config.DIALECT.to_string @@
       Cobol_config.dialect config.cobol_config,
       config.source_format,
       config.source_format_by_extension,
       config.libpath,
       config.libexts) []
  with
  | s -> Some (Digest.string s)
  | exception Invalid_argument _ ->
      None              (* nothing here should be functional, but if ever *)

let project_key ~rootdir =
  CACHE.shallow_key ~label:"project"
    ~inputs:[Digest.string @@ Lsp_project.string_of_rootdir rootdir]

(* The document's contents take part in the key, rather than being witnessed:
   entries are published without ever replacing one another, so a document that
   is edited has to land under a key of its own. *)
let document_key ~project ~filename ~contents =
  Option.map begin fun config_digest ->
    CACHE.shallow_key ~label:"document"
      ~inputs:[Digest.string @@ WITNESS.canonical_path filename;
               Digest.string contents;
               config_digest]
  end (config_digest project)

let read_file filename =
  try Some (Lsp_platform.record.read_text_file filename)
  with Sys_error _ -> None

(* --- Saving --- *)

(* Keyed on what the editor is showing, which is what the cached artifacts were
   actually parsed from -- and not on what the file holds, which may still differ
   from it.  Since a document is restored by reading the file, keying it on
   anything else would make it restorable in a state it was never parsed in. *)
let store_document ~cache ~project doc =
  let cached = Lsp_document.to_cache doc in
  let filename = cached.doc_cache_filename in
  let absolute = Lsp_project.absolute_path_for ~filename project in
  let contents = Lsp.Text_document.text doc.Lsp_document.textdoc in
  match document_key ~project ~filename:absolute ~contents with
  | None -> None
  | Some key ->
      match
        CACHE.store cache ~key ~label:filename
          ~witnesses:cached.doc_cache_witnesses
          ~write_item:CACHING.write_item cached
      with
      | Ok () -> Some filename
      | Error _ -> None

(* let store_index ~cache ~quiet (Lsp_project.{ rootdir; _ } as project) index_docs = *)
(*   match *)
(*     CACHE.store cache ~key:(project_key ~rootdir) ~label:"project" *)
(*       ~witnesses:[]         (\* the configuration file is checked on the way in *\) *)
(*       ~write_item:CACHING.write_item *)
(*       { index_project = Lsp_project.to_cache project(\* ; *\) *)
(*         (\* index_docs = List.sort_uniq String.compare index_docs *\) } *)
(*   with *)
(*   | Ok () -> *)
(*       if not quiet then *)
(*         Lsp_io.log_info "Wrote cache for: %s" *)
(*           (Lsp_project.string_of_rootdir rootdir) *)
(*   | Error _ -> *)
(*       () *)

let save_project ~config (Lsp_project.{ rootdir; _ } (* as project *)) _docs =
  let cache = cache_for ~config ~rootdir in
  (* store_index ~cache ~quiet:false project @@ *)
  (* List.filter_map (store_document ~cache ~project) docs; *)
  (* The cache has just grown, so this is when it is worth seeing whether it
     still fits.  Only here, and not on every document saved: pruning walks the
     whole of it, and another instance already at it simply makes this a
     no-op. *)
  match CACHE.prune cache ~policy:config.cache_bounds with
  | Ok 0 | Error _ -> ()
  | Ok reclaimed ->
      Lsp_io.log_info "Reclaimed@ %d@ cache@ entries@ for:@ %s" reclaimed
        (Lsp_project.string_of_rootdir rootdir)

let do_save ~config docs =
  (* Pivot all active projects: associate projects with all their documents, and
     ignore any project that has none. *)
  URIMap.fold begin fun _ (Lsp_document.{ project; _ } as doc) ->
    Lsp_project.MAP.update project begin function
      | None -> Some [doc]
      | Some docs -> Some (doc :: docs)
    end
  end docs Lsp_project.MAP.empty |>
  Lsp_project.MAP.iter (save_project ~config)

(* --- Saving one document --- *)

(* (\** Reads back the list of documents already known for a project, so that *)
(*     caching one of them does not drop the others from the index. *\) *)
(* let indexed_documents ~cache ~rootdir = *)
(*   match *)
(*     CACHE.lookup cache ~key:(project_key ~rootdir) ~resolve:(fun _ -> true) *)
(*       ~read_item:CACHING.read_item *)
(*   with *)
(*   | Ok (Some { index_docs; _ }) -> index_docs *)
(*   | Ok None | Error _ -> [] *)

let do_save_document ~config (Lsp_document.{ project; _ } as doc) =
  let Lsp_project.{ rootdir; _ } = project in
  let cache = cache_for ~config ~rootdir in
  match store_document ~cache ~project doc with
  | None -> ()
  | Some filename ->
      (* The index has to name the document too, or a later session would never
         think to look it up. *)
      (* store_index ~cache ~quiet:true project @@ *)
      (* filename :: indexed_documents ~cache ~rootdir; *)
      if config.cache_verbose then
        Lsp_io.log_info "Wrote@ document@ cache@ for@ %s" filename

let save_document ~params:{ config = { cache_config = config; _ }; _ } doc =
  try do_save_document ~config doc with
  | Unix.Unix_error (e, op, args) ->
      if config.cache_verbose then
        Lsp_io.log_debug "While@ saving@ document@ cache:@ %s %s:@ %s"
          op args (Unix.error_message e)
  | e ->
      Lsp_error.internal
        "Exception@ caught@ while@ saving@ document@ cache:@ %a@." Fmt.exn e

let save ~params:{ config = { cache_config = config; _ }; _ } docs =
  try do_save ~config docs with
  | Unix.Unix_error (e, op, args) ->
      if config.cache_verbose then
        Lsp_io.log_debug "While@ saving@ project@ caches:@ %s %s:@ %s"
          op args (Unix.error_message e)
  | e ->
      Lsp_error.internal
        "Exception@ caught@ while@ saving@ project@ caches:@ %a@." Fmt.exn e

(* --- Loading --- *)

let load_document (* ~cache *) ~project ~params (* ~config *) ~filename ~contents =
  let config = params.config.cache_config in
  let Lsp_project.{ rootdir; _ } = project in
  let cache = cache_for ~config ~rootdir in
  let absolute = Lsp_project.absolute_path_for ~filename project in
  let resolve =
    Superbol_cache.Recorder.resolver ~platform:Lsp_platform.record ()
  in
  (* match read_file absolute with *)
  (* | None -> docs *)
  (* | Some contents -> *)
  match document_key ~project ~filename:absolute ~contents with
  | None ->
      None
  | Some key ->
      match CACHE.lookup cache ~key ~resolve ~read_item:CACHING.read_item with
      | Ok (Some cached) ->
          let doc = Lsp_document.of_cache ~project ~params ~contents cached in
          if config.cache_verbose then
            Lsp_io.log_info "Successfully@ read@ document@ cache@ for@ %s"
              (Lsp.Uri.to_string @@ Lsp_document.uri doc);
          Some doc
          (* URIMap.add (Lsp_document.uri doc) doc docs *)
      | Ok None | Error _ ->
          if config.cache_verbose then
            Lsp_io.log_info "Not@ reading@ document@ cache@ for@ %s" filename;
          None
          (* docs *)

let load ~rootdir ~layout ~params =
  let config = params.config.cache_config in
  let cache = cache_for ~config ~rootdir in
  match
    CACHE.lookup cache ~key:(project_key ~rootdir) ~resolve:(fun _ -> true)
      ~read_item:CACHING.read_item
  with
  | Ok (Some { index_project(* ; index_docs = _ *) }) ->
      begin match Lsp_project.of_cache ~rootdir ~layout index_project with
        | project ->
            Lsp_io.log_info "Successfully@ read@ project@ cache@ for@ %s"
              Lsp.Uri.(to_string @@ Lsp_project.rooturi project);
            Some project
            (* List.fold_left *)
            (*   (fun docs filename -> *)
            (*      load_document ~cache ~project ~params ~config filename docs) *)
            (* URIMap.empty (\* index_docs *\) *)
        | exception _ ->
            None
            (* URIMap.empty          (\* outdated configuration file, most likely *\) *)
      end
  | Ok None | Error _ ->
      None
      (* URIMap.empty *)
