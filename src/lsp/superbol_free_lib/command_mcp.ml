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

(** [superbol mcp]: tools for language models (see {!Cobol_lsp.Tools}), over
    the Model Context Protocol, for AI agents other than those of an editor that
    runs the LSP server. *)

open Ezcmd.V2
open EZCMD.TYPES

module TOOLS = Cobol_lsp.Tools
module MCP = Ez_mcp.Types

let instructions =
  "Tools on the COBOL source files of the workspace, from the SuperBOL \
   language server: outlines, symbol search, problems, project settings, and \
   control flow.  Searching symbols and listing problems only cover files \
   whose analysis SuperBOL has cached (e.g. by its Visual Studio Code \
   extension, or with `superbol cache analyze`)."

(** [mcp_tool server tool] is [tool] for the MCP server, run with the LSP
    server held in [server], that may load projects and files as it runs. *)
let mcp_tool server (tool: TOOLS.tool) : Ez_mcp.Server.tool =
  {
    tool =
      MCP.Tool.create () ~name:tool.name ~title:tool.display_name
        ~description:tool.model_description ~input_schema:tool.input_schema
        ~annotations:(MCP.Tool_annotations.create ()
                        ~read_only_hint:true ~open_world_hint:false);
    call = begin fun arguments ->
      let TOOLS.{ text; is_error }, registry =
        TOOLS.call ~open_files:true !server ~name:tool.name ~arguments in
      server := registry;
      MCP.Tools_call.Result.text text
        ?is_error:(if is_error then Some true else None)
    end;
  }

(** [mcp_server ?workspace ?client_cobol_settings ?client_file_encoding
    options] is an MCP server that runs an LSP server with [options], for the
    workspace in [workspace] (the current directory by default).  Messages of
    the LSP server go where {!Cobol_lsp.IO.set_output} says. *)
let mcp_server ?workspace ?client_cobol_settings ?client_file_encoding
    ?(log = prerr_endline) options =
  Cobol_common.Platform.copy ~dst:Cobol_lsp.Platform.record
    ~src:Superbol_platform.record;
  let config =
    Command_lsp.lsp_config options ~enable_client_configs:false ~verbose:false
      ?client_cobol_settings ?client_file_encoding
  in
  let workspace =
    match workspace with
    | None -> Sys.getcwd ()
    | Some dir when Filename.is_relative dir ->
        Filename.concat (Sys.getcwd ()) dir
    | Some dir -> dir
  in
  let workspace_uri = Lsp.Uri.of_path workspace in
  let server =
    ref @@ Cobol_lsp.Server.init ~params:{
      config;
      root_uri = Some workspace_uri;
      workspace_folders = [workspace_uri];
      with_semantic_tokens = false;
      with_client_file_watcher = `no;
      with_client_config_watcher = false;
      position_encoding = `UTF16;
    }
  in
  (* Caches are never saved as a whole (see
     {!Cobol_lsp.Server.save_project_caches}), as an editor may be using them
     at the same time: only the analyses of files opened by tools are stored,
     as `superbol cache analyze' does. *)
  Ez_mcp.Server.create ~log {
    server_info = MCP.Implementation.create ()
        ~name:"superbol" ~title:"SuperBOL" ~version:Version.version;
    instructions = Some instructions;
    tools = List.map (mcp_tool server) (TOOLS.definitions ~with_editor:false);
  }

let run_mcp ?workspace ?client_cobol_settings ?client_file_encoding options =
  (* Standard output carries MCP messages only. *)
  Cobol_lsp.IO.set_output (Messages_to Fmt.stderr);
  let server =
    mcp_server options ?workspace ?client_cobol_settings ?client_file_encoding
  in
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  Ez_mcp.Server.run server stdin stdout

let cmd =
  let server_options, server_args = Command_lsp.server_args ()
  and encoding, encoding_args = Project_args.encoding ()
  and client_settings, client_settings_args =
    Project_args.client_settings () ~doc:
      "Configure projects that have no `superbol.toml' file as Visual Studio \
       Code does with these settings of its `superbol.cobol' section (e.g. \
       `{\"dialect\": \"ibm\"}')"
  and workspace = ref None in
  EZCMD.sub "mcp"
    (fun () ->
       run_mcp (server_options ())
         ?workspace:!workspace
         ?client_cobol_settings:(client_settings ())
         ?client_file_encoding:(encoding ()))
    ~doc:"run MCP server"
    ~args:(server_args @ encoding_args @ client_settings_args @ [
        ["workspace"], Arg.String (fun d -> workspace := Some d),
        EZCMD.info ~docv:"DIR"
          "Root directory of the workspace (the current directory by \
           default)";
      ])
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Start a Model Context Protocol (MCP) server on the standard input \
            and output, that gives AI agents tools on the COBOL source files \
            of the workspace: outlines, symbol search, problems, project \
            settings, and control flow.";
        `P "Searching symbols and listing problems only cover the files whose \
            analysis SuperBOL has cached, e.g. by its Visual Studio Code \
            extension, or with $(b,superbol cache analyze); options that \
            pertain to caches and settings should be the same as for these, \
            for the MCP server to find their analyses.";
      ];
    ]
