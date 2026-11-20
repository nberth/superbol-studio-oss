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

(** Output of the server when it has no LSP client. *)

module IO = Cobol_lsp.IO

let%expect_test "messages without a client" =
  IO.set_output (Messages_to Format.std_formatter);
  Fun.protect ~finally:(fun () -> IO.set_output Client) begin fun () ->
    IO.log_info "Loading project in %s" "/some/dir";
    IO.log_debug "A debug message";
    IO.notify_warn "Something@ odd@ happened";
    (* Messages other than those for logs and users are dropped. *)
    IO.send_diagnostics ~uri:(Lsp.Uri.of_path "/some/file.cob") [];
    IO.send_request @@
    Jsonrpc.Request.create ~id:(`Int 1) ~method_:"workspace/configuration" ();
    IO.send_response @@ Jsonrpc.Response.ok (`Int 2) `Null;
  end;
  [%expect {|
    [info] Loading project in /some/dir
    [log] A debug message
    [warning] Something odd happened |}]
