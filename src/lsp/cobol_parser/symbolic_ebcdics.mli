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

(* Note: unused for now. *)

(** {1 Alphanumerics with symbolic EBCDIC characters} *)

(** [decode_symbolic_ebcdics' ~quotation s'] decodes the symbolic EBCDIC
    characters from the localized string [s'], and returns the resulting
    {!Grammar_tokens.ALPHANUM} token and a set of diagnostics.  In case of
    errors, the alphanumeric token returned may represent part of the encoded
    input. *)
val decode_symbolic_ebcdics'
  : quotation: Cobol_ptree.alphanum_quote
  -> string Cobol_common.Srcloc.with_loc
  -> Grammar_tokens.token Cobol_common.Srcloc.with_loc *
     Parser_diagnostics_types.error list
