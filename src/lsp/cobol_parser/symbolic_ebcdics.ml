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

open Parser_diagnostics_types

open Cobol_common.Srcloc.INFIX

(** Decoding Symbolic EBCDIC in alphanumerics *)

let ebcdic_char i =
  (* TODO: (fixed/configurable tables) *)
  String.make 1 (Char.chr @@ Ebcdic.default.to_ascii.(i))

let decode_symbolic_ebcdics' ~quotation w =
  let acc_error e (acc, diags) = acc, e :: diags in
  let symbolic_ebcdic ~loc:_ = Text_categorizer.symbolic_ebcdic
  and alphanum_string ~loc:_ = Text_categorizer.alphanum_string in
  let str, diags =
    Cobol_common.Tokenizing.fold_tokens w ("", [])
      ~tokenizer:alphanum_string
      ~until:(function AEnd _ -> true | _ -> false)
      ~next_tokenizer:(function
          | AEBCDIC _
          | AStr (_, EBCDIC) | AUnexpected (_, EBCDIC) -> symbolic_ebcdic
          | AStr (_, STR) | AUnexpected (_, STR) | AEnd _ -> alphanum_string)
      ~f:begin fun t -> match ~&t with      (* TODO: (fixed/configurable tables) *)
        | AStr (s, _) ->
            fun (acc, diags) -> acc ^ s, diags
        | AEBCDIC i when i < 1 || i > 256 ->
            let stuff = Symbolic_EBCDIC_orginal i in
            acc_error @@ Unexpected { loc = ~@t; stuff }
        | AEBCDIC i ->
            fun (acc, diags) -> acc ^ ebcdic_char i, diags
        | AEnd { wellformed = true } ->
            Fun.id
        | AEnd { wellformed = false } ->
            acc_error @@ Malformed { loc = ~@w; stuff = Alphanumeric_literal }
        | AUnexpected (c, _) ->
            acc_error @@ Unexpected { loc = ~@t;
                                      stuff = Character_in_symbolic_EBCDIC c }
      end
  in
  Grammar_tokens.ALPHANUM { quotation; hexadecimal = false;
                            str;             (* CHECKME: not the given string *)
                            runtime_repr = Native_bytes } &@<- w,
  diags
