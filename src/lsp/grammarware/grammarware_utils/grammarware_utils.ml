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

module Make (Params: sig val name: string val filename: string end) = struct
  module Grammar = MenhirSdk.Cmly_read.Read (Params)
  open Grammar

  (* --- *)

  let pp_functor_parameters =
    Fmt.(list (fmt "(%s)"))

  let pp_extension_module ppf pp_struct =
    Fmt.pf ppf
      "(* Caution: this file was automatically generated from %s; do not edit *)\
       @\n[@@@@@@warning \"-33\"] (* <- do not warn on unused opens *)\
       @\n[@@@@@@warning \"-27\"] (* <- do not warn on unused variabes *)\
       @\n@\n" Params.filename;
    let all_parameters =
      let parameters =
        List.filter (Attribute.has_label @@ Params.name ^ ".parameter")
          Grammar.attributes |>
        List.map Attribute.payload
      in
      Grammar.parameters @ parameters
    in
    if all_parameters <> [] then
      Fmt.pf ppf
        "@[<2>@[<2>module@ Make@ %a@] = struct@\n%t@]@\nend@\n"
        pp_functor_parameters all_parameters
        pp_struct
    else
      pp_struct ppf

  let grammar_module =
    String.capitalize_ascii (Filename.basename Grammar.basename)

  let grammar_params =
    List.map (fun p -> List.hd (String.split_on_char ':' p)) Grammar.parameters

  let pp_grammar_open ppf =
    if grammar_params = [] then
      Fmt.pf ppf "@[<2>module@ Grammar@ =@ %s@]@\n" grammar_module
    else
      Fmt.pf ppf "@[<2>module@ Grammar@ =@ %s.Make@ %a@]@\n" grammar_module
        pp_functor_parameters grammar_params;
    Fmt.pf ppf "@[<2>open@ Grammar@]@\n"

  let pp_grammar_functor_application ppf functor_name =
    Fmt.pf ppf "@[<2>module@ %s@ =@ %s.Make@ %a@]@\n"
      functor_name functor_name
      pp_functor_parameters grammar_params

  let pp_header ppf =
    List.iter begin fun a ->
      if Attribute.has_label "header" a ||
         Attribute.has_label (Params.name ^ ".header") a then
        Fmt.pf ppf "%s@\n" (Attribute.payload a)
    end Grammar.attributes

end
