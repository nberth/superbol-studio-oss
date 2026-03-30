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

open EzCompat
open Ezcmd.V2
open EZCMD.TYPES

type dual_switch = [`enable_disable | `with_without | `boolean]
type single_switch = [`use | `force]

let dual_switch ?descr (kind: dual_switch) ~name ~default =
  let switch = ref default in
  let set, unset = match kind with
    | `enable_disable -> "Enable", "Disable"
    | `with_without   -> "With", "Without"
    | `boolean        -> "Set", "Clear"
  in
  let arg_set, arg_unset = match kind with
    | `enable_disable | `boolean -> name, "no-"^name
    | `with_without -> "with-"^name, "without-"^name
  in
  let default = match kind with
    | `enable_disable when default -> "enabled"
    | `enable_disable              -> "disabled"
    | `with_without when default   -> "with"
    | `with_without                -> "without"
    | `boolean when default        -> "true"
    | `boolean                     -> "false"
  in
  let descr = Option.value descr ~default:name in
  switch,
  [
    [arg_set], Arg.Set switch,
    Pretty.string_to EZCMD.info "%s %s (%s by default)" set descr default;

    [arg_unset], Arg.Clear switch,
    Pretty.string_to EZCMD.info "%s %s (%s by default)" unset descr default;
  ]

let single_switch ?descr (kind: single_switch) ~name ~default =
  let switch = ref default in
  let set = match kind with
    | `use when not default -> "Use"
    | `use -> "Don't use"
    | `force when not default -> "Force"
    | `force -> "Don't force"
  in
  let arg_set = match kind with
    | `use when not default -> name
    | `use -> "no-"^name
    | `force when not default -> "force-"^name
    | `force -> "default-"^name
  in
  let descr = Option.value descr ~default:name in
  switch,
  [
    [arg_set], Arg.Set switch,
    Pretty.string_to EZCMD.info "%s %s" set descr;
  ]

let switch ?descr = function
  | #dual_switch as kind -> dual_switch ?descr kind
  | #single_switch as kind -> single_switch ?descr kind

(* --- *)

let fold_comma_separated_spec ~available_values ~option_name ~f spec acc =
  EzString.split_simplify spec ',' |>
  List.fold_left begin fun (acc, unknowns) spec ->
    let spec' = String.(lowercase_ascii @@ trim @@ spec) in
    if spec' = "" then acc, unknowns else
      match
        List.find_map begin fun (sl, tag) ->
          if List.mem spec' sl then Some tag else None
        end available_values
      with
      | Some tag -> f tag acc, unknowns
      | None -> acc, StringSet.add spec unknowns
  end (acc, StringSet.empty) |>
  fun (acc, unknowns) ->
  if StringSet.is_empty unknowns then
    acc
  else
    raise @@ Stdlib.Arg.Bad
      Pretty.(to_string "@[Unknown@ arguments@ for@ `%s':@ %a@]"
                option_name
                (list ~fopen:"" ~fsep:",@ " ~fclose:"" string)
                (StringSet.elements unknowns))

let comma_separated_set ~name ?(alternate_names=[]) ~available_values ~default
    ~set descr =
  let values = ref default in                                      (* default *)
  let set_values_from_string s =
    values :=
      fold_comma_separated_spec ~available_values s !values
        ~option_name:("--"^name)
        ~f:set
  in
  values,
  [
    name :: alternate_names,
    Arg.String set_values_from_string,
    Pretty.string_to EZCMD.info
      "%s;@ accepts@ a@ comma-separated@ list@ of@ options.@ Accepted@ options@ \
       include@ %a." descr
      Pretty.(list ~fopen:"\"" ~fsep:"\",@ \"" ~fclose:"\"" string)
      List.(flatten @@ map fst available_values)
  ]
