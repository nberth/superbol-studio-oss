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

(** Messages of the Model Context Protocol, restricted to what servers that
    provide tools need.

    Types follow the published JSON schemas of the protocol
    (https://github.com/modelcontextprotocol/modelcontextprotocol/tree/main/schema).
    Fields are named after those of the schemas, in snake case.  Decoders
    ignore unknown fields, so that messages of newer protocol versions can be
    read; encoders of results take the protocol version of the request, since
    what results must hold depends on it. *)

open Json

module Protocol_version = struct
  type t = string

  let v2024_11_05 = "2024-11-05"
  let v2025_03_26 = "2025-03-26"
  let v2025_06_18 = "2025-06-18"
  let v2025_11_25 = "2025-11-25"
  let v2026_07_28 = "2026-07-28"

  (** Supported versions, most recent first. *)
  let supported =
    [ v2026_07_28; v2025_11_25; v2025_06_18; v2025_03_26; v2024_11_05 ]

  let is_supported v = List.mem v supported

  (** Most recent version that starts sessions with [initialize]. *)
  let latest_with_initialization = v2025_11_25

  (** Whether requests of version [v] are self-contained, i.e., carry their
      protocol version and the capabilities of the client in their [_meta]
      field, and results hold a [resultType] (from version 2026-07-28 on).
      Versions are dates, which compare as strings. *)
  let is_stateless v =
    String.compare v v2026_07_28 >= 0
end

module Implementation = struct
  type t =
    {
      name: string;
      title: string option;
      version: string;
    }

  let create ?title ~name ~version () = { name; title; version }

  let t_of_yojson json =
    { name = string (field "name" json);
      title = string_opt "title" json;
      version = string (field "version" json) }

  let yojson_of_t { name; title; version } =
    obj [ "name", Some (str name);
          "title", opt str title;
          "version", Some (str version) ]
end

(** Contents of the [_meta] field of requests.  From version 2026-07-28 on,
    every request carries the protocol version, and the information about
    the client that was previously given with [initialize]. *)
module Request_meta = struct
  type t =
    {
      protocol_version: Protocol_version.t option;
      client_info: Implementation.t option;
      client_capabilities: Json.t option;
      progress_token: Json.t option;
    }

  let empty =
    { protocol_version = None; client_info = None;
      client_capabilities = None; progress_token = None }

  let prefix = "io.modelcontextprotocol/"

  let t_of_yojson json =
    { protocol_version = string_opt (prefix ^ "protocolVersion") json;
      client_info = Option.map Implementation.t_of_yojson @@
        field_opt (prefix ^ "clientInfo") json;
      client_capabilities = field_opt (prefix ^ "clientCapabilities") json;
      progress_token = field_opt "progressToken" json }

  let yojson_of_t { protocol_version; client_info; client_capabilities;
                    progress_token } =
    obj [ prefix ^ "protocolVersion", opt str protocol_version;
          prefix ^ "clientInfo", opt Implementation.yojson_of_t client_info;
          prefix ^ "clientCapabilities", client_capabilities;
          "progressToken", progress_token ]

  (** [of_params params] is the [_meta] field of request parameters. *)
  let of_params = function
    | None -> empty
    | Some params ->
        Option.fold ~none:empty ~some:t_of_yojson (field_opt "_meta" params)

  let to_params meta fields =
    obj (("_meta", if meta = empty then None else Some (yojson_of_t meta))
         :: fields)
end

(** Caching hints of some results, from version 2026-07-28 on. *)
module Cache = struct
  type scope = [ `Public | `Private ]

  type t =
    {
      scope: scope;
      ttl_ms: int;
    }

  (** Results that are immediately stale, for the client only. *)
  let default = { scope = `Private; ttl_ms = 0 }

  let t_of_yojson json =
    { scope = (match string_opt "cacheScope" json with
          | Some "public" -> `Public
          | _ -> `Private);
      ttl_ms = Option.fold ~none:0 ~some:int (field_opt "ttlMs" json) }

  let fields { scope; ttl_ms } =
    [ "cacheScope", Some (str (match scope with
          | `Public -> "public"
          | `Private -> "private"));
      "ttlMs", Some (`Int ttl_ms) ]
end

(** Fields that every result holds, depending on the protocol version. *)
let result_fields ~version ?server_info () =
  if Protocol_version.is_stateless version then
    [ "resultType", Some (str "complete");
      "_meta", opt (fun info ->
          obj [ Request_meta.prefix ^ "serverInfo",
                Some (Implementation.yojson_of_t info) ]) server_info ]
  else []

let cache_fields ~version cache =
  if Protocol_version.is_stateless version then Cache.fields cache else []

module Server_capabilities = struct
  type tools =
    {
      list_changed: bool option;
    }

  type t =
    {
      tools: tools option;
    }

  let t_of_yojson json =
    { tools = Option.map (fun t -> { list_changed = bool_opt "listChanged" t })
          (field_opt "tools" json) }

  let yojson_of_t { tools } =
    obj [ "tools", opt (fun { list_changed } ->
        obj [ "listChanged", opt (fun b -> `Bool b) list_changed ]) tools ]
end

module Tool_annotations = struct
  type t =
    {
      title: string option;
      read_only_hint: bool option;
      destructive_hint: bool option;
      idempotent_hint: bool option;
      open_world_hint: bool option;
    }

  let create ?title ?read_only_hint ?destructive_hint ?idempotent_hint
      ?open_world_hint () =
    { title; read_only_hint; destructive_hint; idempotent_hint;
      open_world_hint }

  let t_of_yojson json =
    { title = string_opt "title" json;
      read_only_hint = bool_opt "readOnlyHint" json;
      destructive_hint = bool_opt "destructiveHint" json;
      idempotent_hint = bool_opt "idempotentHint" json;
      open_world_hint = bool_opt "openWorldHint" json }

  let yojson_of_t { title; read_only_hint; destructive_hint;
                    idempotent_hint; open_world_hint } =
    let bool b = `Bool b in
    obj [ "title", opt str title;
          "readOnlyHint", opt bool read_only_hint;
          "destructiveHint", opt bool destructive_hint;
          "idempotentHint", opt bool idempotent_hint;
          "openWorldHint", opt bool open_world_hint ]
end

module Tool = struct
  type t =
    {
      name: string;
      title: string option;
      description: string option;
      input_schema: Json.t;          (** JSON schema of an object *)
      output_schema: Json.t option;
      annotations: Tool_annotations.t option;
    }

  let create ?title ?description ?output_schema ?annotations ~name
      ~input_schema () =
    { name; title; description; input_schema; output_schema; annotations }

  let t_of_yojson json =
    { name = string (field "name" json);
      title = string_opt "title" json;
      description = string_opt "description" json;
      input_schema = field "inputSchema" json;
      output_schema = field_opt "outputSchema" json;
      annotations = Option.map Tool_annotations.t_of_yojson
          (field_opt "annotations" json) }

  let yojson_of_t { name; title; description; input_schema; output_schema;
                    annotations } =
    obj [ "name", Some (str name);
          "title", opt str title;
          "description", opt str description;
          "inputSchema", Some input_schema;
          "outputSchema", output_schema;
          "annotations", opt Tool_annotations.yojson_of_t annotations ]
end

module Content_block = struct
  type t =
    | Text of string
    | Other of Json.t    (** images, audio, resources... kept as they are *)

  let t_of_yojson json =
    match string_opt "type" json with
    | Some "text" -> Text (string (field "text" json))
    | _ -> Other json

  let yojson_of_t = function
    | Text text -> obj [ "type", Some (str "text"); "text", Some (str text) ]
    | Other json -> json
end

module Initialize = struct
  module Params = struct
    type t =
      {
        protocol_version: Protocol_version.t;
        capabilities: Json.t;              (** capabilities of the client *)
        client_info: Implementation.t;
      }

    let t_of_yojson json =
      { protocol_version = string (field "protocolVersion" json);
        capabilities = Option.value ~default:(`Assoc [])
            (field_opt "capabilities" json);
        client_info = Implementation.t_of_yojson (field "clientInfo" json) }

    let yojson_of_t { protocol_version; capabilities; client_info } =
      obj [ "protocolVersion", Some (str protocol_version);
            "capabilities", Some capabilities;
            "clientInfo", Some (Implementation.yojson_of_t client_info) ]
  end

  module Result = struct
    type t =
      {
        protocol_version: Protocol_version.t;
        capabilities: Server_capabilities.t;
        server_info: Implementation.t;
        instructions: string option;
      }

    let t_of_yojson json =
      { protocol_version = string (field "protocolVersion" json);
        capabilities = Server_capabilities.t_of_yojson
            (field "capabilities" json);
        server_info = Implementation.t_of_yojson (field "serverInfo" json);
        instructions = string_opt "instructions" json }

    let yojson_of_t { protocol_version; capabilities; server_info;
                      instructions } =
      obj [ "protocolVersion", Some (str protocol_version);
            "capabilities", Some (Server_capabilities.yojson_of_t capabilities);
            "serverInfo", Some (Implementation.yojson_of_t server_info);
            "instructions", opt str instructions ]
  end
end

(** [server/discover], that replaces [initialize] from version 2026-07-28 on. *)
module Discover = struct
  module Params = struct
    type t =
      {
        meta: Request_meta.t;
      }

    let t_of_yojson json =
      { meta = Request_meta.of_params (Some json) }

    let yojson_of_t { meta } =
      Request_meta.to_params meta []
  end

  module Result = struct
    type t =
      {
        supported_versions: Protocol_version.t list;
        capabilities: Server_capabilities.t;
        instructions: string option;
        cache: Cache.t;
      }

    let t_of_yojson json =
      { supported_versions = list string (field "supportedVersions" json);
        capabilities = Server_capabilities.t_of_yojson
            (field "capabilities" json);
        instructions = string_opt "instructions" json;
        cache = Cache.t_of_yojson json }

    let yojson_of_t ~version ?server_info
        { supported_versions; capabilities; instructions; cache } =
      obj ([ "supportedVersions", Some (`List (List.map str
                                                 supported_versions));
             "capabilities", Some (Server_capabilities.yojson_of_t
                                     capabilities);
             "instructions", opt str instructions ] @
           Cache.fields cache @
           result_fields ~version ?server_info ())
  end
end

module Tools_list = struct
  module Params = struct
    type t =
      {
        cursor: string option;
        meta: Request_meta.t;
      }

    let t_of_yojson json =
      { cursor = string_opt "cursor" json;
        meta = Request_meta.of_params (Some json) }

    let yojson_of_t { cursor; meta } =
      Request_meta.to_params meta [ "cursor", opt str cursor ]
  end

  module Result = struct
    type t =
      {
        tools: Tool.t list;
        next_cursor: string option;
        cache: Cache.t;
      }

    let t_of_yojson json =
      { tools = list Tool.t_of_yojson (field "tools" json);
        next_cursor = string_opt "nextCursor" json;
        cache = Cache.t_of_yojson json }

    let yojson_of_t ~version ?server_info { tools; next_cursor; cache } =
      obj ([ "tools", Some (`List (List.map Tool.yojson_of_t tools));
             "nextCursor", opt str next_cursor ] @
           cache_fields ~version cache @
           result_fields ~version ?server_info ())
  end
end

module Tools_call = struct
  module Params = struct
    type t =
      {
        name: string;
        arguments: (string * Json.t) list;
        meta: Request_meta.t;
      }

    let t_of_yojson json =
      { name = string (field "name" json);
        arguments = Option.fold ~none:[] ~some:assoc
            (field_opt "arguments" json);
        meta = Request_meta.of_params (Some json) }

    let yojson_of_t { name; arguments; meta } =
      Request_meta.to_params meta [ "name", Some (str name);
                                    "arguments", Some (`Assoc arguments) ]
  end

  module Result = struct
    type t =
      {
        content: Content_block.t list;
        structured_content: Json.t option;
        is_error: bool option;
      }

    (** [text ?is_error s] is a result made of a single text block [s]. *)
    let text ?is_error s =
      { content = [ Content_block.Text s ]; structured_content = None;
        is_error }

    let t_of_yojson json =
      { content = list Content_block.t_of_yojson (field "content" json);
        structured_content = field_opt "structuredContent" json;
        is_error = bool_opt "isError" json }

    let yojson_of_t ~version ?server_info
        { content; structured_content; is_error } =
      obj ([ "content", Some (`List (List.map Content_block.yojson_of_t
                                         content));
             "structuredContent", structured_content;
             "isError", opt (fun b -> `Bool b) is_error ] @
           result_fields ~version ?server_info ())
  end
end

(** Results of requests that return nothing, like [ping]. *)
module Empty_result = struct
  let yojson_of_t ~version ?server_info () =
    obj (result_fields ~version ?server_info ())
end
