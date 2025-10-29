open Alsdiff_base

module Param = struct
  type t = {
    name : string;
    value : float;
    automation : int;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; _ } ->
      let value = Upath.get_float_attr_opt "/Manual" "Value" xml |> Option.value ~default:0.0 in
      let automation = Upath.get_int_attr_opt "/AutomationTarget" "Id" xml |> Option.value ~default: 0 in
      { name; value; automation }
    | _ -> failwith "Invalid XML element for creating Param"
end

type t = {
  id : int;
  device_name : string;
  preset_name : string;
  pointee : int;
  params : Param.t list;
} [@@deriving eq]

let create (xml : Xml.t) : t =
  match xml with
  | Xml.Element { name; _ } ->
    let id = Xml.get_int_attr "Id" xml in
    let preset_name = Upath.get_attr "/UserName" "Value" xml in
    let pointee = Upath.get_int_attr "/Pointee" "Id" xml in
    let params = Upath.find_all "/**/LomId/../Manual/.." xml |> List.map snd |> List.map Param.create in
    { id; device_name=name; preset_name; pointee; params }
  | _ -> failwith "Invalid XML element for creating Device"
