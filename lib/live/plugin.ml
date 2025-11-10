open Alsdiff_base
open Alsdiff_base.Diff

module PluginParam = struct
  type param_value =
    | Float of float
    | Int of int
    | Bool of bool
    | Enum of int
  [@@deriving eq]

  type t = {
    id : int;                   (* ParameterId *)
    name : string;              (* ParameterName *)
    index : int;                (* VisualIndex *)
    value : param_value;        (* Manual *)
    automation : int;           (* ParameterValue/AutomationTarget *)
    modulation : int;           (* ParameterValue/ModulationTarget *)
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let id = Upath.get_int_attr "/ParameterId" "Value" xml in
    let name = Upath.get_attr "/ParameterName" "Value" xml in
    let index = Upath.get_int_attr "/VisualIndex" "Value" xml in

    let value =
      let parameter_type = Xml.get_name xml in
      match  parameter_type with
      | "PluginFloatParameter" ->
        Float (Upath.get_float_attr "/ParameterValue/Manual" "Value" xml)
      | "PluginIntParameter" ->
        Int (Upath.get_int_attr "/ParameterValue/Manual" "Value" xml)
      | "PluginBoolParameter" ->
        Bool (Upath.get_bool_attr "/ParameterValue/Manual" "Value" xml)
      | _ -> failwith ("Invalid parameter type " ^ parameter_type)
    in

    let automation = Upath.get_int_attr "/ParameterValue/AutomationTarget" "Id" xml in
    let modulation = Upath.get_int_attr "/ParameterValue/ModulationTarget" "Id" xml in

    { id; name; index; value; automation; modulation }

  module Patch = struct
    type t = {
      name : string simple_flat_change;
      index : int simple_flat_change;
      value : param_value simple_flat_change;
      automation : int simple_flat_change;
      modulation : int simple_flat_change;
    }

    let is_empty patch =
      patch.name = `Unchanged &&
      patch.index = `Unchanged &&
      patch.value = `Unchanged &&
      patch.automation = `Unchanged &&
      patch.modulation = `Unchanged
  end

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let diff (old_param : t) (new_param : t) : Patch.t =
    if old_param.id <> new_param.id then
      failwith "cannot diff two PluginParams with different Ids"
    else
      let name_change = diff_value old_param.name new_param.name in
      let index_change = diff_value old_param.index new_param.index in
      let value_change = diff_value old_param.value new_param.value in
      let automation_change = diff_value old_param.automation new_param.automation in
      let modulation_change = diff_value old_param.modulation new_param.modulation in

      {
        Patch.name = name_change;
        index = index_change;
        value = value_change;
        automation = automation_change;
        modulation = modulation_change;
      }
end


module PluginDesc = struct
  type plugin_type = Vst2 | Vst3 | Auv2 [@@deriving eq]

  type t = {
    name : string;
    uid : string;
    plugin_type : plugin_type;
    state : string;
  } [@@deriving eq]

  (** Trim all whitespace characters from a string *)
  let trim_blob_str s =
    let len = String.length s in
    let rec trim_left i =
      if i >= len then i
      else
        match s.[i] with
        | ' ' | '\t' | '\n' | '\r' -> trim_left (i + 1)
        | _ -> i
    in
    let rec trim_right i =
      if i < 0 then -1
      else
        match s.[i] with
        | ' ' | '\t' | '\n' | '\r' -> trim_right (i - 1)
        | _ -> i
    in
    let left = trim_left 0 in
    let right = trim_right (len - 1) in
    if left > right then ""
    else String.sub s left (right - left + 1)

  let parse_uid xml =
    let fields = Upath.find_all "/'Fields\\.[0-9]+$'" xml in
    let sorted_fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
    sorted_fields
    |> List.map (fun (_, field_xml) -> Xml.get_attr "Value" field_xml)
    |> String.concat "-"

  let create (xml : Xml.t) : t =
    (* Extract plugin type based on the element name *)
    let plugin_info_xml = Xml.get_childs xml |> List.hd in

    let plugin_type =
      match Xml.get_name plugin_info_xml with
      | "Vst3PluginInfo" -> Vst3
      | "VstPluginInfo" -> Vst2
      | "AuPluginInfo" -> Auv2
      | name -> failwith ("Unsupported plugin type: " ^ name)
    in

    (* Get plugin name *)
    let name = Upath.get_attr "/Name" "Value" plugin_info_xml in

    (* Get UID *)
    let uid = Upath.find "/Uid" plugin_info_xml |> snd |> parse_uid in

    (* Get processor state - look for it in the preset first, then in the main plugin info *)
    let state =
      match Upath.find_opt "/Preset/Vst3Preset/ProcessorState" plugin_info_xml with
      | Some (_, state_xml) ->
        (* Get all text content and trim it *)
        let content =
          state_xml
          |> Xml.get_childs
          |> List.filter_map (function Xml.Data { value; _ } -> Some value | _ -> None)
          |> String.concat ""
          |> trim_blob_str
        in
        content
      | None -> ""
    in

    { name; uid; plugin_type; state }

  module Patch = struct
    type t = {
      name : string simple_flat_change;
      uid : string simple_flat_change;
      plugin_type : plugin_type simple_flat_change;
      state : string simple_flat_change;
    }

    let is_empty patch =
      patch.name = `Unchanged &&
      patch.uid = `Unchanged &&
      patch.plugin_type = `Unchanged &&
      patch.state = `Unchanged
  end

  let diff (old_desc : t) (new_desc : t) : Patch.t =
    if old_desc.uid <> new_desc.uid then
      failwith "cannot diff two PluginDesc with different UIDs"
    else
      let name_change = diff_value old_desc.name new_desc.name in
      let uid_change = diff_value old_desc.uid new_desc.uid in
      let plugin_type_change = diff_value old_desc.plugin_type new_desc.plugin_type in
      let state_change = diff_value old_desc.state new_desc.state in

      {
        Patch.name = name_change;
        uid = uid_change;
        plugin_type = plugin_type_change;
        state = state_change;
      }

  let has_same_id a b = a.uid = b.uid

  let id_hash t = Hashtbl.hash t.uid

end


module PluginDevice = struct
  type t = {
    id : int;
    pointee : int;
    enabled : Device.DeviceParam.t;
    desc : PluginDesc.t;
    params : PluginParam.t list;
    (* TODO: Support sidechain and MPE settigns *)
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    (* Get device ID *)
    let id = Xml.get_int_attr "Id" xml in

    (* Get pointee ID *)
    let pointee = Upath.get_int_attr "/Pointee" "Id" xml in
    let enabled = Upath.find "/On" xml |> snd |> Device.DeviceParam.create in

    (* Find the PluginDesc element *)
    let plugin_desc_xml = Upath.find "/PluginDesc" xml |> snd in
    let desc = PluginDesc.create plugin_desc_xml in

    (* Extract all plugin parameters *)
    let params =
      Upath.find_all "/ParameterList/*" xml
      |> List.map snd
      |> List.map PluginParam.create

      (* ParameterId=-1 is possibly an invisible/hidden parameter that we shouldn't
         care. Its doesn't even have a ParameterName and an valid VisualIndex *)
      |> List.filter (fun x -> x.PluginParam.id <> -1)

    in

    { id; pointee; enabled; desc; params }

  module Patch = struct
    type param_change = (PluginParam.t, PluginParam.Patch.t) structured_change

    type t = {
      pointee : int simple_flat_change;
      enabled : Device.DeviceParam.Patch.t simple_structured_change;
      desc : PluginDesc.Patch.t simple_structured_change;
      params : param_change list;
    }

    let is_empty patch =
      patch.pointee = `Unchanged &&
      patch.enabled = `Unchanged &&
      patch.desc = `Unchanged &&
      List.for_all (function
          | `Unchanged -> true
          | _ -> false
        ) patch.params
  end

  let has_same_id a b = a.id = b.id

  let diff (old_device : t) (new_device : t) : Patch.t =
    if not (has_same_id old_device new_device) then
      failwith "cannot diff two PluginDevices with different IDs"
    else
      let pointee_change = diff_value old_device.pointee new_device.pointee in

      let enabled_change =
        diff_structured_value (module Device.DeviceParam) old_device.enabled new_device.enabled
      in

      let desc_change =
        diff_structured_value (module PluginDesc) old_device.desc new_device.desc
      in

      let params_change =
        diff_list_id (module PluginParam) old_device.params new_device.params
        |> List.map @@ structured_change_of_flat (module PluginParam)
      in

      {
        Patch.pointee = pointee_change;
        enabled = enabled_change;
        desc = desc_change;
        params = params_change;
      }

end
