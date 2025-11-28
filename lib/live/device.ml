open Alsdiff_base
open Alsdiff_base.Diff
open Alsdiff_base.Equality


exception Not_implemented of string

type enum_desc = {
  min : int;
  max : int;
  enums : string array;
} [@@deriving eq]

type param_value =
  | Float of float
  | Int of int
  | Bool of bool
  | Enum of int * enum_desc
[@@deriving eq]


(* ================== Common modules ================== *)
module DeviceParam = struct

  type macro_mapping = {
    id : int;                   (* macro id *)
    low : int;                  (* mapping range *)
    high : int;
  } [@@deriving eq]


  (** Represents a single device parameter with a name, a value of a mixed type,
      and an automation ID. *)
  type t = {
    name : string;
    value : param_value;
    automation : int;
    modulation : int;
    mapping : macro_mapping option;
  } [@@deriving eq]

  let has_same_id a b = a.name = b.name (* each parameter has their unique names in the device *)

  let id_hash t = Hashtbl.hash t.name

  (** [value_of_string_opt s] attempts to parse a string [s] into a [value] type.
      For device parameters, we always prefer float values for numeric parameters,
      as device parameters are typically continuous values even when they appear
      as whole numbers. This is a private helper function. *)
  let value_of_string_opt (s : string) : param_value option =
    match float_of_string_opt s with
    | Some f -> Some (Float f)
    | None ->
      (match bool_of_string_opt s with
       | Some b -> Some (Bool b)
       | None ->
         (* Try int as last resort for values that can't be parsed as float *)
         (match int_of_string_opt s with
          | Some i -> Some (Int i)
          | None -> None))

  (** Extract range from MidiControllerRange (continuous parameters) *)
  let extract_continuous_range (xml : Xml.t) : (int * int) option =
    match
      (Upath.get_int_attr_opt "/MidiControllerRange/Min" "Value" xml,
       Upath.get_int_attr_opt "/MidiControllerRange/Max" "Value" xml)
    with
    | (Some min_val, Some max_val) ->
      Some (min_val, max_val)
    | _ -> None

  (** Extract range from MidiCCOnOffThresholds (On/Off parameters) *)
  let extract_onoff_range (xml : Xml.t) : (int * int) option =
    match
      (Upath.get_int_attr_opt "/MidiCCOnOffThresholds/Min" "Value" xml,
       Upath.get_int_attr_opt "/MidiCCOnOffThresholds/Max" "Value" xml)
    with
    | (Some min_val, Some max_val) -> Some (min_val, max_val)
    | _ -> None

  (** Parse macro mapping from XML element *)
  let parse_macro_mapping (xml : Xml.t) : macro_mapping option =
    match Upath.get_attr_opt "/KeyMidi/ControllerMapMode" "Value" xml with
    | Some "0" ->
      (match Upath.get_int_attr_opt "/KeyMidi/NoteOrController" "Value" xml with
       | Some macro_id ->
         (match extract_continuous_range xml with
          | Some (low, high) -> Some { id = macro_id; low; high }
          | None -> extract_onoff_range xml |> Option.map (fun (low, high) -> { id = macro_id; low; high }))
       | None -> None)
    | _ -> None

  (** [create xml] creates a device parameter from an XML element.
      It raises [Failure "Invalid XML element for creating DeviceParam"] if the XML
      is not a valid element, and raises [Failure "Failed to parse device parameter"]
      if the parameter value cannot be parsed. *)
  let create (path : string) (xml : Xml.t) : t =
    match xml with
    | Xml.Element _ ->
      let name =
        let path_parts = String.split_on_char '/' path in
        let non_empty_parts = List.filter (fun s -> s <> "") path_parts in
        match non_empty_parts with
        | [] -> ""
        | [single] -> single (* Only one part, return it as-is *)
        | _ :: rest -> String.concat "/" rest (* Skip first part, join rest *)
      in
      let automation =
        Upath.get_int_attr_opt "/AutomationTarget" "Id" xml
        |> Option.value ~default:0
      in
      let modulation =
        Upath.get_int_attr_opt "/ModulationTarget" "Id" xml
        |> Option.value ~default:0
      in
      let value_str_opt = Upath.get_attr_opt "/Manual" "Value" xml in
      let value =
        match value_str_opt with
        | Some s ->
          (match value_of_string_opt s with
           | Some v -> v
           | None -> failwith "Failed to parse device parameter")
        | None ->
          (* Default to 0.0 when Manual element is missing *)
          Float 0.0
      in
      let mapping = parse_macro_mapping xml in
      { name; value; automation; modulation; mapping }
    | _ -> failwith "Invalid XML element for creating DeviceParam"

  let create_from_upath_find (path, xml) = create path xml

  module Patch = struct
    type t = {
      value : param_value simple_flat_change;
      automation : int simple_flat_change;
      modulation : int simple_flat_change;
    }

    let is_empty = function
      | { value = `Unchanged; automation = `Unchanged; modulation = `Unchanged } -> true
      | _ -> false
  end

  let diff (old_param : t) (new_param : t) : Patch.t =
    if old_param.name <> new_param.name then
      failwith "cannot diff two DeviceParams with different names"
    else
      let value_change = diff_value old_param.value new_param.value in
      let automation_change = diff_value old_param.automation new_param.automation in
      let modulation_change = diff_value old_param.modulation new_param.modulation in
      { value = value_change; automation = automation_change; modulation = modulation_change }
end


module PresetRef = struct
  type preset_type =
    | UserPreset
    | DefaultPreset
  [@@deriving eq]

  type t = {
    id : int;                   (* not unique *)
    name : string;
    preset_type : preset_type;
    relative_path : string;
    path : string;
    pack_name : string;
    pack_id : int;
    file_size : int;
    crc : int;
  } [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name=tag; childs; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let preset_type =
        match tag with
        | "FilePresetRef" -> UserPreset
        | "AbletonDefaultPresetRef" -> DefaultPreset
        | _ -> failwith ("Unknown PresetRef type" ^ tag)
      in

      (* Extract FileRef element *)
      let file_ref_xml =
        try List.find (function
          | Xml.Element { name = "FileRef"; _ } -> true
          | _ -> false) childs
        with Not_found -> failwith "FileRef element not found in PresetRef"
      in

      (* Extract all the required fields from FileRef *)
      let relative_path = Upath.get_attr "/RelativePath" "Value" file_ref_xml in
      let path = Upath.get_attr "/Path" "Value" file_ref_xml in

      let preset_file_name = Upath.get_attr "/FileRef/Path" "Value" xml |> Filename.basename |> Filename.remove_extension in
      let name = match preset_type with
        | UserPreset -> preset_file_name
        | DefaultPreset ->
          let device_name = Upath.get_attr "/DeviceId" "Name" xml in
          if device_name <> "" then device_name else preset_file_name
      in

      let pack_name = Upath.get_attr "/LivePackName" "Value" file_ref_xml in
      let pack_id = Upath.get_int_attr_opt "/LivePackId" "Value" file_ref_xml |> Option.value ~default:0 in
      let file_size = Upath.get_int_attr "/OriginalFileSize" "Value" file_ref_xml in
      let crc = Upath.get_int_attr_opt "/OriginalCrc" "Value" file_ref_xml |> Option.value ~default:0 in

      { id; name; preset_type; relative_path; path; pack_name; pack_id; file_size; crc }
    | _ -> failwith "Invalid XML element for creating PresetRef"

  module Patch = struct
    type t = {
      (* TODO: add the [name] field *)
      relative_path : string simple_flat_change;
      path : string simple_flat_change;
      pack_name : string simple_flat_change;
      pack_id : int simple_flat_change;
      file_size : int simple_flat_change;
      crc : int simple_flat_change;
    }

    let is_empty patch =
      patch.relative_path = `Unchanged &&
      patch.path = `Unchanged &&
      patch.pack_name = `Unchanged &&
      patch.pack_id = `Unchanged &&
      patch.file_size = `Unchanged &&
      patch.crc = `Unchanged
  end

  let diff (old_preset : t) (new_preset : t) : Patch.t =
    if old_preset.id <> new_preset.id then
      failwith "cannot diff two PresetRefs with different Ids"
    else
      let relative_path_change = diff_value old_preset.relative_path new_preset.relative_path in
      let path_change = diff_value old_preset.path new_preset.path in
      let pack_name_change = diff_value old_preset.pack_name new_preset.pack_name in
      let pack_id_change = diff_value old_preset.pack_id new_preset.pack_id in
      let file_size_change = diff_value old_preset.file_size new_preset.file_size in
      let crc_change = diff_value old_preset.crc new_preset.crc in
      {
        relative_path = relative_path_change;
        path = path_change;
        pack_name = pack_name_change;
        pack_id = pack_id_change;
        file_size = file_size_change;
        crc = crc_change;
      }
end


(* ================== M4L PatchRef module ================== *)
module PatchRef = struct
  type t = {
    id : int;
    name : string;
    preset_type : PresetRef.preset_type;
    relative_path : string;
    path : string;
    pack_name : string;
    pack_id : int;
    file_size : int;
    crc : int;
    last_mod_date : int64;  (* LastModDate Value attribute - UNIX timestamp *)
  } [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name="MxPatchRef"; childs; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let preset_type = PresetRef.UserPreset in (* M4L patches are always user presets *)

      (* Extract FileRef element *)
      let file_ref_xml =
        try List.find (function
          | Xml.Element { name = "FileRef"; _ } -> true
          | _ -> false) childs
        with Not_found -> failwith "FileRef element not found in MxPatchRef"
      in

      (* Extract all the required fields from FileRef *)
      let relative_path = Upath.get_attr "/RelativePath" "Value" file_ref_xml in
      let path = Upath.get_attr "/Path" "Value" file_ref_xml in
      let preset_file_name = Upath.get_attr "/Path" "Value" file_ref_xml |> Filename.basename |> Filename.remove_extension in

      (* Extract LastModDate as int64 UNIX timestamp *)
      let last_mod_date =
        try Upath.get_attr "/LastModDate" "Value" xml |> Int64.of_string
        with _ -> 0L (* Default to 0L if not found *)
      in

      let name = preset_file_name in
      let pack_name = Upath.get_attr "/LivePackName" "Value" file_ref_xml in
      let pack_id = Upath.get_int_attr_opt "/LivePackId" "Value" file_ref_xml |> Option.value ~default:0 in
      let file_size = Upath.get_int_attr "/OriginalFileSize" "Value" file_ref_xml in
      let crc = Upath.get_int_attr_opt "/OriginalCrc" "Value" file_ref_xml |> Option.value ~default:0 in

      { id; name; preset_type; relative_path; path; pack_name; pack_id; file_size; crc; last_mod_date }
    | _ -> failwith "Invalid XML element for creating PatchRef (expected MxPatchRef)"

  module Patch = struct
    type t = {
      relative_path : string simple_flat_change;
      path : string simple_flat_change;
      pack_name : string simple_flat_change;
      pack_id : int simple_flat_change;
      file_size : int simple_flat_change;
      crc : int simple_flat_change;
      last_mod_date : int64 simple_flat_change;
    }

    let is_empty patch =
      patch.relative_path = `Unchanged &&
      patch.path = `Unchanged &&
      patch.pack_name = `Unchanged &&
      patch.pack_id = `Unchanged &&
      patch.file_size = `Unchanged &&
      patch.crc = `Unchanged &&
      patch.last_mod_date = `Unchanged
  end

  let diff (old_patch : t) (new_patch : t) : Patch.t =
    (* FIXME: temporary suppress for debugging *)
    (* if old_patch.id <> new_patch.id then *)
    (*   failwith "cannot diff two PatchRefs with different Ids" *)
    (* else *)
      let relative_path_change = diff_value old_patch.relative_path new_patch.relative_path in
      let path_change = diff_value old_patch.path new_patch.path in
      let pack_name_change = diff_value old_patch.pack_name new_patch.pack_name in
      let pack_id_change = diff_value old_patch.pack_id new_patch.pack_id in
      let file_size_change = diff_value old_patch.file_size new_patch.file_size in
      let crc_change = diff_value old_patch.crc new_patch.crc in
      let last_mod_date_change = diff_value old_patch.last_mod_date new_patch.last_mod_date in
      {
        relative_path = relative_path_change;
        path = path_change;
        pack_name = pack_name_change;
        pack_id = pack_id_change;
        file_size = file_size_change;
        crc = crc_change;
        last_mod_date = last_mod_date_change;
      }
end


(* ================== Mixer module ================== *)
module Send = struct
  type t = {
    id : int;
    device_param : DeviceParam.t;
  } [@@deriving eq]

  (** Create [Send.t] from XML element.
      @param xml XML element [<TrackHolder Id="N">...</TrackHolder>] *)
  let create (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let device_param = Upath.find "/Send" xml |> snd |> DeviceParam.create "Send" in
    { id; device_param }

  let has_same_id a b = a.id = b.id
  let id_hash t = Hashtbl.hash t

  module Patch = DeviceParam.Patch

  let diff old_send new_send =
    if old_send.id <> new_send.id then
      failwith (Printf.sprintf "You can't compare two Send with different IDs: old = %d, new = %d" old_send.id new_send.id)
    else
      DeviceParam.diff old_send.device_param new_send.device_param

end

module Mixer = struct
  type t = {
    volume : DeviceParam.t;
    pan : DeviceParam.t;
    mute : DeviceParam.t;
    solo : DeviceParam.t;
    sends : Send.t list;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let volume = Upath.find "/Volume" xml |> snd |> DeviceParam.create "Volume" in
    let pan = Upath.find "/Pan" xml |> snd |> DeviceParam.create "Pan" in
    let mute = Upath.find "/On" xml |> snd |> DeviceParam.create "On" in

    (* SoloSink has a different structure - it's just <SoloSink Value="..."/> without Manual element *)
    (* We need to wrap it to make it compatible with DeviceParam.create *)
    let solo =
      let solo_value = Upath.get_bool_attr "/SoloSink" "Value" xml in
      let wrapped_element = Xml.Element {
        name = "SoloSink";
        attrs = [];
        childs = [
          Xml.Element {
            name = "Manual";
            attrs = ["Value", string_of_bool solo_value];
            childs = []
          }
        ]
      } in
      DeviceParam.create "SoloSink" wrapped_element
    in

    let sends = xml
      |> Upath.find_all "/Sends/TrackSendHolder"
      |> List.map (fun (_, xml) -> Send.create xml)
    in
    { volume; pan; mute; solo; sends }

  (* Mixer doesn't have a natural ID, so use placeholder interface *)
  (* TODO: is this necessary? *)
  let has_same_id _ _ = true
  let id_hash _ = Hashtbl.hash 0

  module Patch = struct
    type t = {
      volume : DeviceParam.Patch.t;
      pan : DeviceParam.Patch.t;
      mute : DeviceParam.Patch.t;
      solo : DeviceParam.Patch.t;
      sends : (Send.t, Send.Patch.t) structured_change list;
    }

    let is_empty patch =
      DeviceParam.Patch.is_empty patch.volume &&
      DeviceParam.Patch.is_empty patch.pan &&
      DeviceParam.Patch.is_empty patch.mute &&
      DeviceParam.Patch.is_empty patch.solo &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.sends
  end

  let diff (old_mixer : t) (new_mixer : t) : Patch.t =
    let volume_change = DeviceParam.diff old_mixer.volume new_mixer.volume in
    let pan_change = DeviceParam.diff old_mixer.pan new_mixer.pan in
    let mute_change = DeviceParam.diff old_mixer.mute new_mixer.mute in
    let solo_change = DeviceParam.diff old_mixer.solo new_mixer.solo in

    let send_changes = diff_list_myers_id (module Send) old_mixer.sends new_mixer.sends
                     |> List.map @@ structured_change_of_flat (module Send)
    in
    { volume = volume_change;
      pan = pan_change;
      mute = mute_change;
      solo = solo_change;
      sends = send_changes;
    }
end


(* ================== Plugin related modules ================== *)
module PluginParam = struct
  type t = {
    id : int;                   (* ParameterId *)
    name : string;              (* ParameterName *)
    index : int;                (* VisualIndex *)
    value : param_value;        (* Manual *)
    automation : int;           (* ParameterValue/AutomationTarget *)
    modulation : int;           (* ParameterValue/ModulationTarget *)
    (* TODO: macro mapping *)
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
      | "PluginEnumParameter" ->
        (* failwith "PluginEnumParameter parsing not yet implemented - needs further analysis of Ableton Live XML format" *)
        Float (Upath.get_float_attr "/ParameterValue/Manual" "Value" xml)  (* FIXME: Temporary fallback *)
      | _ -> failwith ("Invalid parameter type " ^ parameter_type)
    in

    let automation = Upath.get_int_attr "/ParameterValue/AutomationTarget" "Id" xml in
    let modulation = Upath.get_int_attr_opt "/ParameterValue/ModulationTarget" "Id" xml |> Option.value ~default:0 in

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

  let parse_vst3_uid xml =
    let fields = Upath.find_all "/'Fields\\.[0-9]+$'" xml in
    let sorted_fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
    sorted_fields
    |> List.map (fun (_, field_xml) -> Xml.get_attr "Value" field_xml)
    |> String.concat "-"

  let parse_vst3_processor_state plugin_info_xml =
    match Upath.find_opt "/Preset/Vst3Preset/ProcessorState" plugin_info_xml with
    | Some (_, state_xml) ->
      (* Get all text content and trim it *)
      let content =
        state_xml
        |> Xml.get_childs
        |> List.filter_map (function Xml.Data value -> Some value | _ -> None)
        |> String.concat ""
        |> trim_blob_str
      in
      content
    | None -> ""

  let parse_vst3_info plugin_info_xml =
    (* Get plugin name - try different possible locations *)
    let name =
      match Upath.get_attr_opt "/Name" "Value" plugin_info_xml with
      | Some value -> value
      | None ->
        match Upath.get_attr_opt "/PlugName" "Value" plugin_info_xml with
        | Some plug_name -> plug_name
        | None -> "Unknown VST3 Plugin"
    in

    (* Get UID from VST3 specific structure *)
    let uid = Upath.find "/Uid" plugin_info_xml |> snd |> parse_vst3_uid in

    (* Get processor state from VST3 preset *)
    let state = parse_vst3_processor_state plugin_info_xml in

    (name, uid, state)

  let parse_vst2_processor_state plugin_info_xml =
    (* VST2 plugins typically don't have processor state in the same way as VST3 *)
    match Upath.find_opt "/Preset/VstPreset/State" plugin_info_xml with
    | Some (_, state_xml) ->
      (* Get all text content and trim it *)
      let content =
        state_xml
        |> Xml.get_childs
        |> List.filter_map (function Xml.Data value -> Some value | _ -> None)
        |> String.concat ""
        |> trim_blob_str
      in
      content
    | None -> ""

  let parse_vst2_info plugin_info_xml =
    (* Get plugin name - try different possible locations for VST2 *)
    let name =
      match Upath.get_attr_opt "/PlugName" "Value" plugin_info_xml with
      | Some plug_name -> plug_name
      | None ->
        match Upath.get_attr_opt "/Name" "Value" plugin_info_xml with
        | Some value -> value
        | None -> "Unknown VST2 Plugin"
    in

    (* Get UID from VST2 specific structure *)
    let uid =
      match Upath.get_attr_opt "/UniqueId" "Value" plugin_info_xml with
      | Some unique_id -> unique_id
      | None ->
        (* Try to extract from path as fallback *)
        match Upath.get_attr_opt "/Path" "Value" plugin_info_xml with
        | Some path ->
          (* Extract plugin name from path and create a simple hash *)
          let filename = Filename.basename path in
          string_of_int (Hashtbl.hash filename)
        | None -> "0"
    in

    (* Get processor state from VST2 preset *)
    let state = parse_vst2_processor_state plugin_info_xml in

    (name, uid, state)

  let parse_au_processor_state plugin_info_xml =
    (* AU plugins use Buffer element for state data *)
    match Upath.find_opt "/Preset/AuPreset/Buffer" plugin_info_xml with
    | Some (_, buffer_xml) ->
      (* Get all text content and trim it *)
      let content =
        buffer_xml
        |> Xml.get_childs
        |> List.filter_map (function Xml.Data value -> Some value | _ -> None)
        |> String.concat ""
        |> trim_blob_str
      in
      content
    | None -> ""

  let parse_au_info plugin_info_xml =
    (* Get plugin name for AU plugins *)
    let name =
      match Upath.get_attr_opt "/Name" "Value" plugin_info_xml with
      | Some value -> value
      | None -> "Unknown AU Plugin"
    in

    (* Create UID from AU component identifiers *)
    let uid =
      match Upath.get_attr_opt "/ComponentType" "Value" plugin_info_xml,
            Upath.get_attr_opt "/ComponentSubType" "Value" plugin_info_xml,
            Upath.get_attr_opt "/ComponentManufacturer" "Value" plugin_info_xml with
      | Some comp_type, Some comp_subtype, Some comp_manufacturer ->
        Printf.sprintf "%s-%s-%s" comp_type comp_subtype comp_manufacturer
      | _ ->
        (* Fallback: try to use manufacturer and name *)
        match Upath.get_attr_opt "/Manufacturer" "Value" plugin_info_xml with
        | Some manufacturer ->
          let combined = manufacturer ^ "-" ^ name in
          string_of_int (Hashtbl.hash combined)
        | None ->
          (* Last resort: hash the plugin name *)
          string_of_int (Hashtbl.hash name)
    in

    (* Get processor state from AU preset buffer *)
    let state = parse_au_processor_state plugin_info_xml in

    (name, uid, state)

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

    let (name, uid, state) =
      match plugin_type with
      | Vst3 -> parse_vst3_info plugin_info_xml
      | Vst2 -> parse_vst2_info plugin_info_xml
      | Auv2 -> parse_au_info plugin_info_xml
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


(* ================== Max4Live device related modules ================== *)
module Max4LiveParam = struct
  type t = {
    id : int;                   (* ParameterId *)
    name : string;              (* ParameterName *)
    index : int;                (* VisualIndex *)
    value : param_value;        (* Manual *)
    automation : int;           (* Timeable/AutomationTarget *)
    modulation : int;           (* Timeable/ModulationTarget *)
    (* TODO: macro mapping *)
  } [@@deriving eq]

  (** Extract enum description from Names/Name/Name structure - specific to M4L *)
  let extract_enum_desc (xml : Xml.t) : enum_desc =
    try
      (* Find all Name elements under Names *)
      let name_elements = Upath.find_all "/Names/Name/Name" xml in
      let enums =
        List.map (fun (_, name_xml) ->
          Xml.get_attr "Value" name_xml
        ) name_elements
        |> Array.of_list
      in
      if Array.length enums > 0 then
        let max_id = Array.length enums - 1 in
        { min = 0; max = max_id; enums }
      else
        (* Fallback to empty enum if no names found *)
        { min = 0; max = 0; enums = [|""|] }
    with
    | _ ->
      (* Fallback if parsing fails *)
      { min = 0; max = 0; enums = [|""|] }

  (** Create M4L parameter from XML element *)
  let create (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let name = Upath.get_attr "/Name" "Value" xml in
    let index = Upath.get_int_attr "/Index" "Value" xml in

    let value =
      let parameter_type = Xml.get_name xml in
      match parameter_type with
      | "MxDFloatParameter" ->
        Float (Upath.get_float_attr "/Timeable/Manual" "Value" xml)
      | "MxDIntParameter" ->
        Int (Upath.get_int_attr "/Timeable/Manual" "Value" xml)
      | "MxDBoolParameter" ->
        Bool (Upath.get_bool_attr "/Timeable/Manual" "Value" xml)
      | "MxDEnumParameter" ->
        let enum_value = Upath.get_int_attr "/Timeable/Manual" "Value" xml in
        let enum_desc = extract_enum_desc xml in
        Enum (enum_value, enum_desc)
      | _ -> failwith ("Invalid M4L parameter type: " ^ parameter_type)
    in

    (* AutomationTarget is optional, use -1 as fallback *)
    let automation =
      try Upath.get_int_attr "/Timeable/AutomationTarget" "Id" xml
      with _ -> -1
    in

    (* ModulationTarget is optional, use -1 as fallback *)
    let modulation =
      try Upath.get_int_attr "/Timeable/ModulationTarget" "Id" xml
      with _ -> -1
    in

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
      failwith "cannot diff two Max4LiveParams with different Ids"
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


(* ================== Group device related modules ================== *)
(* The rack chain's mixer is different to track's mixer *)
module MixerDevice = struct
  type t = {
    on : DeviceParam.t;
    speaker : DeviceParam.t;     (* mute or not *)
    volume : DeviceParam.t;      (* volume *)
    pan : DeviceParam.t;         (* panorama/panning *)
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MixerDevice"; _ } ->
      (* Extract On parameter *)
      let on = Upath.find "/On" xml |> DeviceParam.create_from_upath_find in
      let speaker = Upath.find "/Speaker" xml |> DeviceParam.create_from_upath_find in
      let volume = Upath.find "/Volume" xml |> DeviceParam.create_from_upath_find in
      let pan = Upath.find "/Panorama" xml |> DeviceParam.create_from_upath_find in
      { on; speaker; volume; pan }
    | _ -> failwith "Invalid XML element for creating MixerDevice"

  module Patch = struct
    type t = {
      on : DeviceParam.Patch.t simple_structured_change;
      speaker : DeviceParam.Patch.t simple_structured_change;
      volume : DeviceParam.Patch.t simple_structured_change;
      pan : DeviceParam.Patch.t simple_structured_change;
    }

    let is_empty patch =
      patch.on = `Unchanged &&
      patch.speaker = `Unchanged &&
      patch.volume = `Unchanged &&
      patch.pan = `Unchanged
  end

  let diff (old_mixer : t) (new_mixer : t) : Patch.t =
    let on_change = diff_structured_value (module DeviceParam) old_mixer.on new_mixer.on in
    let speaker_change = diff_structured_value (module DeviceParam) old_mixer.speaker new_mixer.speaker in
    let volume_change = diff_structured_value (module DeviceParam) old_mixer.volume new_mixer.volume in
    let pan_change = diff_structured_value (module DeviceParam) old_mixer.pan new_mixer.pan in
    {
      Patch.on = on_change;
      speaker = speaker_change;
      volume = volume_change;
      pan = pan_change;
    }
end


(** [extract_index_from_name element_name] extracts the numeric index from
    element names like "MacroValues.3", "MacroControls.15", etc.
    Raises [Failure] if the element name doesn't contain a valid index. *)
let extract_index_from_name (element_name : string) : int =
  let parts = String.split_on_char '.' element_name in
  match List.rev parts with
  | index :: _ -> int_of_string index
  | [] -> failwith ("Invalid element name: " ^ element_name)


module Macro = struct
  type t = {
    id : int;
    name : string;
    manual : float;             (* current value *)
    automation : int;
    modulation : int;           (* Live 12 added modulation *)
  } [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let create (name_xml : Xml.t) (control_xml : Xml.t) : t =
    (* Extract the macro name from MacroDisplayNames element *)
    let name_id = extract_index_from_name @@ Xml.get_name name_xml in
    let control_id = extract_index_from_name @@ Xml.get_name control_xml in
    let name = Xml.get_attr "Value" name_xml in
    let manual = Upath.get_float_attr "/Manual" "Value" control_xml in
    let automation = Upath.get_int_attr "/AutomationTarget" "Id" control_xml in
    (* Extract modulation target ID (Live 12 feature) *)
    let modulation =
      Upath.get_int_attr_opt "/ModulationTarget" "Id" control_xml
      |> Option.value ~default:0 in
    if name_id <> control_id then
      failwith ("Macro name ID " ^ string_of_int name_id ^ " does not match control ID " ^ string_of_int control_id ^ ". Macro names and controls must be paired correctly.")
    else
    { id=name_id; name; manual; automation; modulation }

  module Patch = struct
    type t = {
      name : string simple_flat_change;
      manual : float simple_flat_change;
      automation : int simple_flat_change;
      modulation : int simple_flat_change;
    }

    let is_empty patch =
      patch.name = `Unchanged &&
      patch.manual = `Unchanged &&
      patch.automation = `Unchanged &&
      patch.modulation = `Unchanged
  end

  let diff (old_macro : t) (new_macro : t) : Patch.t =
    if old_macro.id <> new_macro.id then
      failwith "cannot diff two Macros with different Ids"
    else
      let name_change = diff_value old_macro.name new_macro.name in
      let manual_change = diff_value old_macro.manual new_macro.manual in
      let automation_change = diff_value old_macro.automation new_macro.automation in
      let modulation_change = diff_value old_macro.modulation new_macro.modulation in
      {
        Patch.name = name_change;
        manual = manual_change;
        automation = automation_change;
        modulation = modulation_change;
      }
end


module Snapshot = struct
  type t = {
    id : int;
    name : string;
    values : float list;
  } [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MacroSnapshot"; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr "/SnapshotName" "Value" xml in

      (* Extract all MacroValues.N elements using regex *)
      let macro_values_xml = Upath.find_all "/'MacroValues\\.[0-9]+'" xml in

      (* Build ordered list of values from MacroValues *)
      let values =
        List.map (fun (_, xml_elem) ->
          let element_name = match xml_elem with
            | Xml.Element { name; _ } -> name
            | Xml.Data _ -> failwith "Expected Element, got Data"
          in
          let index = extract_index_from_name element_name in
          let value = Xml.get_float_attr "Value" xml_elem in
          (index, value)
        ) macro_values_xml
        |> List.sort (fun (i1, _) (i2, _) -> Stdlib.compare i1 i2)
        |> List.map snd
      in

      { id; name; values }
    | _ -> failwith "Invalid XML element for creating Snapshot"

  module Patch = struct
    type t = {
      name : string simple_flat_change;
      values : float flat_change list;
    }

    let is_empty patch =
      patch.name = `Unchanged &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.values
  end

  let diff (old_snapshot : t) (new_snapshot : t) : Patch.t =
    if old_snapshot.id <> new_snapshot.id then
      failwith "cannot diff two Snapshots with different Ids"
    else
      let name_change = diff_value old_snapshot.name new_snapshot.name in
      let values_changes = diff_list_ord (module FloatEq) old_snapshot.values new_snapshot.values in

      {
        name = name_change;
        values = values_changes;
      }
end


(* ================== Type definitions ================== *)
(* JEEZ, CIRCULR DEPENDENCIES *)
type device =
  | Regular of regular_device
  | Plugin of plugin_device
  | Max4Live of max4live_device
  | Group of group_device [@@deriving eq]

and regular_device = {
  id : int;
  device_name : string;
  display_name : string;        (* either UserName or PresetName *)
  pointee : int;
  enabled : DeviceParam.t;
  params : DeviceParam.t list;
  preset : PresetRef.t option;
} [@@deriving eq]

and plugin_device = {
  id : int;
  device_name : string;
  display_name : string;
  pointee : int;
  enabled : DeviceParam.t;
  desc : PluginDesc.t;
  params : PluginParam.t list;
  preset : PresetRef.t option;
  (* TODO: Support sidechain and MPE settigns *)
} [@@deriving eq]

and max4live_device = {
  id : int;
  device_name : string;
  display_name : string;
  pointee : int;
  enabled : DeviceParam.t;
  patch_ref : PatchRef.t;       (* the .amxd file *)
  params : Max4LiveParam.t list;
  preset : PresetRef.t option;
} [@@deriving eq]

and branch = {
  id : int;
  devices : device list;
  mixer : MixerDevice.t;
} [@@deriving eq]
and group_device = {
  id : int;
  device_name : string;
  display_name : string;
  pointee : int;
  enabled : DeviceParam.t;
  branches : branch list;
  macros : Macro.t list;
  snapshots : Snapshot.t list;
  preset : PresetRef.t option;
} [@@deriving eq]

type device_patch =
  | RegularPatch of regular_device_patch
  | PluginPatch of plugin_device_patch
  | Max4LivePatch of max4live_device_patch
  | GroupPatch of group_device_patch

and regular_device_patch = {
  display_name : string simple_flat_change;

  (* parameters can be added or removed due to Ableton updates on their built-in devices,
     so structured_change is more semantically correct than simple_structured_change *)
  params : (DeviceParam.t, DeviceParam.Patch.t) structured_change list;

  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}

and plugin_device_patch = {
  display_name : string simple_flat_change;
  enabled : DeviceParam.Patch.t simple_structured_change;
  desc : PluginDesc.Patch.t simple_structured_change;
  params : (PluginParam.t, PluginParam.Patch.t) structured_change list;
  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}

and max4live_device_patch = {
  display_name : string simple_flat_change;
  enabled : DeviceParam.Patch.t simple_structured_change;
  patch_ref : (PatchRef.t, PatchRef.Patch.t) structured_change;
  params : (Max4LiveParam.t, Max4LiveParam.Patch.t) structured_change list;
  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}

and branch_patch = {
  id : int simple_flat_change;
  devices : (device, device_patch) structured_change list;
  mixer : MixerDevice.Patch.t simple_structured_change;
}
and group_device_patch = {
  display_name : string simple_flat_change;
  enabled : DeviceParam.Patch.t simple_structured_change;

  (* devices always have preset, its either user-defined one or the defualt one,
     so only Unchanged/Patched cases *)
  branches : (branch, branch_patch) structured_change list;
  macros : (Macro.t, Macro.Patch.t) structured_change list;
  snapshots : (Snapshot.t, Snapshot.Patch.t) structured_change list;
  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}


(* regular_device diff functions *)
let rec regular_device_diff (old_device : regular_device) (new_device : regular_device) : regular_device_patch =
  if old_device.id <> new_device.id && old_device.device_name <> new_device.device_name  then
    failwith "cannot diff two RegularDevices with different Ids & Device names"
  else
    let display_name_change = diff_value old_device.display_name new_device.display_name in
    let preset_change = diff_optional_structured_value (module PresetRef) old_device.preset new_device.preset in
    let params_changes =
      diff_list_id (module DeviceParam) old_device.params new_device.params
      |> List.map @@ structured_change_of_flat (module DeviceParam)
    in
    {
      display_name = display_name_change;
      preset = preset_change;
      params = params_changes;
    }
(* plugin_device diff functions *)
and plugin_device_diff (old_device : plugin_device) (new_device : plugin_device) : plugin_device_patch =
  if old_device.id <> new_device.id then
    failwith "cannot diff two PluginDevices with different IDs"
  else
    let display_name_change =
      diff_value old_device.display_name new_device.display_name
    in
    let enabled_change =
      diff_structured_value (module DeviceParam) old_device.enabled new_device.enabled
    in

    let desc_change =
      diff_structured_value (module PluginDesc) old_device.desc new_device.desc
    in

    let params_change =
      diff_list_id (module PluginParam) old_device.params new_device.params
      |> List.map @@ structured_change_of_flat (module PluginParam)
    in

    let preset_change =
      diff_optional_structured_value (module PresetRef) old_device.preset new_device.preset
    in

    {
      display_name = display_name_change;
      enabled = enabled_change;
      desc = desc_change;
      params = params_change;
      preset = preset_change
    }

(* max4live_device diff functions *)
and max4live_device_diff (old_device : max4live_device) (new_device : max4live_device) : max4live_device_patch =
  if old_device.id <> new_device.id then
    failwith "cannot diff two Max4LiveDevices with different IDs"
  else
    let display_name_change =
      diff_value old_device.display_name new_device.display_name
    in
    let enabled_change =
      diff_structured_value (module DeviceParam) old_device.enabled new_device.enabled
    in
    let patch_ref_change =
      let patch = PatchRef.diff old_device.patch_ref new_device.patch_ref in
      `Patched patch
    in
    let params_change =
      diff_list_id (module Max4LiveParam) old_device.params new_device.params
      |> List.map @@ structured_change_of_flat (module Max4LiveParam)
    in
    let preset_change =
      diff_optional_structured_value (module PresetRef) old_device.preset new_device.preset
    in
    {
      display_name = display_name_change;
      enabled = enabled_change;
      patch_ref = patch_ref_change;
      params = params_change;
      preset = preset_change
    }

(* group_device diff functions *)
and  branch_diff (old_branch : branch) (new_branch : branch) =
  if old_branch.id <> new_branch.id then
    failwith "cannot diff two Branches with different Ids"
  else
    let id_change = `Unchanged in (* IDs must be the same *)
    let module DeviceId = struct
      type t = device
      let equal = (=)
      let has_same_id old_device new_device =
        match old_device, new_device with
        | Regular old_reg, Regular new_reg -> old_reg.id = new_reg.id
        | Plugin old_plug, Plugin new_plug -> old_plug.id = new_plug.id
        | Group old_group, Group new_group -> old_group.id = new_group.id
        | Max4Live old_m4l, Max4Live new_m4l -> old_m4l.id = new_m4l.id
        | _ -> false
      let id_hash = function
        | Regular reg -> Hashtbl.hash reg.id
        | Plugin plug -> Hashtbl.hash plug.id
        | Group grp -> Hashtbl.hash grp.id
        | Max4Live m4l -> Hashtbl.hash m4l.id
    end in
    let devices_changes =
      diff_list_id (module DeviceId) old_branch.devices new_branch.devices
      |> List.map (function
          | `Unchanged -> `Unchanged
          | `Added device -> `Added device
          | `Removed device -> `Removed device
          | `Modified { old = old_device; new_ = new_device } ->
             (* TODO: figure out if device id is UNIQUE across different file versions *)
              match old_device, new_device with
              | Regular old_reg, Regular new_reg ->
                let patch = regular_device_diff old_reg new_reg in
                `Patched (RegularPatch patch)
              | Plugin old_plug, Plugin new_plug ->
                let patch = plugin_device_diff old_plug new_plug in
                `Patched (PluginPatch patch)
              | Group old_group, Group new_group ->
                let patch = group_device_diff old_group new_group in
                `Patched (GroupPatch patch)
              | Max4Live old_m4l, Max4Live new_m4l ->
                let patch = max4live_device_diff old_m4l new_m4l in
                `Patched (Max4LivePatch patch)
              | _ -> failwith "Cannot diff devices of different types")
    in
    let mixer_change =
      diff_structured_value_eq (module MixerDevice) old_branch.mixer new_branch.mixer
    in
    {
      id = id_change;
      devices = devices_changes;
      mixer = mixer_change;
    }
and is_branch_patch_empty (patch : branch_patch) =
  patch.id = `Unchanged &&
  patch.mixer = `Unchanged &&
  List.for_all (function `Unchanged -> true | _ -> false) patch.devices
and group_device_diff (old_group : group_device) (new_group : group_device) =
  if old_group.id <> new_group.id then
    failwith "cannot diff two GroupDevices with different Ids"
  else
    let display_name_change = diff_value old_group.display_name new_group.display_name in
    let enabled_change = diff_structured_value (module DeviceParam) old_group.enabled new_group.enabled in
    let preset_change = diff_optional_structured_value (module PresetRef) old_group.preset new_group.preset in
    let branches_changes =
      let module BranchId = struct
        type t = branch
        let equal = (=)
        let has_same_id (old_branch : t) (new_branch : t) = old_branch.id = new_branch.id
        let id_hash (branch : t) = Hashtbl.hash branch.id
        module Patch = struct
          type t = branch_patch
          let is_empty = is_branch_patch_empty
        end
        let diff = branch_diff
      end in
      diff_list_id (module BranchId) old_group.branches new_group.branches
      |> List.map @@ structured_change_of_flat (module BranchId)
    in
    let macros_changes =
      diff_list_id (module Macro) old_group.macros new_group.macros
      |> List.map @@ structured_change_of_flat (module Macro)
    in
    let snapshots_changes =
      diff_list_id (module Snapshot) old_group.snapshots new_group.snapshots
      |> List.map @@ structured_change_of_flat (module Snapshot)
    in
    {
      display_name = display_name_change;
      enabled = enabled_change;
      preset = preset_change;
      branches = branches_changes;
      macros = macros_changes;
      snapshots = snapshots_changes;
    }


(** Get display name based on [ShouldShowPresetName].
    if [ShouldShowPresetName] set to be [true], return the preset name.
    else return [UserName] if exists, if [UserName] is empty,
    return device type name.
    For a device with default preset, return the [DeviceId] as display name
    instead device type name.

    @param preset The preset reference of the device
    @param xml The XML element of the device
*)
let get_display_name (preset_opt : PresetRef.t option) (xml : Xml.t) =
  if Upath.get_bool_attr "ShouldShowPresetName" "Value" xml &&
     Option.is_some preset_opt then
    Option.map (fun p -> p.PresetRef.name) preset_opt |> Option.get
  else
    let user_name = Upath.get_attr "UserName" "Value" xml in
    if user_name <> "" then
      user_name
    else
      Xml.get_name xml


(* ================== Device modules ================== *)
module RegularDevice = struct
  (** All the built-in devices *)
  type t = regular_device [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; _ } ->
      let id = Alsdiff_base.Xml.get_int_attr "Id" xml in
      let pointee = Alsdiff_base.Upath.get_int_attr "/Pointee" "Id" xml in
      let preset = Upath.find_opt "/LastPresetRef/Value/*" xml |> Option.map snd |> Option.map PresetRef.create in
      let display_name = get_display_name preset xml in
      let enabled = Upath.find "/On" xml |> DeviceParam.create_from_upath_find in

      (* Find all elements that has both LomId and Manual child elements *)
      let params = Alsdiff_base.Upath.find_all "/**/LomId/../Manual/.." xml
        |> List.map DeviceParam.create_from_upath_find
      in
      { id; device_name=name; display_name; pointee; enabled; params; preset }
    | _ -> failwith "Invalid XML element for creating Device"

  module Patch = struct
    type t = regular_device_patch

    let is_empty (patch : t) =
      patch.display_name = `Unchanged &&
      patch.preset = `Unchanged &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.params
  end

  let diff (old_device : t) (new_device : t) : Patch.t =
    regular_device_diff old_device new_device
end


module PluginDevice = struct
  type t = plugin_device [@@deriving eq]

  let create (xml : Xml.t) : t =
    (* Get device ID *)
    let id = Xml.get_int_attr "Id" xml in
    let device_name = Xml.get_name xml in

    let preset = Upath.find_opt "/LastPresetRef/Value/*" xml
               |> Option.map snd
               |> Option.map PresetRef.create in
    let display_name = get_display_name preset xml in

    (* Get pointee ID *)
    let pointee = Upath.get_int_attr "/Pointee" "Id" xml in
    let enabled = Upath.find "/On" xml |> DeviceParam.create_from_upath_find in

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

    { id; device_name; display_name; pointee; enabled; desc; params; preset }


  module Patch = struct
    type t = plugin_device_patch

    let is_empty (patch : t) =
      patch.enabled = `Unchanged &&
      patch.desc = `Unchanged &&
      List.for_all (function
          | `Unchanged -> true
          | _ -> false
        ) patch.params
  end

  let has_same_id (a : t) (b : t) = a.id = b.id

  let diff = plugin_device_diff
end


module Branch = struct
  type t = branch [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let create (device_creator : Xml.t -> device) (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let mixer = Upath.find "MixerDevice" xml |> snd |> MixerDevice.create in
    let devices = Upath.find "/DeviceChain/*/Devices" xml
      |> snd
      |> Xml.get_childs
      |> List.map device_creator
    in
    { id; devices; mixer }

  module Patch = struct
    type t = branch_patch
    let is_empty = is_branch_patch_empty
  end

  let diff = branch_diff
end


module GroupDevice = struct
  type t = group_device [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let create (device_creator : Xml.t -> device) (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let pointee = Upath.get_int_attr "/Pointee" "Id" xml in
      let preset = Upath.find_opt "/LastPresetRef/Value/*" xml
        |> Option.map snd
        |> Option.map PresetRef.create in
      let display_name = get_display_name preset xml in
      let enabled = Upath.find "/On" xml |> DeviceParam.create_from_upath_find in

      let branches = Upath.find "/Branches" xml
        |> snd
        |> Xml.get_childs
        |> List.map (Branch.create device_creator)
      in
      let macro_names_xml = Upath.find_all "/'MacroDisplayNames\\.[0-9]+$'" xml in
      let macro_controls_xml = Upath.find_all "/'MacroControls\\.[0-9]+$'" xml in

      (* Create ordered list of macros by index *)
      let macros =
        List.combine macro_names_xml macro_controls_xml
        |> List.map (fun (n,c) ->
            let element_name = match (snd n) with
              | Xml.Element { name; _ } -> name
              | Xml.Data _ -> failwith "Expected Element, got Data"
            in
            let index = extract_index_from_name element_name in
            let macro = Macro.create (snd n) (snd c) in
            (index, macro)
          )
        |> List.sort (fun (i1, _) (i2, _) -> Stdlib.compare i1 i2)
        |> List.map snd
      in
      let snapshots =
        Upath.find_all "/MacroVariations/MacroSnapshots/MacroSnapshot" xml
        |> List.map snd
        |> List.map Snapshot.create
      in

      { id; device_name=name; display_name; pointee;  enabled; branches; macros; snapshots; preset }
    | _ -> invalid_arg "Cannot create a GroupDevice on Data"

  module Patch = struct
    type t = group_device_patch

    let is_empty (patch : t) =
      patch.display_name = `Unchanged &&
      patch.enabled = `Unchanged &&
      patch.preset = `Unchanged &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.branches &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.macros &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.snapshots
  end

  let diff = group_device_diff
end


module Max4LiveDevice = struct
  type t = max4live_device [@@deriving eq]

  let create (xml : Xml.t) : t =
    (* Get device ID *)
    let id = Xml.get_int_attr "Id" xml in
    let device_name = Xml.get_name xml in

    let preset = Upath.find_opt "/LastPresetRef/Value/*" xml
               |> Option.map snd
               |> Option.map PresetRef.create in
    let display_name = get_display_name preset xml in

    (* Get pointee ID *)
    let pointee = Upath.get_int_attr "/Pointee" "Id" xml in
    let enabled = Upath.find "/On" xml |> DeviceParam.create_from_upath_find in

    (* Parse PatchSlot/MxPatchRef for patch_ref *)
    let patch_ref =
      try
        let patch_slot_xml = Upath.find "/PatchSlot/Value/MxPatchRef" xml |> snd in
        PatchRef.create patch_slot_xml
      with _ ->
        (* Fallback if PatchSlot/MxPatchRef not found *)
        {
          id = 0;
          name = "";
          preset_type = PresetRef.UserPreset;
          relative_path = "";
          path = "";
          pack_name = "";
          pack_id = 0;
          file_size = 0;
          crc = 0;
          last_mod_date = 0L;
        }
    in

    (* Extract all M4L parameters *)
    let float_params = Alsdiff_base.Upath.find_all "**/MxDFloatParameter" xml in
    let int_params = Alsdiff_base.Upath.find_all "**/MxDIntParameter" xml in
    let bool_params = Alsdiff_base.Upath.find_all "**/MxDBoolParameter" xml in
    let enum_params = Alsdiff_base.Upath.find_all "**/MxDEnumParameter" xml in
    let all_params = float_params @ int_params @ bool_params @ enum_params in
    let params = List.map (fun (_, param_xml) -> Max4LiveParam.create param_xml) all_params in

    { id; device_name; display_name; pointee; enabled; patch_ref; params; preset }

  module Patch = struct
    type t = max4live_device_patch

    let is_empty (patch : t) =
      patch.enabled = `Unchanged &&
      patch.patch_ref = `Unchanged &&
      List.for_all (function
          | `Unchanged -> true
          | _ -> false
        ) patch.params
  end

  let has_same_id (a : t) (b : t) = a.id = b.id

  let diff = max4live_device_diff
end


type t = device [@@deriving eq]

let rec create (xml : Xml.t) : t =
  match xml with
  | Xml.Element { name; _ } ->
    (match name with
     | "InstrumentGroupDevice" | "DrumGroupDevice" | "MidiEffectGroupDevice" | "AudioEffectGroupDevice" ->
       Group (GroupDevice.create create xml)
     | "PluginDevice" | "AuPluginDevice" ->
       Plugin (PluginDevice.create xml)
     | "MxDeviceInstrument" | "MxDeviceAudioEffect" | "MxDeviceMidiEffect" ->
       Max4Live (Max4LiveDevice.create xml)
     | _ -> Regular (RegularDevice.create xml))

  | _ -> invalid_arg "Cannot create a Device on Data"

let has_same_id old_device new_device =
  match old_device, new_device with
  | Regular old_reg, Regular new_reg -> old_reg.id = new_reg.id
  | Plugin old_plug, Plugin new_plug -> old_plug.id = new_plug.id
  | Group old_group, Group new_group -> old_group.id = new_group.id
  | Max4Live old_m4l, Max4Live new_m4l -> old_m4l.id = new_m4l.id
  | _ -> false

let id_hash device =
  match device with
  | Regular reg -> Hashtbl.hash reg.id
  | Plugin plug -> Hashtbl.hash plug.id
  | Max4Live m4l -> Hashtbl.hash m4l.id
  | Group group -> Hashtbl.hash group.id

module Patch = struct
  type t =
    | RegularPatch of RegularDevice.Patch.t
    | PluginPatch of PluginDevice.Patch.t
    | Max4LivePatch of Max4LiveDevice.Patch.t
    | GroupPatch of GroupDevice.Patch.t
  let is_empty = function
    | RegularPatch patch -> RegularDevice.Patch.is_empty patch
    | PluginPatch patch -> PluginDevice.Patch.is_empty patch
    | Max4LivePatch patch -> Max4LiveDevice.Patch.is_empty patch
    | GroupPatch patch -> GroupDevice.Patch.is_empty patch
end

let diff (old_device : t) (new_device : t) : Patch.t =
  match (old_device, new_device) with
  | (Regular old_reg, Regular new_reg) ->
    let patch = RegularDevice.diff old_reg new_reg in
    Patch.RegularPatch patch
  | (Group old_group, Group new_group) ->
    let patch = GroupDevice.diff old_group new_group in
    Patch.GroupPatch patch
  | (Plugin old_plug, Plugin new_plug) ->
    let patch = PluginDevice.diff old_plug new_plug in
    Patch.PluginPatch patch
  | (Max4Live old_m4l, Max4Live new_m4l) ->
    let patch = Max4LiveDevice.diff old_m4l new_m4l in
    Patch.Max4LivePatch patch
  | _ -> failwith "cannot diff devices of different types (Regular vs Group)"
