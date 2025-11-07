open Alsdiff_base
open Alsdiff_base.Diff
open Alsdiff_base.Equality

exception Not_implemented of string

module DeviceParam = struct
  (** Represents the value of a device parameter, which can be one of several types. *)
  type value =
    | Bool of bool
    | Int of int                (* probably will never be used *)
    | Float of float
  [@@deriving eq]

  type macro_mapping = {
    id : int;                   (* macro id *)
    low : int;                  (* mapping range *)
    high : int;
  } [@@deriving eq]

  (** Represents a single device parameter with a name, a value of a mixed type,
      and an automation ID. *)
  type t = {
    name : string;
    value : value;
    automation : int;
    mapping : macro_mapping option;
  } [@@deriving eq]

  let has_same_id a b = a.name = b.name (* each parameter has their unique names in the device *)

  let id_hash t = Hashtbl.hash t.name

  (** [value_of_string_opt s] attempts to parse a string [s] into a [value] type.
      For device parameters, we always prefer float values for numeric parameters,
      as device parameters are typically continuous values even when they appear
      as whole numbers. This is a private helper function. *)
  let value_of_string_opt (s : string) : value option =
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
    ;;

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
  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; _ } ->
      let automation =
        Upath.get_int_attr_opt "/AutomationTarget" "Id" xml
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
      { name; value; automation; mapping }
    | _ -> failwith "Invalid XML element for creating DeviceParam"

  module Patch = struct
    type t = {
      value : value simple_flat_change;
      automation : int simple_flat_change;
    }

    let is_empty = function
      | { value = `Unchanged; automation = `Unchanged } -> true
      | _ -> false
  end

  let diff (old_param : t) (new_param : t) : Patch.t =
    if old_param.name <> new_param.name then
      failwith "cannot diff two DeviceParams with different names"
    else
      let value_change = diff_value old_param.value new_param.value in
      let automation_change = diff_value old_param.automation new_param.automation in
      { value = value_change; automation = automation_change }
end


(** All the built-in devices *)
module RegularDevice = struct
  type t = {
    id : int;
    device_name : string;
    preset_name : string;
    pointee : int;
    params : DeviceParam.t list;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; _ } ->
      let id = Alsdiff_base.Xml.get_int_attr "Id" xml in
      let preset_name = Alsdiff_base.Upath.get_attr "/UserName" "Value" xml in
      let pointee = Alsdiff_base.Upath.get_int_attr "/Pointee" "Id" xml in
      let params = Alsdiff_base.Upath.find_all "/**/LomId/../Manual/.." xml |> List.map snd |> List.map DeviceParam.create in
      { id; device_name=name; preset_name; pointee; params }
    | _ -> failwith "Invalid XML element for creating Device"

  module Patch = struct
    type t = {
      device_name : string simple_flat_change;
      preset_name : string simple_flat_change;
      pointee : int simple_flat_change;

      (* since whatever a parameter is being used or not, its always be saved in the XML,
         so there are no `Added or `Removed changes. *)
      params : DeviceParam.Patch.t simple_structured_change list;
    }

    let is_empty patch =
      patch.device_name = `Unchanged &&
      patch.preset_name = `Unchanged &&
      patch.pointee = `Unchanged &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.params
  end

  let diff (old_device : t) (new_device : t) : Patch.t =
    if old_device.id <> new_device.id then
      failwith "cannot diff two RegularDevices with different Ids"
    else
      let device_name_change = diff_value old_device.device_name new_device.device_name in
      let preset_name_change = diff_value old_device.preset_name new_device.preset_name in
      let pointee_change = diff_value old_device.pointee new_device.pointee in
      let params_changes =
        diff_list_id (module DeviceParam) old_device.params new_device.params
        |> List.map (function
            | `Modified { old = old_param; new_ = new_param } ->
              `Patched (DeviceParam.diff old_param new_param)
            | `Unchanged -> `Unchanged
            | _ -> failwith "Not possible")
      in
      {
        device_name = device_name_change;
        preset_name = preset_name_change;
        pointee = pointee_change;
        params = params_changes;
      }
end


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
      let on_xml = Upath.find "/On" xml |> snd in
      let on = DeviceParam.create on_xml in

      (* Extract Speaker parameter *)
      let speaker_xml = Upath.find "/Speaker" xml |> snd in
      let speaker = DeviceParam.create speaker_xml in

      (* Extract Volume parameter *)
      let volume_xml = Upath.find "/Volume" xml |> snd in
      let volume = DeviceParam.create volume_xml in

      (* Extract Panorama parameter *)
      let pan_xml = Upath.find "/Panorama" xml |> snd in
      let pan = DeviceParam.create pan_xml in

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
    | Xml.Element { name = "MacroSnapshot"; attrs = _; childs = _; parent = _ } ->
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


type device =
  | Group of group_device
  | Regular of RegularDevice.t [@@deriving eq]
and group_device = {
  id : int;
  name : string;
  enabled : DeviceParam.t;
  branches : (device list * MixerDevice.t) list;
  macros : Macro.t list;
  snapshots : Snapshot.t list;
} [@@deriving eq]


module GroupDevice = struct
  type t = group_device [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let create_branch (device_creator : Xml.t -> device) (xml : Xml.t) : device list * MixerDevice.t =
    let mixer_device = Upath.find "MixerDevice" xml |> snd |> MixerDevice.create in
    let devices = Upath.find "/**/Devices" xml
      |> snd
      |> Xml.get_childs
      |> List.map device_creator
    in
    (devices, mixer_device)

  let create (device_creator : Xml.t -> device) (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = _; attrs = _; childs = _; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr "/UserName" "Value" xml in
      let enabled_xml = Upath.find "/On/Manual" xml |> snd in
      let enabled = DeviceParam.create enabled_xml in

      let branches = Upath.find "/Branches" xml
        |> snd
        |> Xml.get_childs
        |> List.map (create_branch device_creator)
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

      { id; name; enabled; branches; macros; snapshots }
    | _ -> invalid_arg "Cannot create a GroupDevice on Data"

  module Patch = struct
    type t = {
      name : string simple_flat_change;
      enabled : DeviceParam.Patch.t simple_structured_change;
      branches : (device list * MixerDevice.t) flat_change list;
      macros : Macro.t flat_change list;
      snapshots : Snapshot.t flat_change list;
    }

    let is_empty patch =
      patch.name = `Unchanged &&
      patch.enabled = `Unchanged &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.branches &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.macros &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.snapshots
  end

  let diff (old_group : t) (new_group : t) : Patch.t =
    if old_group.id <> new_group.id then
      failwith "cannot diff two GroupDevices with different Ids"
    else
      let name_change = diff_value old_group.name new_group.name in
      let enabled_change = diff_structured_value (module DeviceParam) old_group.enabled new_group.enabled in
      (* Create equality module for branch type *)
      let module BranchEq = struct
        type t = device list * MixerDevice.t
        let equal = (=)
      end in
      let branches_changes = diff_list (module BranchEq) old_group.branches new_group.branches in
      let macros_changes = diff_list_id (module Macro) old_group.macros new_group.macros in
      let snapshots_changes = diff_list_id (module Snapshot) old_group.snapshots new_group.snapshots in

      {
        name = name_change;
        enabled = enabled_change;
        branches = branches_changes;
        macros = macros_changes;
        snapshots = snapshots_changes;
      }

end


type t = device [@@deriving eq]

let rec create (xml : Xml.t) : t =
  match xml with
  | Xml.Element { name; _ } ->
    (match name with
     | "InstrumentGroupDevice" | "DrumGroupDevice" | "MidiEffectGroupDevice" | "AudioEffectGroupDevice" ->
       Group (GroupDevice.create create xml)
     | _ -> Regular (RegularDevice.create xml))
  | _ -> invalid_arg "Cannot create a Device on Data"

module Patch = struct
  type t =
    | RegularPatch of RegularDevice.Patch.t
    | GroupPatch of GroupDevice.Patch.t

  let is_empty = function
    | RegularPatch patch -> RegularDevice.Patch.is_empty patch
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
  | (Regular _, Group _) | (Group _, Regular _) ->
      failwith "cannot diff devices of different types (Regular vs Group)"
