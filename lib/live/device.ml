open Alsdiff_base
open Alsdiff_base.Diff

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
      let value_change =
        if old_param.value = new_param.value then
          `Unchanged
        else
          `Modified { old = old_param.value; new_ = new_param.value }
      in
      let automation_change =
        if old_param.automation = new_param.automation then
          `Unchanged
        else
          `Modified { old = old_param.automation; new_ = new_param.automation }
      in
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
end


module Macro = struct
  type t = {
    name : string;
    manual : float;             (* current value *)
    automation : int;
    modulation : int;           (* Live 12 added modulation *)
  } [@@deriving eq]

  let create (name_xml : Xml.t) (control_xml : Xml.t) : t =
    (* Extract the macro name from MacroDisplayNames element *)
    let name = Xml.get_attr "Value" name_xml in
    let manual = Upath.get_float_attr "/Manual" "Value" control_xml in
    let automation = Upath.get_int_attr "/AutomationTarget" "Id" control_xml in
    (* Extract modulation target ID (Live 12 feature) *)
    let modulation =
      Upath.get_int_attr_opt "/ModulationTarget" "Id" control_xml
      |> Option.value ~default:0 in
    { name; manual; automation; modulation }

end


(** Common helper functions for device processing *)

(** [extract_index_from_name element_name] extracts the numeric index from
    element names like "MacroValues.3", "MacroControls.15", etc.
    Raises [Failure] if the element name doesn't contain a valid index. *)
let extract_index_from_name (element_name : string) : int =
  let parts = String.split_on_char '.' element_name in
  match List.rev parts with
  | index :: _ -> int_of_string index
  | [] -> failwith ("Invalid element name: " ^ element_name)


module Snapshot = struct
  type t = {
    id : int;
    name : string;
    values : float list;
  } [@@deriving eq]

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
end


type device =
  | Group of group_device
  | Regular of RegularDevice.t [@@deriving eq]
and group_device = {
    name : string;
    enabled : DeviceParam.t;
    branches : (device list * MixerDevice.t) list;
    macros : Macro.t list;
    snapshots : Snapshot.t list;
  } [@@deriving eq]


module GroupDevice = struct
  type t = group_device [@@deriving eq]

  let create_branch (device_creator : Xml.t -> device) (xml : Xml.t) : device list * MixerDevice.t =
    let mixer_device = Upath.find "MixerDevice" xml |> snd |> MixerDevice.create in
    let devices = Upath.find "/**/Devices" xml
      |> snd
      |> Xml.get_childs
      |> List.map device_creator
    in
    (devices, mixer_device)


  let create_macro (control_xml : Xml.t) (name_xml : Xml.t option) : Macro.t =
    (* Create a macro from control XML and optional name XML *)
    let default_name_xml = Xml.Element { name = "DefaultMacro"; attrs = []; childs = []; parent = None } in
    Macro.create
      (Option.value ~default:default_name_xml name_xml)
      control_xml

  let create (device_creator : Xml.t -> device) (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = _; attrs = _; childs = _; parent = _ } ->
      (* TODO: to be implemented *)
      let name = Upath.get_attr "/UserName" "Value" xml in
      let enabled_xml = Upath.find "/On/Manual" xml |> snd in
      let enabled = DeviceParam.create enabled_xml in

      (* Empty branches, macros, and snapshots for now *)
      let branches = Upath.find "/Branches" xml
        |> snd
        |> Xml.get_childs
        |> List.map (create_branch device_creator)
      in
      let macro_names_xml = Upath.find_all "/'MacroDisplayNames\\.[0-9]+$'" xml in
      let macro_controls_xml = Upath.find_all "/'MacroControls\\.[0-9]+$'" xml in

      (* Create ordered list of macros by index *)
      let macros =
        assert (List.length macro_names_xml = List.length macro_controls_xml);
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

      { name; enabled; branches; macros; snapshots }
    | _ -> invalid_arg "Cannot create a GroupDevice on Data"
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
