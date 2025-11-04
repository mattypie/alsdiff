open Alsdiff_base
open Alsdiff_base.Diff

module DeviceParam = struct
  (** Represents the value of a device parameter, which can be one of several types. *)
  type value =
    | Bool of bool
    | Int of int                (* probably will never be used *)
    | Float of float
  [@@deriving eq]

  (** Represents a single device parameter with a name, a value of a mixed type,
      and an automation ID. *)
  type t = {
    name : string;
    value : value;
    automation : int;
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
      { name; value; automation }
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


module Macro = struct
  type target = {
    target : int;
    low : float;
    high : float;
  } [@@deriving eq]

  type t = {
    name : string;
    manual : float;             (* current value *)
    pointee : int;
    targets : target list;
  } [@@deriving eq]
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


exception Not_implemented of string


type device =
  | Group of group_device
  | Regular of RegularDevice.t [@@deriving eq]
and group_device = {
    name : string;
    enabled : DeviceParam.t;
    branches : (device list * MixerDevice.t) list;
    macros : Macro.t list;
    snapshots : Macro.t list list;
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


  let create_macro (xml : Xml.t) : Macro.t =
    ignore (xml);
    raise (Not_implemented "TODO")


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
      let macros = [] in
      let snapshots = [] in

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
