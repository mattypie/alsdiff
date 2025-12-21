open Alsdiff_base
open Alsdiff_base.Diff


module Locator = struct
  type t = {
    id : int;
    name : string;
    time : float;
  } [@@deriving eq]

  let create (xml : Xml.t) (_file_path : string) : t =
    match xml with
    | Xml.Element { name = "Locator"; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr "/Name" "Value" xml in
      let time = Upath.get_float_attr "/Time" "Value" xml in
      { id; name; time }
    | _ -> failwith "Invalid XML element for creating Locator"

  let has_same_id a b = a.id = b.id
  let id_hash t = Hashtbl.hash t.id

  module Patch = struct
    type t = {
      name : string atomic_update;
      time : float atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.name &&
      is_unchanged_atomic_update p.time
  end

  let diff (old_locator : t) (new_locator : t) : Patch.t =
    let { id = old_id; name = old_name; time = old_time } = old_locator in
    let { id = new_id; name = new_name; time = new_time } = new_locator in

    (* Only compare locators with the same id *)
    if old_id <> new_id then
      failwith "cannot diff two locators with different Id"
    else
      let name_change = diff_atomic_value (module Equality.StringEq) old_name new_name in
      let time_change = diff_atomic_value (module Equality.FloatEq) old_time new_time in
      { name = name_change; time = time_change }
end

module Version = struct
  type t = {
    major : string;
    minor : string;
    revision : string;
  }

  let equal v1 v2 =
    v1.major = v2.major && v1.minor = v2.minor && v1.revision = v2.revision

  module Patch = struct
    type t = {
      major : string atomic_update;
      minor : string atomic_update;
      revision : string atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.major &&
      is_unchanged_atomic_update p.minor &&
      is_unchanged_atomic_update p.revision
  end

  let diff (old_version : t) (new_version : t) : Patch.t =
    let { major = old_major; minor = old_minor; revision = old_rev } = old_version in
    let { major = new_major; minor = new_minor; revision = new_rev } = new_version in

    let major_change = diff_atomic_value (module Equality.StringEq) old_major new_major in
    let minor_change = diff_atomic_value (module Equality.StringEq) old_minor new_minor in
    let rev_change = diff_atomic_value (module Equality.StringEq) old_rev new_rev in
    {
      Patch.major = major_change;
      minor = minor_change;
      revision = rev_change;
    }
end


module IntHashtbl = Hashtbl.Make(Int)


type pointee =
  | DevicePointee of Device.t
  | DeviceParamPointee of Device.DeviceParam.t   (* TODO: make those param pointees a tuple like (Device.t, Device.DeviceParam.t) *)
  | Max4LiveParamPointee of Device.Max4LiveParam.t
  | PluginParamPointee of Device.PluginParam.t
  | MacroPointee of Device.Macro.t


type t = {
  name : string;
  version : Version.t;
  creator : string;
  tracks : Track.t list;        (* for Audio, MIDI and Group tracks *)
  returns : Track.t list;       (* only Return tracks *)
  locators : Locator.t list;
  pointees : pointee IntHashtbl.t;
}


(* Helper to extract devices from any track type *)
let get_track_devices = function
  | Track.Midi t -> t.devices
  | Track.Audio t | Track.Group t | Track.Return t -> t.devices
  | Track.Main t -> t.devices

(* Recursive helper to process a device and its children (for Groups) *)
let rec process_device_recursive (pointees : pointee IntHashtbl.t) (device : Device.t) : unit =
  (* 1. Add the device itself to the pointees table *)
  let pointee_id = match device with
  | Device.Regular reg -> reg.pointee
  | Device.Plugin plugin -> plugin.pointee
  | Device.Max4Live m4l -> m4l.pointee
  | Device.Group group -> group.pointee
  in
  IntHashtbl.add pointees pointee_id (DevicePointee device);

  (* 2. Process parameters based on device type *)
  match device with
  | Device.Regular reg ->
      List.iter (fun (param : Device.DeviceParam.t) ->
        IntHashtbl.add pointees param.base.automation (DeviceParamPointee param);
        IntHashtbl.add pointees param.base.modulation (DeviceParamPointee param)
      ) reg.params

  | Device.Plugin plugin ->
      List.iter (fun (param : Device.PluginParam.t) ->
        IntHashtbl.add pointees param.base.automation (PluginParamPointee param);
        IntHashtbl.add pointees param.base.modulation (PluginParamPointee param)
      ) plugin.params

  | Device.Max4Live m4l ->
      List.iter (fun (param : Device.Max4LiveParam.t) ->
        IntHashtbl.add pointees param.base.automation (Max4LiveParamPointee param);
        IntHashtbl.add pointees param.base.modulation (Max4LiveParamPointee param)
      ) m4l.params

  | Device.Group group ->
      (* Process Macros *)
      List.iter (fun (macro : Device.Macro.t) ->
        IntHashtbl.add pointees macro.base.automation (MacroPointee macro);
        IntHashtbl.add pointees macro.base.modulation (MacroPointee macro)
      ) group.macros;

      (* Process Branches Recursively *)
      process_group_branches pointees group.branches

(* Helper to traverse Device.Group branches *)
and process_group_branches (pointees : pointee IntHashtbl.t) (branches : Device.branch list) : unit =
  List.iter (fun (branch : Device.branch) ->
    (* A. Recursively process devices in this branch *)
    List.iter (process_device_recursive pointees) branch.devices;

    (* B. Process Branch Mixer Parameters *)
    (* Branch mixer has: volume, pan, speaker, on *)
    let mixer = branch.mixer in
    let mixer_params = [mixer.volume; mixer.pan; mixer.speaker; mixer.on] in
    List.iter (fun (param : Device.DeviceParam.t) ->
      IntHashtbl.add pointees param.base.automation (DeviceParamPointee param);
      IntHashtbl.add pointees param.base.modulation (DeviceParamPointee param)
    ) mixer_params
  ) branches

let build_pointees_table (liveset : t) : unit =
  (* Clear the existing hashtable *)
  IntHashtbl.clear liveset.pointees;

  (* Process each track *)
  List.iter (fun track ->
    let devices = get_track_devices track in
    (* Use the recursive function instead of the flat one *)
    List.iter (process_device_recursive liveset.pointees) devices
  ) liveset.tracks


let create (xml : Xml.t) (file_path : string) : t =
  (* 1. Validate XML structure *)
  (match xml with
  | Xml.Element { name = "Ableton"; _ } ->
      (* Check for LiveSet child *)
      (match Upath.find_opt "LiveSet" xml with
       | Some _ -> ()
       | None -> failwith "Invalid Ableton file: missing LiveSet element")
  | Xml.Element { name; _ } ->
      failwith ("Invalid Ableton file: expected root element 'Ableton', got '" ^ name ^ "'")
  | _ ->
      failwith "Invalid Ableton file: root is not an element"
  );

  (* 2. Extract version information *)
  let version =
    try
      let major = Xml.get_attr "MajorVersion" xml in
      let minor = Xml.get_attr "MinorVersion" xml in
      let revision = Xml.get_attr_opt "Revision" xml |> Option.value ~default:"" in
      { Version.major; minor; revision }
    with Not_found ->
      (* Handle edge case where version info might be structured differently *)
      { Version.major = ""; minor = ""; revision = "" }
  in

  (* 3. Extract creator information *)
  let creator = Xml.get_attr_opt "Creator" xml |> Option.value ~default:"Unknown" in

  (* 4. Extract name from file path *)
  let name =
    file_path
    |> Filename.basename
    |> Filename.remove_extension
  in

  (* 5. Find LiveSet element *)
  let liveset_xml = Upath.find "LiveSet" xml |> snd in

  (* 6. Parse tracks *)
  let tracks_xml = Upath.find "Tracks" liveset_xml |> snd in

  let tracks =
      Xml.get_childs tracks_xml
      |> List.filter_map (fun track_xml ->
          match Xml.get_name track_xml with
          | "MidiTrack" | "AudioTrack" | "GroupTrack" -> Some (Track.create track_xml)
          | _ -> None
        )
  in

  let returns =
      Xml.get_childs tracks_xml
      |> List.filter_map (fun track_xml ->
          match Xml.get_name track_xml with
          | "ReturnTrack" -> Some (Track.create track_xml)
          | _ -> None
        )
  in

  (* 7. Parse locators *)
  let locators =
    try
      let locators_xml = Upath.find "/Locators/Locators" liveset_xml |> snd in
      Xml.get_childs locators_xml
      |> List.filter_map (function
          | Xml.Element { name = "Locator"; _ } as locator_xml ->
            Some (Locator.create locator_xml file_path)
          | _ -> None
        )
    with Not_found -> []
  in

  (* 8. Create initial liveset record *)
  let liveset = {
    name;
    version;
    creator;
    tracks;
    returns;
    locators;
    pointees = IntHashtbl.create 512  (* Initial size estimate *)
  } in

  (* 9. Build pointees table *)
  build_pointees_table liveset;

  (* 10. Return the fully initialized liveset *)
  liveset

module Patch = struct
  type t = {
    name : string atomic_update;
    version : Version.Patch.t structured_update;
    creator : string atomic_update;
    tracks : (Track.t, Track.Patch.t) structured_change list;
    returns : (Track.t, Track.Patch.t) structured_change list;
    locators : (Locator.t, Locator.Patch.t) structured_change list;
    (* Note: pointees is derived from tracks, so we don't diff it directly *)
  }

  let is_empty p =
    is_unchanged_atomic_update p.name &&
    is_unchanged_update (module Version.Patch) p.version &&
    is_unchanged_atomic_update p.creator &&
    List.for_all (is_unchanged_change (module Track.Patch)) p.tracks &&
    List.for_all (is_unchanged_change (module Track.Patch)) p.returns &&
    List.for_all (is_unchanged_change (module Locator.Patch)) p.locators
end

let diff (old_liveset : t) (new_liveset : t) : Patch.t =
  let name_change = diff_atomic_value (module Equality.StringEq) old_liveset.name new_liveset.name in
  let version_change = diff_complex_value (module Version) old_liveset.version new_liveset.version in
  let creator_change = diff_atomic_value (module Equality.StringEq) old_liveset.creator new_liveset.creator in
  let tracks_changes =
    diff_list_id (module Track) old_liveset.tracks new_liveset.tracks
  in
  let returns_changes =
    diff_list_id (module Track) old_liveset.returns new_liveset.returns
  in
  let locators_changes =
    diff_list_id (module Locator) old_liveset.locators new_liveset.locators
  in

  {
    Patch.name = name_change;
    version = version_change;
    creator = creator_change;
    tracks = tracks_changes;
    returns = returns_changes;
    locators = locators_changes;
  }
