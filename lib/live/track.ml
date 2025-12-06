open Alsdiff_base
open Alsdiff_base.Diff


module Routing = struct
  type route_type =
    | MidiIn
    | MidiOut
    | AudioIn
    | AudioOut
  [@@deriving eq]

  module RouteTypeEq = Equality.MakeDefaultEq(struct type t = route_type end)

  type t = {
    route_type : route_type;
    target : string;
    upper_string : string;
    lower_string : string;
  } [@@deriving eq]

  let has_same_id a b = a.route_type = b.route_type
  let id_hash t = Hashtbl.hash
      (match t.route_type with
       | MidiIn -> 1
       | MidiOut -> 2
       | AudioIn -> 3
       | AudioOut -> 4)

  (** Parse route type from XML element name *)
  let parse_route_type xml =
    match Xml.get_name xml with
      | "MidiInputRouting" -> MidiIn
      | "MidiOutputRouting" -> MidiOut
      | "AudioInputRouting" -> AudioIn
      | "AudioOutputRouting" -> AudioOut
      | name -> failwith ("Invalid routing element: " ^ name)

  (** [create xml] creates a routing object from an XML element *)
  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element _ ->
      let route_type = parse_route_type xml in
      let target = Upath.get_attr "/Target" "Value" xml in
      let upper_string = Upath.get_attr "/UpperDisplayString" "Value" xml in
      let lower_string = Upath.get_attr "/LowerDisplayString" "Value" xml in

      { route_type; target; upper_string; lower_string }
    | Xml.Data _ ->
      failwith "Invalid XML element for creating Routing"

  module Patch = struct
    type t = {
      route_type : route_type atomic_update;
      target : string atomic_update;
      upper_string : string atomic_update;
      lower_string : string atomic_update;
    }

    let is_empty patch =
      patch.route_type = `Unchanged &&
      patch.target = `Unchanged &&
      patch.upper_string = `Unchanged &&
      patch.lower_string = `Unchanged
  end

  let diff (old_routing : t) (new_routing : t) : Patch.t =
    if old_routing.route_type <> new_routing.route_type then
      failwith "cannot diff two Routing with different route types"
    else
      let route_type_change = diff_atomic_value (module RouteTypeEq) old_routing.route_type new_routing.route_type in
      let target_change = diff_atomic_value (module Equality.StringEq) old_routing.target new_routing.target in
      let upper_string_change = diff_atomic_value (module Equality.StringEq) old_routing.upper_string new_routing.upper_string in
      let lower_string_change = diff_atomic_value (module Equality.StringEq) old_routing.lower_string new_routing.lower_string in

      {
        Patch.route_type = route_type_change;
        target = target_change;
        upper_string = upper_string_change;
        lower_string = lower_string_change;
      }

end


module RoutingSet = struct
  type t = {
    audio_in : Routing.t;
    audio_out : Routing.t;
    midi_in : Routing.t;
    midi_out : Routing.t;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let audio_in = Upath.find "AudioInputRouting" xml |> snd |> Routing.create in
    let audio_out = Upath.find "AudioOutputRouting" xml |> snd |> Routing.create in
    let midi_in = Upath.find "MidiInputRouting" xml |> snd |> Routing.create in
    let midi_out = Upath.find "MidiOutputRouting" xml |> snd |> Routing.create in
    { audio_in; audio_out; midi_in; midi_out }

  let has_same_id _ _ = true
  let id_hash _ = Hashtbl.hash 0

  module Patch = struct
    type t = {
      audio_in : Routing.Patch.t update;
      audio_out : Routing.Patch.t update;
      midi_in : Routing.Patch.t update;
      midi_out : Routing.Patch.t update;
    }

    let is_empty patch =
      patch.audio_in = `Unchanged &&
      patch.audio_out = `Unchanged &&
      patch.midi_in = `Unchanged &&
      patch.midi_out = `Unchanged
  end

  let diff (old_set : t) (new_set : t) : Patch.t =
    let audio_in = diff_complex_value_id (module Routing) old_set.audio_in new_set.audio_in in
    let audio_out = diff_complex_value_id (module Routing) old_set.audio_out new_set.audio_out in
    let midi_in = diff_complex_value_id (module Routing) old_set.midi_in new_set.midi_in in
    let midi_out = diff_complex_value_id (module Routing) old_set.midi_out new_set.midi_out in
    { audio_in; audio_out; midi_in; midi_out }
end


module MidiTrack = struct
  type t = {
    id : int;                     (* Id attribute *)
    name : string;                (* EffectiveName *)
    clips : Clip.MidiClip.t list;
    automations : Automation.t list;
    devices : Device.t list;
    mixer : Device.Mixer.t;
    routings : RoutingSet.t;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let name = Upath.get_attr "/Name/EffectiveName" "Value" xml in
    let automations =
      Upath.find_all_seq "/AutomationEnvelopes/*/AutomationEnvelope" xml
      |> Seq.map (fun x -> x |> snd |> Automation.create)
      |> List.of_seq in
    let clips = Upath.find_all_seq "/**/ClipTimeable/ArrangerAutomation/Events/MidiClip" xml
              |> Seq.map (fun x -> x |> snd |> Clip.MidiClip.create)
              |> List.of_seq in
    let devices = Upath.find_all_seq "/DeviceChain/*/Devices" xml
      |> Seq.map snd
      |> Seq.concat_map (fun devs ->
          Xml.get_childs devs |> List.to_seq |> Seq.map Device.create)
      |> List.of_seq in
    let mixer = Upath.find "/DeviceChain/Mixer" xml |> snd |> Device.Mixer.create in
    let routings = Upath.find "/DeviceChain" xml |> snd |> RoutingSet.create in

    { id; name; clips; automations; devices; mixer; routings }

  let has_same_id a b = a.id = b.id
  let id_hash t = Hashtbl.hash t.id

  module Patch = struct
    type t = {
      name : string atomic_update;
      clips : (Clip.MidiClip.t, Clip.MidiClip.Patch.t) change list;
      automations : (Automation.t, Automation.Patch.t) change list;
      devices : (Device.t, Device.Patch.t) change list;
      mixer : Device.Mixer.Patch.t update;
      routings : RoutingSet.Patch.t update;
    }

    let is_empty patch =
      patch.name = `Unchanged &&
      patch.mixer = `Unchanged &&
      patch.routings = `Unchanged &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.clips &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.automations &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.devices
  end

  let diff (old_track : t) (new_track : t) : Patch.t =
    if old_track.id <> new_track.id then
      failwith "cannot diff two MidiTracks with different Ids"
    else
      let name_change = diff_atomic_value (module Equality.StringEq) old_track.name new_track.name in
      let clips_changes =
        diff_list_id (module Clip.MidiClip) old_track.clips new_track.clips
      in
      let automations_changes =
        diff_list_id (module Automation) old_track.automations new_track.automations
      in
      let devices_changes =
        diff_list_id (module Device) old_track.devices new_track.devices
      in
      let mixer_change = diff_complex_value (module Device.Mixer) old_track.mixer new_track.mixer in
      let routings_change = diff_complex_value_id (module RoutingSet) old_track.routings new_track.routings in
      {
        Patch.name = name_change;
        clips = clips_changes;
        automations = automations_changes;
        devices = devices_changes;
        mixer = mixer_change;
        routings = routings_change;
      }
end


module AudioTrack = struct
  type t = {
    id : int;                     (* Id attribute *)
    name : string;                (* EffectiveName *)
    clips : Clip.AudioClip.t list;
    automations : Automation.t list;
    devices : Device.t list;
    mixer : Device.Mixer.t;
    routings : RoutingSet.t;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let name = Upath.get_attr "/Name/EffectiveName" "Value" xml in
    let automations =
      Upath.find_all_seq "/AutomationEnvelopes/*/AutomationEnvelope" xml
      |> Seq.map (fun x -> x |> snd |> Automation.create)
      |> List.of_seq in
    let clips = Upath.find_all_seq "/**/AudioClip" xml
              |> Seq.map (fun x -> x |> snd |> Clip.AudioClip.create)
              |> List.of_seq in
    let devices = Upath.find_all_seq "/DeviceChain/*/Devices" xml
      |> Seq.map snd
      |> Seq.concat_map (fun devs ->
          Xml.get_childs devs |> List.to_seq |> Seq.map Device.create)
      |> List.of_seq in
    let mixer = Upath.find "/DeviceChain/Mixer" xml |> snd |> Device.Mixer.create in
    let routings = Upath.find "/DeviceChain" xml |> snd |> RoutingSet.create in
    { id; name; clips; automations; devices; mixer; routings }

  let has_same_id a b = a.id = b.id
  let id_hash t = Hashtbl.hash t.id

  module Patch = struct
    type t = {
      name : string atomic_update;
      clips : (Clip.AudioClip.t, Clip.AudioClip.Patch.t) change list;
      automations : (Automation.t, Automation.Patch.t) change list;
      devices : (Device.t, Device.Patch.t) change list;
      mixer : Device.Mixer.Patch.t update;
      routings : RoutingSet.Patch.t update;
    }

    let is_empty patch =
      patch.name = `Unchanged &&
      patch.mixer = `Unchanged &&
      patch.routings = `Unchanged &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.clips &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.automations &&
      List.for_all (function `Unchanged -> true | _ -> false) patch.devices
  end

  let diff (old_track : t) (new_track : t) : Patch.t =
    if old_track.id <> new_track.id then
      failwith "cannot diff two AudioTracks with different Ids"
    else
      let name_change = diff_atomic_value (module Equality.StringEq) old_track.name new_track.name in
      let clips_changes =
        diff_list_id (module Clip.AudioClip) old_track.clips new_track.clips
      in
      let automations_changes =
        diff_list_id (module Automation) old_track.automations new_track.automations
      in
      let devices_changes =
        diff_list_id (module Device) old_track.devices new_track.devices
      in
      let mixer_change = diff_complex_value (module Device.Mixer) old_track.mixer new_track.mixer in
      let routings_change = diff_complex_value_id (module RoutingSet) old_track.routings new_track.routings in
      {
        Patch.name = name_change;
        clips = clips_changes;
        automations = automations_changes;
        devices = devices_changes;
        mixer = mixer_change;
        routings = routings_change;
      }
end


(* Sum type that represents either a MidiTrack or AudioTrack *)
type t =
  | Midi of MidiTrack.t
  | Audio of AudioTrack.t
  (* TODO: send track, group track *)
[@@deriving eq]

let has_same_id old_track new_track =
  match old_track, new_track with
  | Midi old_midi, Midi new_midi -> MidiTrack.has_same_id old_midi new_midi
  | Audio old_audio, Audio new_audio -> AudioTrack.has_same_id old_audio new_audio
  | _ -> false

let id_hash = function
  | Midi midi -> MidiTrack.id_hash midi
  | Audio audio -> AudioTrack.id_hash audio

let create (xml : Xml.t) : t =
  match xml with
  | Xml.Element { name = "MidiTrack"; _ } -> Midi (MidiTrack.create xml)
  | Xml.Element { name = "AudioTrack"; _ } -> Audio (AudioTrack.create xml)
  | _ -> failwith ("Unsupported track type: " ^
                 match xml with
                 | Xml.Element { name; _ } -> name
                 | _ -> "non-element")

module Patch = struct
  type t =
    | MidiPatch of MidiTrack.Patch.t
    | AudioPatch of AudioTrack.Patch.t

  let is_empty = function
    | MidiPatch patch -> MidiTrack.Patch.is_empty patch
    | AudioPatch patch -> AudioTrack.Patch.is_empty patch
end

let diff (old_track : t) (new_track : t) : Patch.t =
  match old_track, new_track with
  | Midi old_midi, Midi new_midi ->
    let midi_patch = MidiTrack.diff old_midi new_midi in
    Patch.MidiPatch midi_patch
  | Audio old_audio, Audio new_audio ->
    let audio_patch = AudioTrack.diff old_audio new_audio in
    Patch.AudioPatch audio_patch
  | Midi _, Audio _ | Audio _, Midi _ ->
    failwith "cannot diff tracks of different types (MidiTrack vs AudioTrack)"
