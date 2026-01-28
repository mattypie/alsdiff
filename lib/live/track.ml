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
    | name -> raise (Xml.Xml_error (xml, "Invalid routing element: " ^ name))

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
      raise (Xml.Xml_error (xml, "Invalid XML element for creating Routing"))

  module Patch = struct
    type t = {
      route_type : route_type atomic_update;
      target : string atomic_update;
      upper_string : string atomic_update;
      lower_string : string atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.route_type &&
      is_unchanged_atomic_update p.target &&
      is_unchanged_atomic_update p.upper_string &&
      is_unchanged_atomic_update p.lower_string
  end

  let diff (old_routing : t) (new_routing : t) : Patch.t =
    if old_routing.route_type <> new_routing.route_type then
      failwith "cannot diff two Routing with different route types"
    else
      let route_type_change = diff_atomic_value (module RouteTypeEq) old_routing.route_type new_routing.route_type in
      let target_change = diff_atomic_value (module String) old_routing.target new_routing.target in
      let upper_string_change = diff_atomic_value (module String) old_routing.upper_string new_routing.upper_string in
      let lower_string_change = diff_atomic_value (module String) old_routing.lower_string new_routing.lower_string in

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
      audio_in : Routing.Patch.t structured_update;
      audio_out : Routing.Patch.t structured_update;
      midi_in : Routing.Patch.t structured_update;
      midi_out : Routing.Patch.t structured_update;
    }

    let is_empty p =
      is_unchanged_update (module Routing.Patch) p.audio_in &&
      is_unchanged_update (module Routing.Patch) p.audio_out &&
      is_unchanged_update (module Routing.Patch) p.midi_in &&
      is_unchanged_update (module Routing.Patch) p.midi_out
  end

  let diff (old_set : t) (new_set : t) : Patch.t =
    let audio_in = diff_complex_value_id (module Routing) old_set.audio_in new_set.audio_in in
    let audio_out = diff_complex_value_id (module Routing) old_set.audio_out new_set.audio_out in
    let midi_in = diff_complex_value_id (module Routing) old_set.midi_in new_set.midi_in in
    let midi_out = diff_complex_value_id (module Routing) old_set.midi_out new_set.midi_out in
    { audio_in; audio_out; midi_in; midi_out }
end


(* ================== Mixer module ================== *)
module GenericParam = Device.GenericParam
module NameIdGenericParam = Device.NameIdGenericParam

module Send = struct
  type t = {
    id : int;                   (* Id is the target *)
    amount : GenericParam.t;
  } [@@deriving eq]

  (** Create [Send.t] from XML element.
      @param xml XML element [<TrackHolder Id="N">...</TrackHolder>] *)
  let create (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let amount = Upath.find "/Send" xml |> snd |> GenericParam.create_float_manual in
    { id; amount }

  let has_same_id a b = a.id = b.id
  let id_hash t = Hashtbl.hash t

  module Patch = struct
    type t = {
      amount : GenericParam.Patch.t structured_update;
    }

    let is_empty p = is_unchanged_update (module GenericParam.Patch) p.amount
  end

  let diff old_send new_send =
    if old_send.id <> new_send.id then
      failwith (Printf.sprintf "You can't compare two Send with different IDs: old = %d, new = %d" old_send.id new_send.id)
    else
      let amount = diff_complex_value (module GenericParam) old_send.amount new_send.amount in
      { Patch.amount }
end


module Mixer = struct
  type t = {
    volume : GenericParam.t;
    pan : GenericParam.t;
    mute : GenericParam.t;
    solo : GenericParam.t;
    sends : Send.t list;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let volume = Upath.find "/Volume" xml |> snd |> GenericParam.create_float_manual in
    let pan = Upath.find "/Pan" xml |> snd |> GenericParam.create_float_manual in
    let mute = Upath.find "/On" xml |> snd |> GenericParam.create_bool_manual in

    (* SoloSink has a different structure - it's just <SoloSink Value="..."/> without Manual element *)
    (* We need to wrap it to make it compatible with Device.GenericParam.create *)
    let solo_value = Upath.get_bool_attr "/SoloSink" "Value" xml in
    let mapping =
      Upath.find_opt "/HeadKeyMidi" xml
      |> Option.map snd
      |> Option.map Device.MIDIMapping.create_head_key_midi
    in
    let solo = {
      GenericParam.name = "SoloSink";
      value = Bool solo_value;
      automation = 0;
      modulation = 0;
      mapping;
    } in
    let sends = xml
      |> Upath.find_all "/Sends/TrackSendHolder"
      |> List.map (fun (_, xml) -> Send.create xml)
    in
    { volume; pan; mute; solo; sends }

  (* MainMixer is a singleton in every track - all instances have same ID *)
  let has_same_id _ _ = true
  let id_hash _ = Hashtbl.hash 0

  module Patch = struct
    type t = {
      volume : GenericParam.Patch.t structured_update;
      pan : GenericParam.Patch.t structured_update;
      mute : GenericParam.Patch.t structured_update;
      solo : GenericParam.Patch.t structured_update;
      sends : (Send.t, Send.Patch.t) structured_change list;
    }

    let is_empty p =
      is_unchanged_update (module GenericParam.Patch) p.volume &&
      is_unchanged_update (module GenericParam.Patch) p.pan &&
      is_unchanged_update (module GenericParam.Patch) p.mute &&
      is_unchanged_update (module GenericParam.Patch) p.solo &&
      List.for_all (is_unchanged_change (module Send.Patch)) p.sends
  end

  let diff (old_mixer : t) (new_mixer : t) : Patch.t =
    let volume_change = diff_complex_value_id (module NameIdGenericParam) old_mixer.volume new_mixer.volume in
    let pan_change = diff_complex_value_id (module NameIdGenericParam) old_mixer.pan new_mixer.pan in
    let mute_change = diff_complex_value_id (module NameIdGenericParam) old_mixer.mute new_mixer.mute in
    let solo_change = diff_complex_value_id (module NameIdGenericParam) old_mixer.solo new_mixer.solo in

    let send_changes =
      diff_list_id (module Send) old_mixer.sends new_mixer.sends
      |> filter_changes (module Send.Patch)
    in
    { volume = volume_change;
      pan = pan_change;
      mute = mute_change;
      solo = solo_change;
      sends = send_changes;
    }
end


module MidiTrack = struct
  type t = {
    id : int;                     (* Id attribute *)
    name : string;                (* EffectiveName *)
    clips : Clip.MidiClip.t list;
    automations : Automation.t list;
    devices : Device.t list;
    mixer : Mixer.t;
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
    let mixer = Upath.find "/DeviceChain/Mixer" xml |> snd |> Mixer.create in
    let routings = Upath.find "/DeviceChain" xml |> snd |> RoutingSet.create in

    { id; name; clips; automations; devices; mixer; routings }

  let has_same_id a b = a.id = b.id
  let id_hash t = Hashtbl.hash t.id

  module Patch = struct
    type t = {
      id : int;
      current_name : string;
      name : string atomic_update;
      clips : (Clip.MidiClip.t, Clip.MidiClip.Patch.t) structured_change list;
      automations : (Automation.t, Automation.Patch.t) structured_change list;
      devices : (Device.t, Device.Patch.t) structured_change list;
      mixer : Mixer.Patch.t structured_update;
      routings : RoutingSet.Patch.t structured_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.name &&
      is_unchanged_update (module Mixer.Patch) p.mixer &&
      is_unchanged_update (module RoutingSet.Patch) p.routings &&
      List.for_all (is_unchanged_change (module Clip.MidiClip.Patch)) p.clips &&
      List.for_all (is_unchanged_change (module Automation.Patch)) p.automations &&
      List.for_all (is_unchanged_change (module Device.Patch)) p.devices
  end

  let diff (old_track : t) (new_track : t) : Patch.t =
    if old_track.id <> new_track.id then
      failwith (Printf.sprintf "Cannot diff two MidiTracks with different Ids: %d vs %d (Names: '%s' vs '%s')"
                  old_track.id new_track.id old_track.name new_track.name)
    else
      let name_change = diff_atomic_value (module String) old_track.name new_track.name in
      let clips_changes =
        diff_list_id (module Clip.MidiClip) old_track.clips new_track.clips
        |> filter_changes (module Clip.MidiClip.Patch)
      in
      let automations_changes =
        diff_list_id (module Automation) old_track.automations new_track.automations
        |> filter_changes (module Automation.Patch)
      in
      let devices_changes =
        diff_list_id (module Device) old_track.devices new_track.devices
        |> filter_changes (module Device.Patch)
      in
      let mixer_change = diff_complex_value (module Mixer) old_track.mixer new_track.mixer in
      let routings_change = diff_complex_value_id (module RoutingSet) old_track.routings new_track.routings in
      {
        id = new_track.id;
        current_name = new_track.name;
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
    mixer : Mixer.t;
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
    let mixer = Upath.find "/DeviceChain/Mixer" xml |> snd |> Mixer.create in
    let routings = Upath.find "/DeviceChain" xml |> snd |> RoutingSet.create in
    { id; name; clips; automations; devices; mixer; routings }

  let has_same_id a b = a.id = b.id
  let id_hash t = Hashtbl.hash t.id

  module Patch = struct
    type t = {
      id : int;
      current_name : string;
      name : string atomic_update;
      clips : (Clip.AudioClip.t, Clip.AudioClip.Patch.t) structured_change list;
      automations : (Automation.t, Automation.Patch.t) structured_change list;
      devices : (Device.t, Device.Patch.t) structured_change list;
      mixer : Mixer.Patch.t structured_update;
      routings : RoutingSet.Patch.t structured_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.name &&
      is_unchanged_update (module Mixer.Patch) p.mixer &&
      is_unchanged_update (module RoutingSet.Patch) p.routings &&
      List.for_all (is_unchanged_change (module Clip.AudioClip.Patch)) p.clips &&
      List.for_all (is_unchanged_change (module Automation.Patch)) p.automations &&
      List.for_all (is_unchanged_change (module Device.Patch)) p.devices
  end

  let diff (old_track : t) (new_track : t) : Patch.t =
    if old_track.id <> new_track.id then
      failwith (Printf.sprintf "Cannot diff two AudioTracks with different Ids: %d vs %d (Names: '%s' vs '%s')"
                  old_track.id new_track.id old_track.name new_track.name)
    else
      let name_change = diff_atomic_value (module String) old_track.name new_track.name in
      let clips_changes =
        diff_list_id (module Clip.AudioClip) old_track.clips new_track.clips
        |> filter_changes (module Clip.AudioClip.Patch)
      in
      let automations_changes =
        diff_list_id (module Automation) old_track.automations new_track.automations
        |> filter_changes (module Automation.Patch)
      in
      let devices_changes =
        diff_list_id (module Device) old_track.devices new_track.devices
        |> filter_changes (module Device.Patch)
      in
      let mixer_change = diff_complex_value (module Mixer) old_track.mixer new_track.mixer in
      let routings_change = diff_complex_value_id (module RoutingSet) old_track.routings new_track.routings in
      {
        id = new_track.id;
        current_name = new_track.name;
        Patch.name = name_change;
        clips = clips_changes;
        automations = automations_changes;
        devices = devices_changes;
        mixer = mixer_change;
        routings = routings_change;
      }
end


module MainMixer = struct
  type t = {
    base : Mixer.t;
    tempo : GenericParam.t;
    time_signature : GenericParam.t; (* TODO: how to parse the time signature number? *)
    crossfade : GenericParam.t;
    global_groove : GenericParam.t;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let base = Mixer.create xml in
    let tempo = Upath.find "/Tempo" xml |> snd |> GenericParam.create_float_manual in
    let time_signature = Upath.find "/TimeSignature" xml |> snd |> GenericParam.create_int_manual in
    let crossfade = Upath.find "/CrossFade" xml |> snd |> GenericParam.create_float_manual in
    let global_groove = Upath.find "/GlobalGrooveAmount" xml |> snd |> GenericParam.create_float_manual in
    { base; tempo; time_signature; crossfade; global_groove; }

  (* MainMixer is a singleton - all instances have same ID *)
  let has_same_id _ _ = true
  let id_hash _ = Hashtbl.hash 0

  module Patch = struct
    type t = {
      base : Mixer.Patch.t structured_update;
      tempo : GenericParam.Patch.t structured_update;
      time_signature : GenericParam.Patch.t structured_update;
      crossfade : GenericParam.Patch.t structured_update;
      global_groove : GenericParam.Patch.t structured_update;
    }

    let is_empty p =
      is_unchanged_update (module Mixer.Patch) p.base &&
      is_unchanged_update (module GenericParam.Patch) p.tempo &&
      is_unchanged_update (module GenericParam.Patch) p.time_signature &&
      is_unchanged_update (module GenericParam.Patch) p.crossfade &&
      is_unchanged_update (module GenericParam.Patch) p.global_groove
  end

  let diff (old_mixer : t) (new_mixer : t) : Patch.t =
    let base_change = diff_complex_value (module Mixer) old_mixer.base new_mixer.base in
    let tempo_change = diff_complex_value_id (module NameIdGenericParam) old_mixer.tempo new_mixer.tempo in
    let time_signature_change = diff_complex_value_id (module NameIdGenericParam) old_mixer.time_signature new_mixer.time_signature in
    let crossfade_change = diff_complex_value_id (module NameIdGenericParam) old_mixer.crossfade new_mixer.crossfade in
    let global_groove_change = diff_complex_value_id (module NameIdGenericParam) old_mixer.global_groove new_mixer.global_groove in
    {
      Patch.base = base_change;
      tempo = tempo_change;
      time_signature = time_signature_change;
      crossfade = crossfade_change;
      global_groove = global_groove_change;
    }

end


module MainTrack = struct
  type t = {
    name : string;                (* EffectiveName *)
    automations : Automation.t list;
    devices : Device.t list;
    mixer : MainMixer.t;          (* Use MainMixer instead of Mixer *)
    routings : RoutingSet.t;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let name = Upath.get_attr "/Name/EffectiveName" "Value" xml in
    let automations =
      Upath.find_all_seq "/AutomationEnvelopes/*/AutomationEnvelope" xml
      |> Seq.map (fun x -> x |> snd |> Automation.create)
      |> List.of_seq in
    let devices = Upath.find_all_seq "/DeviceChain/*/Devices" xml
      |> Seq.map snd
      |> Seq.concat_map (fun devs ->
          Xml.get_childs devs |> List.to_seq |> Seq.map Device.create)
      |> List.of_seq in
    let mixer = Upath.find "/DeviceChain/Mixer" xml |> snd |> MainMixer.create in
    let routings = Upath.find "/DeviceChain" xml |> snd |> RoutingSet.create in
    { name; automations; devices; mixer; routings }

  (* MainTrack is also a singleton *)
  let has_same_id _ _ = true
  let id_hash _ = Hashtbl.hash 0

  module Patch = struct
    type t = {
      current_name : string;
      name : string atomic_update;
      automations : (Automation.t, Automation.Patch.t) structured_change list;
      devices : (Device.t, Device.Patch.t) structured_change list;
      mixer : MainMixer.Patch.t structured_update;
      routings : RoutingSet.Patch.t structured_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.name &&
      is_unchanged_update (module MainMixer.Patch) p.mixer &&
      is_unchanged_update (module RoutingSet.Patch) p.routings &&
      List.for_all (is_unchanged_change (module Automation.Patch)) p.automations &&
      List.for_all (is_unchanged_change (module Device.Patch)) p.devices
  end

  let diff (old_track : t) (new_track : t) : Patch.t =
    let name_change = diff_atomic_value (module String) old_track.name new_track.name in
    let automations_changes =
      diff_list_id (module Automation) old_track.automations new_track.automations
      |> filter_changes (module Automation.Patch)
    in
    let devices_changes =
      diff_list_id (module Device) old_track.devices new_track.devices
      |> filter_changes (module Device.Patch)
    in
    let mixer_change = diff_complex_value (module MainMixer) old_track.mixer new_track.mixer in
    let routings_change = diff_complex_value_id (module RoutingSet) old_track.routings new_track.routings in
    {
      current_name = new_track.name;
      Patch.name = name_change;
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
  | Group of AudioTrack.t
  | Return of AudioTrack.t
  | Main of MainTrack.t
[@@deriving eq]

let has_same_id old_track new_track =
  match old_track, new_track with
  | Midi old_midi, Midi new_midi -> MidiTrack.has_same_id old_midi new_midi
  | Audio old_audio, Audio new_audio
  | Group old_audio, Group new_audio
  | Return old_audio, Return new_audio -> AudioTrack.has_same_id old_audio new_audio
  | Main old_main, Main new_main -> MainTrack.has_same_id old_main new_main
  | _ -> false

let id_hash = function
  | Midi midi -> MidiTrack.id_hash midi
  | Group audio | Audio audio | Return audio -> AudioTrack.id_hash audio
  | Main main -> MainTrack.id_hash main

let create (xml : Xml.t) : t =
  match xml with
  | Xml.Element { name = "MidiTrack"; _ } -> Midi (MidiTrack.create xml)
  | Xml.Element { name = "AudioTrack"; _ } -> Audio (AudioTrack.create xml)
  | Xml.Element { name = "GroupTrack"; _ } -> Group (AudioTrack.create xml)
  | Xml.Element { name = "ReturnTrack"; _ } -> Return (AudioTrack.create xml)
  | Xml.Element { name = "MainTrack"; _ } -> Main (MainTrack.create xml)
  | _ ->
    let name = match xml with
      | Xml.Element { name; _ } -> name
      | _ -> "non-element"
    in
    raise (Xml.Xml_error (xml, "Unsupported track type: " ^ name))

module Patch = struct
  type t =
    | MidiPatch of MidiTrack.Patch.t
    | AudioPatch of AudioTrack.Patch.t
    | MainPatch of MainTrack.Patch.t

  let is_empty = function
    | MidiPatch patch -> MidiTrack.Patch.is_empty patch
    | AudioPatch patch -> AudioTrack.Patch.is_empty patch
    | MainPatch patch -> MainTrack.Patch.is_empty patch
end

let get_name = function
  | Midi a -> a.name
  | Audio a -> a.name
  | Group a -> a.name
  | Return a -> a.name
  | Main _ -> "Main"

let type_name = function
  | Midi _ -> "MidiTrack"
  | Audio _ -> "AudioTrack"
  | Group _ -> "GroupTrack"
  | Return _ -> "ReturnTrack"
  | Main _ -> "MainTrack"

let diff (old_track : t) (new_track : t) : Patch.t =
  match old_track, new_track with
  | Midi old_midi, Midi new_midi ->
    let midi_patch = MidiTrack.diff old_midi new_midi in
    Patch.MidiPatch midi_patch
  | Audio old_audio, Audio new_audio
  | Group old_audio, Group new_audio
  | Return old_audio, Return new_audio ->
    let audio_patch = AudioTrack.diff old_audio new_audio in
    Patch.AudioPatch audio_patch
  | Main old_main, Main new_main ->
    let main_patch = MainTrack.diff old_main new_main in
    Patch.MainPatch main_patch
  | _ ->
    failwith (Printf.sprintf "Cannot diff tracks of different types: %s (%s) vs %s (%s)"
                (type_name old_track) (get_name old_track)
                (type_name new_track) (get_name new_track))
