open Alsdiff_base

type audio_track = {
  id : int;                     (* Id attribute *)
  name : string;                (* EffectiveName *)
  clips : Clip.AudioClip.t list;
  automations : Automation.t list;
  devices : Device.t list;
  mixer : Mixer.t;
}


module MidiTrack = struct
  type t = {
    id : int;                     (* Id attribute *)
    name : string;                (* EffectiveName *)
    clips : Clip.MidiClip.t list;
    automations : Automation.t list;
    devices : Device.t list;
    mixer : Mixer.t;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let name = Upath.get_attr "/**/EffectiveName" "Value" xml in
    let automations =
      Upath.find_all_seq "/AutomationEnvelopes/*/AutomationEnvelope" xml
      |> Seq.map (fun x -> x |> snd |> Automation.create)
      |> List.of_seq in
    let clips = Upath.find_all_seq "/**/MidiClip" xml
              |> Seq.map (fun x -> x |> snd |> Clip.MidiClip.create)
              |> List.of_seq in
    let devices = Upath.find_all_seq "/**/Devices" xml
      |> Seq.map snd
      |> Seq.concat_map (fun devs ->
          Xml.get_childs devs |> List.to_seq |> Seq.map Device.create)
      |> List.of_seq in
    let mixer = Upath.find "/**/Mixer" xml |> snd |> Mixer.create in

    { id; name; clips; automations; devices; mixer }
end
