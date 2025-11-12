open Alsdiff_base
open Alsdiff_base.Diff


module Send = struct
  type t = {
    target : int;
    amount : float;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let target = Upath.get_int_attr ".." "Id" xml in
    let amount = Upath.get_float_attr "Manual" "Value" xml in
    (* As mentioned in TODO.org, the target track's ID is currently unknown *)
    { target; amount }


  module Patch = struct
    type t = {
      target : int flat_change;
      amount : float flat_change;
    }

    let is_empty = function
      | { target = `Unchanged; amount = `Unchanged } -> true
      | _ -> false
    [@@warning "-32"]
  end


  let diff (old_send : t) (new_send : t) : Patch.t =
    let target_change =
      if old_send.target = new_send.target then
        `Unchanged
      else
        `Modified { old = old_send.target; new_ = new_send.target }
    in
    let amount_change =
      if old_send.amount = new_send.amount then
        `Unchanged
      else
        `Modified { old = old_send.amount; new_ = new_send.amount }
    in
    { target = target_change; amount = amount_change }
end


type t = {
  volume : float;
  pan : float;
  mute : bool;
  solo : bool;
  sends : Send.t list;
} [@@deriving eq]


let create (xml : Xml.t) : t =
  let volume = Upath.get_float_attr "/Volume/Manual" "Value" xml in
  let pan = Upath.get_float_attr "/Pan/Manual" "Value" xml in
  let mute = Upath.get_bool_attr "/On/Manual" "Value" xml in
  let solo = Upath.get_bool_attr "/SoloSink" "Value" xml in
  let sends =
    xml
    |> Upath.find_all "/Sends/TrackSendHolder"
    |> List.map (fun (_, track_send_holder) ->
        let holder_id =
          match track_send_holder with
          | Xml.Element { attrs; _ } ->
              List.assoc_opt "Id" attrs
              |> Option.map int_of_string
              |> Option.value ~default:0
          | _ -> 0
        in
        let amount = Upath.get_float_attr "Send/Manual" "Value" track_send_holder in
        { Send.target = holder_id; Send.amount = amount }
      )
  in
  { volume; pan; mute; solo; sends }
[@@warning "-32"]

(* Mixer doesn't have a natural ID, so use placeholder interface *)
let has_same_id _ _ = true
let id_hash _ = Hashtbl.hash 0

module Patch = struct
  type send_changes = (Send.t, Send.Patch.t) structured_change list

  type t = {
    volume : float flat_change;
    pan : float flat_change;
    mute : bool flat_change;
    solo : bool flat_change;
    sends : send_changes;
  }

  let is_empty = function
    | { volume = `Unchanged; pan = `Unchanged; mute = `Unchanged; solo = `Unchanged; sends } ->
      (match sends with
      | [] -> true
      | _ -> List.fold_right (fun x y ->
          (match x with
           | `Unchanged -> true
           | _ -> false) && y) sends true)
    | _ -> false
  [@@warning "-32"]
end


let diff (old_mixer : t) (new_mixer : t) : Patch.t =
  let volume_change =
    if old_mixer.volume = new_mixer.volume then
      `Unchanged
    else
      `Modified { old = old_mixer.volume; new_ = new_mixer.volume }
  in
  let pan_change =
    if old_mixer.pan = new_mixer.pan then
      `Unchanged
    else
      `Modified { old = old_mixer.pan; new_ = new_mixer.pan }
  in
  let mute_change =
    if old_mixer.mute = new_mixer.mute then
      `Unchanged
    else
      `Modified { old = old_mixer.mute; new_ = new_mixer.mute }
  in
  let solo_change =
    if old_mixer.solo = new_mixer.solo then
      `Unchanged
    else
      `Modified { old = old_mixer.solo; new_ = new_mixer.solo }
  in

  (* To properly handle send changes, we need to match sends that represent the same logical send
     Since the target is always 0, we'll use a combination of target and amount to match sends
     For the purpose of this diff, sends with the same target and amount are considered the same *)
  let module SendKey = struct
    type t = int
    let compare = compare
  end in
  let module SendMap = Map.Make(SendKey) in

  (* Create maps of sends keyed by target and amount *)
  let to_map sends =
    List.fold_left (fun map send ->
        let key = send.Send.target in
        SendMap.add key send map
      ) SendMap.empty sends
  in

  let old_map = to_map old_mixer.sends in
  let new_map = to_map new_mixer.sends in

  (* Merge the maps to find changes *)
  let send_changes = SendMap.merge (fun _ old_opt new_opt ->
      match old_opt, new_opt with
      | Some old_send, None -> Some (`Removed old_send)
      | None, Some new_send -> Some (`Added new_send)
      | Some old_send, Some new_send ->
        if Send.equal old_send new_send then
          None  (* Unchanged, don't include in changes list *)
        else
          (* Send was modified, create a patch *)
          let patch = Send.diff old_send new_send in
          Some (`Patched patch)
      | None, None -> None
    ) old_map new_map |> SendMap.to_seq |> Seq.map snd |> List.of_seq in

  { volume = volume_change;
    pan = pan_change;
    mute = mute_change;
    solo = solo_change;
    sends = send_changes;
  }
[@@warning "-32"]
