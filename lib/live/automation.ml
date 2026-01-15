open Alsdiff_base
open Alsdiff_base.Diff


type event_value =
  | FloatEvent of float
  | IntEvent of int
  | EnumEvent of int
[@@deriving eq]

module EnvelopeEvent = struct
  type t = {
    id : int;
    time : float;
    value : event_value;
  } [@@deriving eq]

  (* EnvelopeEvent is a structured type containing primitive fields.
     It represents an automation envelope point that can be added,
     removed, or modified as a unit within the events list. *)

  let create (xml : Xml.t) : t =
    let tag_name = Xml.get_name xml in
    let id = Xml.get_int_attr "Id" xml in
    let time = Xml.get_float_attr "Time" xml in
    let value = match tag_name with
      | "FloatEvent" -> FloatEvent (Xml.get_float_attr "Value" xml)
      | "IntEvent" -> IntEvent (Xml.get_int_attr "Value" xml)
      | "EnumEvent" -> EnumEvent (Xml.get_int_attr "Value" xml)
      | _ -> raise (Xml.Xml_error (xml, "Unknown event type: " ^ tag_name))
    in
    { id; time; value }

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  module Patch = struct
    type t = {
      time : float atomic_update;
      value : event_value atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.time &&
      is_unchanged_atomic_update p.value
  end

  let diff (old_event : t) (new_event : t) : Patch.t =
    if not (has_same_id old_event new_event) then
      failwith "cannot diff two EnvelopeEvents with different Ids"
    else
      let { time = old_time; value = old_value; _ } = old_event in
      let { time = new_time; value = new_value; _ } = new_event in
      let time_change = diff_atomic_value (module Float) old_time new_time in
      let module EventValueEq = struct
        type t = event_value
        let equal = (=)
      end in
      let value_change = diff_atomic_value (module EventValueEq) old_value new_value in
      { time = time_change; value = value_change }
end


type t = {
  id : int;
  target : int;
  events : EnvelopeEvent.t list;
} [@@deriving eq]

(* Automation contains a list of EnvelopeEvents and is therefore
   a structured type at a higher level of abstraction. *)

let create (xml : Alsdiff_base.Xml.t) : t =
  let id = Xml.get_int_attr "Id" xml in
  let target = Upath.get_int_attr "/EnvelopeTarget/PointeeId" "Value" xml in
  let events =
    xml |> Upath.find_all "/Automation/Events/'(Float|Int|Enum)Event'"
    |> List.map (fun (_, event) -> EnvelopeEvent.create event)
    |> List.sort (fun a b -> Float.compare a.EnvelopeEvent.time b.EnvelopeEvent.time)
  in
  { id; target; events }

let has_same_id a b = a.id = b.id && a.target = b.target

let id_hash t = Hashtbl.hash (t.id, t.target)

module Patch = struct
  type t = {
    id : int;
    target : int;
    events : (EnvelopeEvent.t, EnvelopeEvent.Patch.t) structured_change list;
  }

  let is_empty x =
    List.for_all (is_unchanged_change (module EnvelopeEvent.Patch)) x.events
end

let diff (old_envelope : t) (new_envelope : t) : Patch.t =
  if old_envelope.id <> new_envelope.id &&
     old_envelope.target <> new_envelope.target then
    failwith "cannot diff two AutomationEnvelopes with different Id or Target"
  else
    let event_changes =
      (diff_list_id (module EnvelopeEvent) old_envelope.events new_envelope.events
       : (EnvelopeEvent.t, EnvelopeEvent.Patch.t) structured_change list)
    in

    { id = new_envelope.id; target = new_envelope.target; events = event_changes }
