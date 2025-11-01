open Alsdiff_base
open Diff

module EnvelopeEvent = struct
  type t = {
    id : int;
    time : float;
    value : float;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    {
      id = Xml.get_int_attr "Id" xml;
      time = Xml.get_float_attr "Time" xml;
      value = Xml.get_float_attr "Value" xml;
    }

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  module Patch = struct
    type t = {
      time : float simple_flat_change;
      value : float simple_flat_change;
    }

    let is_empty = function
      | { time = `Unchanged; value = `Unchanged } -> true
      | _ -> false
  end

  let diff (old_event : t) (new_event : t) : Patch.t =
    if not (has_same_id old_event new_event) then
      failwith "cannot diff two EnvelopeEvents with different Ids"
    else
      let { time = old_time; value = old_value; _ } = old_event in
      let { time = new_time; value = new_value; _ } = new_event in
      let time_change = if old_time <> new_time then `Modified { old = old_time; new_ = new_time } else `Unchanged in
      let value_change = if old_value <> new_value then `Modified { old = old_value; new_ = new_value } else `Unchanged in
      { time = time_change; value = value_change }
end


type t = {
  id : int;
  target : int;
  events : EnvelopeEvent.t list;
} [@@deriving eq]

let create (xml : Alsdiff_base.Xml.t) : t =
  let id = Xml.get_int_attr "Id" xml in
  let target = Upath.get_int_attr "/EnvelopeTarget/PointeeId" "Value" xml in
  let events =
    xml
    |> Upath.find_all "/Automation/Events/FloatEvent"
    |> List.map (fun (_, event) -> EnvelopeEvent.create event)
  in
  { id; target; events }

let has_same_id a b = a.id = b.id && a.target = b.target

module Patch = struct
  type t = {
    id : int;
    target : int;
    events : EnvelopeEvent.t flat_change list; (* TODO: should be structured_change *)
  }

  let is_empty x =
    List.for_all (function
        | `Unchanged -> true
        | _ -> false) x.events
end

let diff (old_envelope : t) (new_envelope : t) : Patch.t =
  if old_envelope.id <> new_envelope.id &&
     old_envelope.target <> new_envelope.target then
    failwith "cannot diff two AutomationEnvelopes with different Id or Target"
  else
    let event_changes = diff_list_id (module EnvelopeEvent) old_envelope.events new_envelope.events in

    match event_changes with
    | [] -> { id = new_envelope.id; target = new_envelope.target; events = [] } (* No changes in events *)
    | changes -> {
        id = new_envelope.id;
        target = new_envelope.target;
        events = changes;
      }
