open Alsdiff_base
open Alsdiff_base.Diff


module CurveControls = struct
  type t = {
    curve1_x : float;
    curve1_y : float;
    curve2_x : float;
    curve2_y : float;
  } [@@deriving eq]

  (* For curve controls, we use structural equality since there's no natural ID *)
  let has_same_id = equal

  let id_hash t =
    Hashtbl.hash (t.curve1_x, t.curve1_y, t.curve2_x, t.curve2_y)

  module Patch = struct
    type t = {
      curve1_x : float atomic_update;
      curve1_y : float atomic_update;
      curve2_x : float atomic_update;
      curve2_y : float atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.curve1_x &&
      is_unchanged_atomic_update p.curve1_y &&
      is_unchanged_atomic_update p.curve2_x &&
      is_unchanged_atomic_update p.curve2_y
  end

  let diff (old_cc : t) (new_cc : t) : Patch.t =
    {
      curve1_x = diff_atomic_value (module Float) old_cc.curve1_x new_cc.curve1_x;
      curve1_y = diff_atomic_value (module Float) old_cc.curve1_y new_cc.curve1_y;
      curve2_x = diff_atomic_value (module Float) old_cc.curve2_x new_cc.curve2_x;
      curve2_y = diff_atomic_value (module Float) old_cc.curve2_y new_cc.curve2_y;
    }
end


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
    curve : CurveControls.t option;
  } [@@deriving eq]

  (* EnvelopeEvent is a structured type containing primitive fields.
     It represents an automation envelope point that can be added,
     removed, or modified as a unit within the events list. *)

  let create (xml : Xml.t) : t =
    let tag_name = Xml.get_name xml in
    let id = Xml.get_int_attr "Id" xml in
    let time = Xml.get_float_attr "Time" xml in
    let curve =
      match (Xml.get_float_attr_opt "CurveControl1X" xml,
             Xml.get_float_attr_opt "CurveControl1Y" xml,
             Xml.get_float_attr_opt "CurveControl2X" xml,
             Xml.get_float_attr_opt "CurveControl2Y" xml) with
      | (Some c1x, Some c1y, Some c2x, Some c2y) ->
        Some { CurveControls.curve1_x = c1x; curve1_y = c1y;
               curve2_x = c2x; curve2_y = c2y }
      | _ -> None
    in
    let value = match tag_name with
      | "FloatEvent" -> FloatEvent (Xml.get_float_attr "Value" xml)
      | "IntEvent" -> IntEvent (Xml.get_int_attr "Value" xml)
      | "EnumEvent" -> EnumEvent (Xml.get_int_attr "Value" xml)
      | _ -> raise (Xml.Xml_error (xml, "Unknown event type: " ^ tag_name))
    in
    { id; time; value; curve }

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  module Patch = struct
    type t = {
      time : float atomic_update;
      value : event_value atomic_update;
      curve : (CurveControls.t, CurveControls.Patch.t) structured_change;
    }

    let is_empty p =
      is_unchanged_atomic_update p.time &&
      is_unchanged_atomic_update p.value &&
      is_unchanged_change (module CurveControls.Patch) p.curve
  end

  let diff (old_event : t) (new_event : t) : Patch.t =
    if not (has_same_id old_event new_event) then
      failwith "cannot diff two EnvelopeEvents with different Ids"
    else
      let { time = old_time; value = old_value; curve = old_curve; _ } = old_event in
      let { time = new_time; value = new_value; curve = new_curve; _ } = new_event in
      let time_change = diff_atomic_value (module Float) old_time new_time in
      let module EventValueEq = struct
        type t = event_value
        let equal = (=)
      end in
      let value_change = diff_atomic_value (module EventValueEq) old_value new_value in
      let curve_change = diff_complex_value_opt (module CurveControls) old_curve new_curve in
      { time = time_change; value = value_change; curve = curve_change }
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
      |> filter_changes (module EnvelopeEvent.Patch)
    in

    { id = new_envelope.id; target = new_envelope.target; events = event_changes }
