open Alcotest
open Alsdiff_base
open Alsdiff_live

(** Helper to load an Automation.t from a file path. *)
let load_automation_from_file (path : string) : Automation.t =
  let xml = Xml.read_file path in
  let envelope_element = Upath.find "/Envelopes/AutomationEnvelope@Id=0" xml |> snd in
  Automation.create envelope_element


(** The main test function for the automation diffing logic. *)
let test_diff_logic () =
  (* 1. Load the old and new states from the XML files. *)
  let old_automation = load_automation_from_file "automation_old.xml" in
  let new_automation = load_automation_from_file "automation.xml" in

  (* 2. Compute the diff between the old and new states. *)
  let patch = Automation.diff old_automation new_automation in

  (* 3. Check the patch fields directly *)
  check int "patch id" new_automation.id patch.id;
  check int "patch target" new_automation.target patch.target;

  (* Check that the event changes are correct *)
  let event_changes = patch.events in
  check int "number of event changes" 3 (List.length event_changes);

  let open Automation in
  check bool "event has time change from 52.0 to 67.0" true
    (List.exists (function
         | `Modified { EnvelopeEvent.Patch.time = `Modified { Diff.oldval; newval }; _ } ->
           let eps = 1e-9 in
           abs_float (oldval -. 52.0) < eps && abs_float (newval -. 67.0) < eps
         | _ -> false
       ) event_changes);

  check bool "event with Id=998 was removed" true
    (List.exists (function
         | `Removed ev -> ev.EnvelopeEvent.id = 998
         | _ -> false
       ) event_changes);

  check bool "event with Id=197 was added" true
    (List.exists (function
         | `Added ev -> ev.EnvelopeEvent.id = 197
         | _ -> false
       ) event_changes)


(* TODO: test cases for IntEvent and EnumEvent *)

(** Alcotest test suite setup. *)
let () =
  run "Diff Automation" [
    "diff-logic", [ test_case "Test automation diffing logic" `Quick test_diff_logic ];
  ]
