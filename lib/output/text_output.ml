open Alsdiff_live

type t = string

let render_event_change (change : Automation.EnvelopeEvent.t Diff.flat_change) =
  match change with
  | `Unchanged -> ""
  | `Added event ->
      Printf.sprintf "    + Event at time %.2f with value %.4f"
        event.Automation.EnvelopeEvent.time event.Automation.EnvelopeEvent.value
  | `Removed event ->
      Printf.sprintf "    - Event at time %.2f with value %.4f"
        event.Automation.EnvelopeEvent.time event.Automation.EnvelopeEvent.value
  | `Modified m ->
      Printf.sprintf "    ~ Event changed: time %.2f->%.2f, value %.4f->%.4f"
        m.old.Automation.EnvelopeEvent.time m.new_.Automation.EnvelopeEvent.time m.old.Automation.EnvelopeEvent.value m.new_.Automation.EnvelopeEvent.value

let render_envelope_patch (patch : Automation.Patch.t) =
  let header =
    Printf.sprintf "  ~ Patched Envelope (Id: %d, Target: %d):"
      patch.id patch.target
  in
  let event_lines = List.map render_event_change patch.events in
  let non_empty_lines = List.filter (fun s -> s <> "") event_lines in
  String.concat "\n" (header :: non_empty_lines)

let render_envelope_op op =
  match op with
  | `Unchanged -> ""
  | `Added env ->
      Printf.sprintf "+ Added Envelope (Id: %d, Target: %d)"
        env.Automation.id env.Automation.target
  | `Removed env ->
      Printf.sprintf "- Removed Envelope (Id: %d, Target: %d)"
        env.Automation.id env.Automation.target
  | `Patched patch ->
      render_envelope_patch patch

let render_automation_patch (patch : Automation.Patch.t) =
  let header = Printf.sprintf "Automation Patch (Id: %d, Target: %d):" patch.id patch.target in
  let event_lines = List.map render_event_change patch.events in
  let non_empty_lines = List.filter (fun s -> s <> "") event_lines in
  header ^ (String.concat "\n" non_empty_lines)

let render_mixer (patch : Mixer.Patch.t) =
    let header = "Mixer Patch:" in

    (* Render volume change *)
    let volume_line =
      match patch.volume with
      | `Unchanged -> ""
      | `Added v -> Printf.sprintf "  + Volume: %.4f" v
      | `Removed v -> Printf.sprintf "  - Volume: %.4f" v
      | `Modified m -> Printf.sprintf "  ~ Volume changed from %.4f to %.4f" m.old m.new_
    in

    (* Render pan change *)
    let pan_line =
      match patch.pan with
      | `Unchanged -> ""
      | `Added p -> Printf.sprintf "  + Pan: %.4f" p
      | `Removed p -> Printf.sprintf "  - Pan: %.4f" p
      | `Modified m -> Printf.sprintf "  ~ Pan changed from %.4f to %.4f" m.old m.new_
    in

    (* Render mute change *)
    let mute_line =
      match patch.mute with
      | `Unchanged -> ""
      | `Added b -> Printf.sprintf "  + Mute: %b" b
      | `Removed b -> Printf.sprintf "  - Mute: %b" b
      | `Modified m -> Printf.sprintf "  ~ Mute changed from %b to %b" m.old m.new_
    in

    (* Render solo change *)
    let solo_line =
      match patch.solo with
      | `Unchanged -> ""
      | `Added b -> Printf.sprintf "  + Solo: %b" b
      | `Removed b -> Printf.sprintf "  - Solo: %b" b
      | `Modified m -> Printf.sprintf "  ~ Solo changed from %b to %b" m.old m.new_
    in

    (* Render send changes *)
    let render_send_change change =
      match change with
      | `Unchanged -> ""
      | `Added send -> Printf.sprintf "  + Send to track %d with amount %.4f" send.Mixer.Send.target send.Mixer.Send.amount
      | `Removed send -> Printf.sprintf "  - Send to track %d with amount %.4f" send.Mixer.Send.target send.Mixer.Send.amount
      | `Patched patch ->
          (* For a patched send, we have a SendPatch.t *)
          let target_change_part =
            match patch.Mixer.Send.Patch.target with
            | `Unchanged -> None
            | `Modified m -> Some (Printf.sprintf "target: %d->%d" m.old m.new_)
            | `Added t -> Some (Printf.sprintf "target: ->%d" t)
            | `Removed t -> Some (Printf.sprintf "target: %d->" t)
          in
          let amount_change_part =
            match patch.Mixer.Send.Patch.amount with
            | `Unchanged -> None
            | `Modified m -> Some (Printf.sprintf "amount: %.4f->%.4f" m.old m.new_)
            | `Added v -> Some (Printf.sprintf "amount: ->%.4f" v)
            | `Removed v -> Some (Printf.sprintf "amount: %.4f->" v)
          in
          let parts = List.filter_map (fun x -> x) [target_change_part; amount_change_part] in
          match parts with
          | [] -> ""
          | [part] -> Printf.sprintf "    ~ Send modified (%s)" part
          | parts -> Printf.sprintf "    ~ Send modified (%s)" (String.concat ", " parts)
    in

    let send_lines = List.map render_send_change patch.sends in
    let non_empty_send_lines = List.filter (fun s -> s <> "") send_lines in
    let send_section =
      if List.length non_empty_send_lines > 0 then
        "  Send Changes:\n" ^ (String.concat "\n" (List.map (fun s -> "  " ^ s) non_empty_send_lines))
      else
        ""
    in

    let all_lines = [header; volume_line; pan_line; mute_line; solo_line; send_section] in
    let non_empty_lines = List.filter (fun s -> s <> "") all_lines in
    String.concat "\n" non_empty_lines

  let render_loop_section_patch (patch : Clip.Loop.Patch.t) =
    (* Render start time change *)
    let start_line =
      match patch.start_time with
      | `Unchanged -> ""
      | `Added v -> Printf.sprintf "    + Loop start: %.2f" v
      | `Removed v -> Printf.sprintf "    - Loop start: %.2f" v
      | `Modified m -> Printf.sprintf "    ~ Loop start changed from %.2f to %.2f" m.old m.new_
    in

    (* Render end time change *)
    let end_line =
      match patch.end_time with
      | `Unchanged -> ""
      | `Added v -> Printf.sprintf "    + Loop end: %.2f" v
      | `Removed v -> Printf.sprintf "    - Loop end: %.2f" v
      | `Modified m -> Printf.sprintf "    ~ Loop end changed from %.2f to %.2f" m.old m.new_
    in

    (* Render loop on/off change *)
    let on_line =
      match patch.on with
      | `Unchanged -> ""
      | `Added b -> Printf.sprintf "    + Loop enabled: %b" b
      | `Removed b -> Printf.sprintf "    - Loop enabled: %b" b
      | `Modified m -> Printf.sprintf "    ~ Loop enabled changed from %b to %b" m.old m.new_
    in

    let all_lines = [start_line; end_line; on_line] in
    let non_empty_lines = List.filter (fun s -> s <> "") all_lines in
    String.concat "\n" non_empty_lines

  let render_sample_ref_patch (patch : Clip.SampleRef.Patch.t) =
    (* Render file path change *)
    let file_path_line =
      match patch.file_path with
      | `Unchanged -> ""
      | `Added p -> Printf.sprintf "    + File path: %s" p
      | `Removed p -> Printf.sprintf "    - File path: %s" p
      | `Modified m -> Printf.sprintf "    ~ File path changed from %s to %s" m.old m.new_
    in

    (* Render CRC change *)
    let crc_line =
      match patch.crc with
      | `Unchanged -> ""
      | `Added c -> Printf.sprintf "    + CRC: %s" c
      | `Removed c -> Printf.sprintf "    - CRC: %s" c
      | `Modified m -> Printf.sprintf "    ~ CRC changed from %s to %s" m.old m.new_
    in

    (* Render last modified date change *)
    let last_modified_line =
      match patch.last_modified_date with
      | `Unchanged -> ""
      | `Added d -> Printf.sprintf "    + Last modified: %Ld" d
      | `Removed d -> Printf.sprintf "    - Last modified: %Ld" d
      | `Modified m -> Printf.sprintf "    ~ Last modified changed from %Ld to %Ld" m.old m.new_
    in

    let all_lines = [file_path_line; crc_line; last_modified_line] in
    let non_empty_lines = List.filter (fun s -> s <> "") all_lines in
    String.concat "\n" non_empty_lines

  let render_audio_clip (patch : Clip.AudioClip.Patch.t) =
    let header = "Audio Clip Patch:" in

    (* Render name change *)
    let name_line =
      match patch.name with
      | `Unchanged -> ""
      | `Modified m -> Printf.sprintf "  ~ Name changed from %s to %s" m.old m.new_
    in

    (* Render start time change *)
    let start_time_line =
      match patch.start_time with
      | `Unchanged -> ""
      | `Modified m -> Printf.sprintf "  ~ Start time changed from %.2f to %.2f" m.old m.new_
    in

    (* Render end time change *)
    let end_time_line =
      match patch.end_time with
      | `Unchanged -> ""
      | `Modified m -> Printf.sprintf "  ~ End time changed from %.2f to %.2f" m.old m.new_
    in

    (* Render signature change *)
    let signature_line =
      match patch.signature with
      | `Unchanged -> ""
      | `Modified m -> Printf.sprintf "  ~ Time signature changed from %d/%d to %d/%d"
          m.old.numer m.old.denom
          m.new_.numer m.new_.denom
    in

    (* Render loop change *)
    let loop_section =
      match patch.loop with
      | `Unchanged -> ""
      | `Patched loop_patch ->
          let loop_content = render_loop_section_patch loop_patch in
          if loop_content = "" then ""
          else "  Loop Changes:\n" ^ (String.concat "\n" (List.map (fun s -> "  " ^ s) [loop_content]))
    in

    (* Render sample reference change *)
    let sample_ref_section =
      match patch.sample_ref with
      | `Unchanged -> ""
      | `Patched sample_ref_patch ->
          let sample_ref_content = render_sample_ref_patch sample_ref_patch in
          if sample_ref_content = "" then ""
          else "  Sample Reference Changes:\n" ^ (String.concat "\n" (List.map (fun s -> "  " ^ s) [sample_ref_content]))
    in

    let all_lines = [header; name_line; start_time_line; end_time_line; signature_line; loop_section; sample_ref_section] in
    let non_empty_lines = List.filter (fun s -> s <> "") all_lines in
    String.concat "\n" non_empty_lines

  let render_midi_clip (patch : Clip.MidiClip.Patch.t) =
    let header = "Midi Clip Patch:" in

    (* Render name change *)
    let name_line =
      match patch.name with
      | `Unchanged -> ""
      | `Modified m -> Printf.sprintf "  ~ Name changed from %s to %s" m.old m.new_
    in

    (* Render start time change *)
    let start_time_line =
      match patch.start_time with
      | `Unchanged -> ""
      | `Modified m -> Printf.sprintf "  ~ Start time changed from %.2f to %.2f" m.old m.new_
    in

    (* Render end time change *)
    let end_time_line =
      match patch.end_time with
      | `Unchanged -> ""
      | `Modified m -> Printf.sprintf "  ~ End time changed from %.2f to %.2f" m.old m.new_
    in

    (* Render signature change *)
    let signature_line =
      match patch.signature with
      | `Unchanged -> ""
      | `Modified m -> Printf.sprintf "  ~ Time signature changed from %d/%d to %d/%d"
          m.old.numer m.old.denom
          m.new_.numer m.new_.denom
    in

    (* Render loop change *)
    let loop_section =
      match patch.loop with
      | `Unchanged -> ""
      | `Patched loop_patch ->
          let loop_content = render_loop_section_patch loop_patch in
          if loop_content = "" then ""
          else "  Loop Changes:\n" ^ (String.concat "\n" (List.map (fun s -> "  " ^ s) [loop_content]))
    in

    (* Render notes changes *)
    let render_note_change (change : Clip.MidiClip.Patch.note_change) =
      match change with
      | `Unchanged -> ""
      | `Added note -> Printf.sprintf "    + Note: time=%d, duration=%d, velocity=%d, note=%d"
          note.time note.duration note.velocity note.note
      | `Removed note -> Printf.sprintf "    - Note: time=%d, duration=%d, velocity=%d, note=%d"
          note.time note.duration note.velocity note.note
      | `Patched patch ->
          let changes = [
            if patch.time <> `Unchanged then Some (match patch.time with
              | `Modified m -> Printf.sprintf "time: %d->%d" m.old m.new_
              | _ -> "") else None;
            if patch.duration <> `Unchanged then Some (match patch.duration with
              | `Modified m -> Printf.sprintf "duration: %d->%d" m.old m.new_
              | _ -> "") else None;
            if patch.velocity <> `Unchanged then Some (match patch.velocity with
              | `Modified m -> Printf.sprintf "velocity: %d->%d" m.old m.new_
              | _ -> "") else None;
            if patch.note <> `Unchanged then Some (match patch.note with
              | `Modified m -> Printf.sprintf "note: %d->%d" m.old m.new_
              | _ -> "") else None;
            if patch.off_velocity <> `Unchanged then Some (match patch.off_velocity with
              | `Modified m -> Printf.sprintf "off_velocity: %d->%d" m.old m.new_
              | _ -> "") else None;
          ] in
          let changes = List.filter_map (fun x -> x) changes in
          let changes_str = String.concat ", " changes in
          Printf.sprintf "    ~ Note changed (%s)" changes_str
    in
    let notes_section =
      let note_lines = List.map render_note_change patch.notes in
      let non_empty_note_lines = List.filter (fun s -> s <> "") note_lines in
      if List.length non_empty_note_lines > 0 then
        "  Notes Changes:\n" ^ (String.concat "\n" non_empty_note_lines)
      else ""
    in

    let all_lines = [header; name_line; start_time_line; end_time_line; signature_line; loop_section; notes_section] in
    let non_empty_lines = List.filter (fun s -> s <> "") all_lines in
    String.concat "\n" non_empty_lines
