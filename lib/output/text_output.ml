open Alsdiff_base
open Alsdiff_live

type t = string

(** Indentation configuration **)
let indent_size = 2  (* Number of spaces per indent level *)

(** Helper to calculate indent from level **)
let indent_of_level level = level * indent_size
let make_indent_at_level level = String.make (indent_of_level level) ' '

(** Helper modules for rendering patches with consistent formatting **)

(** Module for rendering individual field changes with typed formatters **)
module FieldRenderer = struct
  type 'a formatter = {
    format_value : 'a -> string;
    field_name : string;
    indent_level : int;
  }

  let render_simple_change formatter change =
    let prefix = make_indent_at_level formatter.indent_level in
    match change with
    | `Unchanged -> ""
    | `Added v ->
        Printf.sprintf "%s+ %s: %s" prefix formatter.field_name
          (formatter.format_value v)
    | `Removed v ->
        Printf.sprintf "%s- %s: %s" prefix formatter.field_name
          (formatter.format_value v)
    | `Modified m ->
        Printf.sprintf "%s~ %s changed from %s to %s" prefix
          formatter.field_name
          (formatter.format_value m.Diff.old)
          (formatter.format_value m.Diff.new_)

  (* Specialized renderers *)
  let float_formatter ?(indent_level = 1) ?(precision = 4) name =
    {
      format_value = Printf.sprintf "%.*f" precision;
      field_name = name;
      indent_level;
    }

  let bool_formatter ?(indent_level = 1) name =
    { format_value = string_of_bool; field_name = name; indent_level }

  let string_formatter ?(indent_level = 1) name =
    { format_value = (fun s -> s); field_name = name; indent_level }

  let int64_formatter ?(indent_level = 1) name =
    { format_value = Int64.to_string; field_name = name; indent_level }

  let int_formatter ?(indent_level = 1) name =
    { format_value = string_of_int; field_name = name; indent_level }

  let signature_formatter ?(indent_level = 1) name =
    {
      format_value =
        (fun s ->
          Printf.sprintf "%d/%d" s.Clip.TimeSignature.numer
            s.Clip.TimeSignature.denom);
      field_name = name;
      indent_level;
    }

  let device_value_formatter ?(indent_level = 1) field_name =
    {
      format_value = (fun v ->
        match v with
        | Device.Float f -> Printf.sprintf "%.2f" f
        | Device.Int i -> string_of_int i
        | Device.Bool b -> string_of_bool b
        | Device.Enum (i, _) -> string_of_int i);
      field_name;
      indent_level;
    }
end

(** Module for section rendering with consistent indentation *)
module SectionRenderer = struct
  (* Join lines, filtering out empty strings *)
  let join_non_empty lines =
    let non_empty = List.filter (fun s -> s <> "") lines in
    String.concat "\n" non_empty

  (* Indent each line by the given number of spaces *)
  let indent_lines indent_level lines =
    let indent_str = make_indent_at_level indent_level in
    List.map (fun line -> if line = "" then "" else indent_str ^ line) lines

  (* Render a nested section with a header *)
  let render_section ~header ~indent_level content =
    if content = "" then ""
    else header ^ "\n" ^ String.concat "\n" (indent_lines indent_level [ content ])

  (* Render a section from a list of lines *)
  let render_section_from_lines ~header ~indent_level lines =
    let content = join_non_empty lines in
    render_section ~header ~indent_level content

  (* Render a patch field as a section *)
  let render_patch_section ~header ~indent_level ~renderer patch_field =
    match patch_field with
    | `Unchanged -> ""
    | `Patched patch ->
        let content = renderer patch in
        render_section ~header ~indent_level content
end

(** Module for rendering structured changes (list items) *)
module StructuredChangeRenderer = struct
  type 'item item_formatter = { format_item : 'item -> string; indent_level : int }

  type 'patch patch_formatter = {
    format_patch : int -> 'patch -> string;  (* First param is indent_level *)
  }

  let render_change (type item patch) (item_fmt : item item_formatter)
      (patch_fmt : patch patch_formatter)
      (change : (item, patch) Diff.structured_change) =
    match change with
    | `Unchanged -> ""
    | `Added item ->
        let indent_str = make_indent_at_level item_fmt.indent_level in
        let item_name = item_fmt.format_item item in
        let display_name = if item_name = "" then "(unnamed)" else item_name in
        Printf.sprintf "%s+ %s" indent_str display_name
    | `Removed item ->
        let indent_str = make_indent_at_level item_fmt.indent_level in
        let item_name = item_fmt.format_item item in
        let display_name = if item_name = "" then "(unnamed)" else item_name in
        Printf.sprintf "%s- %s" indent_str display_name
    | `Patched patch ->
        (* Pass the same indent level as items to the patch formatter *)
        patch_fmt.format_patch item_fmt.indent_level patch

  (* Render a list of changes into a section *)
  let render_changes_section ~header ~item_fmt ~patch_fmt changes =
    let lines = List.map (render_change item_fmt patch_fmt) changes in
    let non_empty = List.filter (fun s -> s <> "") lines in
    if List.length non_empty > 0 then
      header ^ "\n" ^ String.concat "\n" non_empty
    else ""
end

let render_event_change
    (change :
      ( Automation.EnvelopeEvent.t,
        Automation.EnvelopeEvent.Patch.t )
      Diff.structured_change) =
  match change with
  | `Unchanged -> ""
  | `Added event ->
      Printf.sprintf "    + Event at time %.2f with value %.4f"
        event.Automation.EnvelopeEvent.time event.Automation.EnvelopeEvent.value
  | `Removed event ->
      Printf.sprintf "    - Event at time %.2f with value %.4f"
        event.Automation.EnvelopeEvent.time event.Automation.EnvelopeEvent.value
  | `Patched _patch ->
      (* For now, just show that the event was patched without details *)
      Printf.sprintf "    ~ Event patched (details available)"

let render_envelope_patch (patch : Automation.Patch.t) =
  let header =
    Printf.sprintf "  ~ Patched Envelope (Id: %d, Target: %d):" patch.id
      patch.target
  in
  let event_lines = List.map render_event_change patch.events in
  let non_empty_lines = List.filter (fun s -> s <> "") event_lines in
  String.concat "\n" (header :: non_empty_lines)

let render_envelope_op op =
  match op with
  | `Unchanged -> ""
  | `Added env ->
      Printf.sprintf "+ Added Envelope (Id: %d, Target: %d)" env.Automation.id
        env.Automation.target
  | `Removed env ->
      Printf.sprintf "- Removed Envelope (Id: %d, Target: %d)" env.Automation.id
        env.Automation.target
  | `Patched patch -> render_envelope_patch patch


let render_mixer ?(indent_level = 0) (patch : Mixer.Patch.t) =
  let open FieldRenderer in
  let open SectionRenderer in
  let open StructuredChangeRenderer in
  let header = make_indent_at_level indent_level ^ "Mixer Patch:" in

  let volume_line =
    render_simple_change (float_formatter ~indent_level:(indent_level + 1) "Volume") patch.volume
  in
  let pan_line = render_simple_change (float_formatter ~indent_level:(indent_level + 1) "Pan") patch.pan in
  let mute_line = render_simple_change (bool_formatter ~indent_level:(indent_level + 1) "Mute") patch.mute in
  let solo_line = render_simple_change (bool_formatter ~indent_level:(indent_level + 1) "Solo") patch.solo in

  (* Helper to render send patch *)
  let render_send_patch indent_level (send_patch : Mixer.Send.Patch.t) =
    let target_part =
      match send_patch.target with
      | `Unchanged -> None
      | `Modified m ->
          Some (Printf.sprintf "target: %d->%d" m.Diff.old m.Diff.new_)
      | `Added t -> Some (Printf.sprintf "target: ->%d" t)
      | `Removed t -> Some (Printf.sprintf "target: %d->" t)
    in
    let amount_part =
      match send_patch.amount with
      | `Unchanged -> None
      | `Modified m ->
          Some (Printf.sprintf "amount: %.4f->%.4f" m.Diff.old m.Diff.new_)
      | `Added v -> Some (Printf.sprintf "amount: ->%.4f" v)
      | `Removed v -> Some (Printf.sprintf "amount: %.4f->" v)
    in
    let parts = List.filter_map (fun x -> x) [ target_part; amount_part ] in
    match parts with
    | [] -> ""
    | parts ->
        let indent_str = make_indent_at_level indent_level in
        Printf.sprintf "%s~ Send modified (%s)" indent_str (String.concat ", " parts)
  in

  let send_section =
    render_changes_section ~header:(make_indent_at_level (indent_level + 1) ^ "Send Changes:")
      ~item_fmt:
        {
          format_item =
            (fun s ->
              Printf.sprintf "Send to track %d with amount %.4f"
                s.Mixer.Send.target s.Mixer.Send.amount);
          indent_level = indent_level + 2;
        }
      ~patch_fmt:{ format_patch = render_send_patch }
      patch.sends
  in

  join_non_empty
    [ header; volume_line; pan_line; mute_line; solo_line; send_section ]

let render_loop_section_patch ?(indent_level = 0) (patch : Clip.Loop.Patch.t) =
  let open FieldRenderer in
  let open SectionRenderer in
  let start_line =
    render_simple_change
      (float_formatter ~indent_level ~precision:2 "Loop start")
      patch.start_time
  in
  let end_line =
    render_simple_change
      (float_formatter ~indent_level ~precision:2 "Loop end")
      patch.end_time
  in
  let on_line =
    render_simple_change (bool_formatter ~indent_level "Loop enabled") patch.on
  in

  join_non_empty [ start_line; end_line; on_line ]

let render_sample_ref_patch ?(indent_level = 0) (patch : Clip.SampleRef.Patch.t) =
  let open FieldRenderer in
  let open SectionRenderer in
  let file_path_line =
    render_simple_change
      (string_formatter ~indent_level "File path")
      patch.file_path
  in
  let crc_line =
    render_simple_change (string_formatter ~indent_level "CRC") patch.crc
  in
  let last_modified_line =
    render_simple_change
      (int64_formatter ~indent_level "Last modified")
      patch.last_modified_date
  in

  join_non_empty [ file_path_line; crc_line; last_modified_line ]

let render_audio_clip ?(indent_level = 0) (patch : Clip.AudioClip.Patch.t) =
  let open FieldRenderer in
  let open SectionRenderer in
  let header = make_indent_at_level indent_level ^ "Audio Clip Patch:" in

  let name_line = render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Name") patch.name in
  let start_time_line =
    render_simple_change
      (float_formatter ~indent_level:(indent_level + 1) ~precision:2 "Start time")
      patch.start_time
  in
  let end_time_line =
    render_simple_change
      (float_formatter ~indent_level:(indent_level + 1) ~precision:2 "End time")
      patch.end_time
  in

  let signature_line =
    match patch.signature with
    | `Unchanged -> ""
    | `Modified m ->
        let indent_str = make_indent_at_level (indent_level + 1) in
        Printf.sprintf "%s~ Time signature changed from %d/%d to %d/%d" indent_str
          m.Diff.old.numer m.Diff.old.denom m.Diff.new_.numer m.Diff.new_.denom
  in

  let loop_section =
    match patch.loop with
    | `Unchanged -> ""
    | `Patched loop_patch ->
        let loop_content = render_loop_section_patch ~indent_level:(indent_level + 3) loop_patch in
        if loop_content = "" then ""
        else
          let header_indent = make_indent_at_level (indent_level + 1) in
          header_indent ^ "Loop Changes:\n" ^ loop_content
  in

  let sample_ref_section =
    match patch.sample_ref with
    | `Unchanged -> ""
    | `Patched sample_ref_patch ->
        let sample_ref_content = render_sample_ref_patch ~indent_level:(indent_level + 3) sample_ref_patch in
        if sample_ref_content = "" then ""
        else
          let header_indent = make_indent_at_level (indent_level + 1) in
          header_indent ^ "Sample Reference Changes:\n" ^ sample_ref_content
  in

  join_non_empty
    [
      header;
      name_line;
      start_time_line;
      end_time_line;
      signature_line;
      loop_section;
      sample_ref_section;
    ]

let render_midi_clip ?(indent_level = 0) (patch : Clip.MidiClip.Patch.t) =
  let open FieldRenderer in
  let open SectionRenderer in
  let open StructuredChangeRenderer in
  let header = make_indent_at_level indent_level ^ "Midi Clip Patch:" in

  let name_line = render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Name") patch.name in
  let start_time_line =
    render_simple_change
      (float_formatter ~indent_level:(indent_level + 1) ~precision:2 "Start time")
      patch.start_time
  in
  let end_time_line =
    render_simple_change
      (float_formatter ~indent_level:(indent_level + 1) ~precision:2 "End time")
      patch.end_time
  in

  let signature_line =
    match patch.signature with
    | `Unchanged -> ""
    | `Modified m ->
        let indent_str = make_indent_at_level (indent_level + 1) in
        Printf.sprintf "%s~ Time signature changed from %d/%d to %d/%d" indent_str
          m.Diff.old.numer m.Diff.old.denom m.Diff.new_.numer m.Diff.new_.denom
  in

  let loop_section =
    match patch.loop with
    | `Unchanged -> ""
    | `Patched loop_patch ->
        let loop_content = render_loop_section_patch ~indent_level:(indent_level + 3) loop_patch in
        if loop_content = "" then ""
        else
          let header_indent = make_indent_at_level (indent_level + 1) in
          header_indent ^ "Loop Changes:\n" ^ loop_content
  in

  (* Helper to render note patch *)
  let render_note_patch indent_level (note_patch : Clip.MidiNote.Patch.t) =
    let changes =
      [
        (if note_patch.time <> `Unchanged then
           Some
             (match note_patch.time with
             | `Modified m ->
                 Printf.sprintf "time: %f->%f" m.Diff.old m.Diff.new_
             | _ -> "")
         else None);
        (if note_patch.duration <> `Unchanged then
           Some
             (match note_patch.duration with
             | `Modified m ->
                 Printf.sprintf "duration: %f->%f" m.Diff.old m.Diff.new_
             | _ -> "")
         else None);
        (if note_patch.velocity <> `Unchanged then
           Some
             (match note_patch.velocity with
             | `Modified m ->
                 Printf.sprintf "velocity: %d->%d" m.Diff.old m.Diff.new_
             | _ -> "")
         else None);
        (if note_patch.note <> `Unchanged then
           Some
             (match note_patch.note with
             | `Modified m ->
                 let old_note_name = Clip.MidiNote.get_note_name_from_int m.Diff.old in
                 let new_note_name = Clip.MidiNote.get_note_name_from_int m.Diff.new_ in
                 Printf.sprintf "note: %s->%s" old_note_name new_note_name
             | _ -> "")
         else None);
        (if note_patch.off_velocity <> `Unchanged then
           Some
             (match note_patch.off_velocity with
             | `Modified m ->
                 Printf.sprintf "off_velocity: %d->%d" m.Diff.old m.Diff.new_
             | _ -> "")
         else None);
      ]
    in
    let changes = List.filter_map (fun x -> x) changes in
    let changes_str = String.concat ", " changes in
    let indent_str = make_indent_at_level indent_level in
    Printf.sprintf "%s~ Note changed (%s)" indent_str changes_str
  in

  let notes_section =
    render_changes_section ~header:(make_indent_at_level (indent_level + 1) ^ "Notes Changes:")
      ~item_fmt:
        {
          format_item =
            (fun (note : Clip.MidiNote.t) ->
              let note_name = Clip.MidiNote.get_note_name_from_int note.Clip.MidiNote.note in
              Printf.sprintf "Note: time=%f, duration=%f, velocity=%d, note=%s"
                note.Clip.MidiNote.time note.Clip.MidiNote.duration
                note.Clip.MidiNote.velocity note_name);
          indent_level = indent_level + 2;
        }
      ~patch_fmt:{ format_patch = render_note_patch }
      patch.notes
  in

  join_non_empty
    [
      header;
      name_line;
      start_time_line;
      end_time_line;
      signature_line;
      loop_section;
      notes_section;
    ]

(* Helper functions for Device and Track rendering *)


let render_parameter_fields ?(include_name = false) ?(include_index = false)
    (name_line : string option) (index_line : string option)
    (value_line : string) (auto_line : string) (mod_line : string) =
  let open SectionRenderer in
  let final_name_line =
    if include_name then
      match name_line with None -> "" | Some s -> s
    else "" in
  let final_index_line =
    if include_index then
      match index_line with None -> "" | Some s -> s
    else "" in
  join_non_empty [ final_name_line; final_index_line; value_line; auto_line; mod_line ]

let render_device_param_patch ?(indent_level = 0) (patch : Device.DeviceParam.Patch.t) =
  let open FieldRenderer in
  let open SectionRenderer in
  let value_line =
    render_simple_change (device_value_formatter ~indent_level:(indent_level + 1) "Value") patch.value
  in
  let auto_line =
    render_simple_change (int_formatter ~indent_level:(indent_level + 1) "Automation") patch.automation
  in
  let mod_line =
    render_simple_change (int_formatter ~indent_level:(indent_level + 1) "Modulation") patch.modulation
  in
  join_non_empty [ value_line; auto_line; mod_line ]

let render_plugin_param_patch ?(indent_level = 0) (patch : Device.PluginParam.Patch.t) =
  let open FieldRenderer in
  let name_line =
    Some (render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Name") patch.name)
  in
  let index_line =
    Some (render_simple_change (int_formatter ~indent_level:(indent_level + 1) "Index") patch.index)
  in
  let value_line =
    render_simple_change (device_value_formatter ~indent_level:(indent_level + 1) "Value") patch.value
  in
  let auto_line =
    render_simple_change (int_formatter ~indent_level:(indent_level + 1) "Automation") patch.automation
  in
  let mod_line =
    render_simple_change (int_formatter ~indent_level:(indent_level + 1) "Modulation") patch.modulation
  in
  render_parameter_fields ~include_name:true ~include_index:true name_line index_line value_line
    auto_line mod_line

let render_max4live_param_patch ?(indent_level = 0) (patch : Device.Max4LiveParam.Patch.t) =
  let open FieldRenderer in
  let name_line =
    Some (render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Name") patch.name)
  in
  let index_line =
    Some (render_simple_change (int_formatter ~indent_level:(indent_level + 1) "Index") patch.index)
  in
  let value_line =
    render_simple_change (device_value_formatter ~indent_level:(indent_level + 1) "Value") patch.value
  in
  let auto_line =
    render_simple_change (int_formatter ~indent_level:(indent_level + 1) "Automation") patch.automation
  in
  let mod_line =
    render_simple_change (int_formatter ~indent_level:(indent_level + 1) "Modulation") patch.modulation
  in
  render_parameter_fields ~include_name:true ~include_index:true name_line index_line value_line
    auto_line mod_line

let render_preset_ref_patch ?(indent_level = 0) (patch : Device.PresetRef.Patch.t) =
  let open FieldRenderer in
  let open SectionRenderer in
  let path_line = render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Path") patch.path in
  let pack_name_line =
    render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Pack Name") patch.pack_name
  in
  join_non_empty [ path_line; pack_name_line ]

let render_macro_patch ?(indent_level = 0) (patch : Device.Macro.Patch.t) =
  let open FieldRenderer in
  let open SectionRenderer in
  let name_line =
    render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Name") patch.name
  in
  let manual_line =
    render_simple_change (float_formatter ~indent_level:(indent_level + 1) "Manual") patch.manual
  in
  let auto_line =
    render_simple_change (int_formatter ~indent_level:(indent_level + 1) "Automation") patch.automation
  in
  let mod_line =
    render_simple_change (int_formatter ~indent_level:(indent_level + 1) "Modulation") patch.modulation
  in
  join_non_empty [ name_line; manual_line; auto_line; mod_line ]

let render_snapshot_patch ?(indent_level = 0) (patch : Device.Snapshot.Patch.t) =
  let open FieldRenderer in
  let open SectionRenderer in
  let name_line =
    render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Name") patch.name
  in
  join_non_empty [ name_line ]

let render_automation_patch ?(indent_level = 0) (patch : Automation.Patch.t) =
  let open FieldRenderer in
  let open SectionRenderer in
  let open StructuredChangeRenderer in
  let id_line =
    let indent_str = make_indent_at_level indent_level in
    Printf.sprintf "%sAutomation Envelope (Id: %d, Target: %d)" indent_str patch.id
      patch.target
  in

  let render_event_patch indent_level (event_patch : Automation.EnvelopeEvent.Patch.t) =
    let time_line =
      render_simple_change (float_formatter ~indent_level "Time") event_patch.time
    in
    let value_line =
      render_simple_change (float_formatter ~indent_level "Value") event_patch.value
    in
    join_non_empty [ time_line; value_line ]
  in

  let events_section =
    render_changes_section ~header:(make_indent_at_level (indent_level + 1) ^ "Events Changes:")
      ~item_fmt:
        {
          format_item =
            (fun (event : Automation.EnvelopeEvent.t) ->
              Printf.sprintf "Event: time=%.2f, value=%.2f" event.time
                event.value);
          indent_level = indent_level + 2;
        }
      ~patch_fmt:{ format_patch = render_event_patch }
      patch.events
  in
  join_non_empty [id_line; events_section]

let render_mixer_device_patch ?(_indent_level = 0) (patch : Device.MixerDevice.Patch.t) =

  let open SectionRenderer in
  let on_line =
    match patch.on with
    | `Unchanged -> ""
    | `Patched p -> "  Mixer On: " ^ render_device_param_patch p
  in
  let speaker_line =
    match patch.speaker with
    | `Unchanged -> ""
    | `Patched p -> "  Mixer Speaker: " ^ render_device_param_patch p
  in
  let volume_line =
    match patch.volume with
    | `Unchanged -> ""
    | `Patched p -> "  Mixer Volume: " ^ render_device_param_patch p
  in
  let pan_line =
    match patch.pan with
    | `Unchanged -> ""
    | `Patched p -> "  Mixer Pan: " ^ render_device_param_patch p
  in
  join_non_empty [on_line; speaker_line; volume_line; pan_line]

(* Helper functions for device rendering *)
let render_device_preset_section ~indent_level preset =
  match preset with
  | `Unchanged -> ""
  | `Added p -> Printf.sprintf "%s+ Preset: %s" (make_indent_at_level (indent_level + 1)) p.Device.PresetRef.name
  | `Removed p -> Printf.sprintf "%s- Preset: %s" (make_indent_at_level (indent_level + 1)) p.Device.PresetRef.name
  | `Patched p -> render_preset_ref_patch ~indent_level p

let render_device_params_section ~indent_level ~header ~param_formatter ~patch_formatter params =
  let open StructuredChangeRenderer in
  render_changes_section
    ~header
    ~item_fmt:{
      format_item = param_formatter;
      indent_level = indent_level + 2;
    }
    ~patch_fmt:{ format_patch = fun level patch -> patch_formatter ~indent_level:level patch }
    params

(* Recursive render_device function *)
let rec render_device ?(indent_level = 0) (patch : Device.Patch.t) : string =
  let open FieldRenderer in
  let open SectionRenderer in

  let render_regular_device indent_level (patch : Device.regular_device_patch) =
    let header = make_indent_at_level indent_level ^ "Regular Device Patch:" in
    let name_line = render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Display Name") patch.display_name in

    let params_section =
      render_device_params_section
        ~indent_level
        ~header:(make_indent_at_level (indent_level + 1) ^ "Parameters Changes:")
        ~param_formatter:(fun (p : Device.DeviceParam.t) -> p.name)
        ~patch_formatter:(fun ~indent_level patch -> render_device_param_patch ~indent_level:indent_level patch)
        patch.params
    in

    let preset_section = render_device_preset_section ~indent_level patch.preset in
    join_non_empty [header; name_line; params_section; preset_section]
  in

  let render_plugin_device indent_level (patch : Device.plugin_device_patch) =
    let header = make_indent_at_level indent_level ^ "Plugin Device Patch:" in
    let name_line = render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Display Name") patch.display_name in

    let params_section =
      render_device_params_section
        ~indent_level
        ~header:(make_indent_at_level (indent_level + 1) ^ "Parameters Changes:")
        ~param_formatter:(fun (p : Device.PluginParam.t) -> Printf.sprintf "%s (Index: %d)" p.name p.index)
        ~patch_formatter:(fun ~indent_level patch -> render_plugin_param_patch ~indent_level:indent_level patch)
        patch.params
    in

    let preset_section = render_device_preset_section ~indent_level patch.preset in
    join_non_empty [header; name_line; params_section; preset_section]
  in

  let render_max4live_device indent_level (patch : Device.max4live_device_patch) =
    let header = make_indent_at_level indent_level ^ "Max4Live Device Patch:" in
    let name_line = render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Display Name") patch.display_name in

    let params_section =
      render_device_params_section
        ~indent_level
        ~header:(make_indent_at_level (indent_level + 1) ^ "Parameters Changes:")
        ~param_formatter:(fun (p : Device.Max4LiveParam.t) -> Printf.sprintf "%s (Index: %d)" p.name p.index)
        ~patch_formatter:(fun ~indent_level patch -> render_max4live_param_patch ~indent_level:indent_level patch)
        patch.params
    in

    let preset_section = render_device_preset_section ~indent_level patch.preset in
    join_non_empty [header; name_line; params_section; preset_section]
  in

  let render_group_device indent_level (patch : Device.group_device_patch) =
    let header = make_indent_at_level indent_level ^ "Group Device Patch:" in
    let name_line = render_simple_change (string_formatter ~indent_level:(indent_level + 1) "Display Name") patch.display_name in

    let render_branch_patch branch_indent_level (branch_patch : Device.branch_patch) =
      let open StructuredChangeRenderer in
      let devices_section =
        render_changes_section
          ~header:(make_indent_at_level branch_indent_level ^ "Devices Changes:")
          ~item_fmt:{
            format_item = (fun (d : Device.t) ->
              match d with
              | Regular r -> r.display_name
              | Plugin p -> p.display_name
              | Max4Live m -> m.display_name
              | Group g -> g.display_name
            );
            indent_level = branch_indent_level + 1;
          }
          ~patch_fmt:{
            format_patch = (fun level dp -> render_device ~indent_level:level (
              match dp with
              | Device.RegularPatch p -> Device.Patch.RegularPatch p
              | Device.PluginPatch p -> Device.Patch.PluginPatch p
              | Device.Max4LivePatch p -> Device.Patch.Max4LivePatch p
              | Device.GroupPatch p -> Device.Patch.GroupPatch p
            ));
          }
          branch_patch.devices
      in
      let mixer_section =
        match branch_patch.mixer with
        | `Unchanged -> ""
        | `Patched m -> render_mixer_device_patch m
      in
      join_non_empty [devices_section; mixer_section]
    in

    let branches_section =
      let open StructuredChangeRenderer in
      render_changes_section
        ~header:(make_indent_at_level (indent_level + 1) ^ "Branches Changes:")
        ~item_fmt:{
          format_item = (fun (b : Device.branch) -> Printf.sprintf "Branch %d" b.id);
          indent_level = indent_level + 2;
        }
        ~patch_fmt:{ format_patch = render_branch_patch }
        patch.branches
    in

    let macros_section =
      let open StructuredChangeRenderer in
      render_changes_section
        ~header:(make_indent_at_level (indent_level + 1) ^ "Macros Changes:")
        ~item_fmt:{
          format_item = (fun (m : Device.Macro.t) -> m.name);
          indent_level = indent_level + 2;
        }
        ~patch_fmt:{ format_patch = fun level patch -> render_macro_patch ~indent_level:level patch }
        patch.macros
    in

    let snapshots_section =
      let open StructuredChangeRenderer in
      render_changes_section
        ~header:(make_indent_at_level (indent_level + 1) ^ "Snapshots Changes:")
        ~item_fmt:{
          format_item = (fun (s : Device.Snapshot.t) -> s.name);
          indent_level = indent_level + 2;
        }
        ~patch_fmt:{ format_patch = fun level patch -> render_snapshot_patch ~indent_level:level patch }
        patch.snapshots
    in

    join_non_empty [header; name_line; branches_section; macros_section; snapshots_section]
  in

  match patch with
  | Device.Patch.RegularPatch p -> render_regular_device indent_level p
  | Device.Patch.PluginPatch p -> render_plugin_device indent_level p
  | Device.Patch.Max4LivePatch p -> render_max4live_device indent_level p
  | Device.Patch.GroupPatch p -> render_group_device indent_level p

(* Helper functions for track rendering *)
let render_track_automations_section ~indent_level automations =
  let open StructuredChangeRenderer in
  render_changes_section
    ~header:(make_indent_at_level (indent_level + 1) ^ "Automations Changes:")
    ~item_fmt:{
      format_item = (fun (a : Automation.t) -> Printf.sprintf "Automation %d" a.id);
      indent_level = indent_level + 2;
    }
    ~patch_fmt:{ format_patch = fun level patch -> render_automation_patch ~indent_level:level patch }
    automations

let render_track_devices_section ~indent_level devices =
  let open StructuredChangeRenderer in
  render_changes_section
    ~header:(make_indent_at_level (indent_level + 1) ^ "Devices Changes:")
    ~item_fmt:{
      format_item = (fun (d : Device.t) ->
        match d with
        | Regular r -> r.display_name
        | Plugin p -> p.display_name
        | Max4Live m -> m.display_name
        | Group g -> g.display_name
      );
      indent_level = indent_level + 2;
    }
    ~patch_fmt:{ format_patch = fun level patch -> render_device ~indent_level:level patch }
    devices

let render_track ?(indent_level = 0) (patch : Track.Patch.t) : string =
  let open FieldRenderer in
  let open SectionRenderer in

  let render_track_common track_indent_level ~header ~name ~clips ~automations ~devices ~mixer =
    let name_line = render_simple_change (string_formatter ~indent_level:(track_indent_level + 1) "Name") name in
    let clips_section = clips () in
    let automations_section = render_track_automations_section ~indent_level:track_indent_level automations in
    let devices_section = render_track_devices_section ~indent_level:track_indent_level devices in
    let mixer_section =
      match mixer with
      | `Unchanged -> ""
      | `Patched m -> render_mixer ~indent_level:(track_indent_level + 1) m
    in
    join_non_empty [header; name_line; clips_section; automations_section; devices_section; mixer_section]
  in

  let render_midi_track (patch : Track.MidiTrack.Patch.t) =
    let open StructuredChangeRenderer in
    render_track_common indent_level
      ~header:(make_indent_at_level indent_level ^ "Midi Track Patch:")
      ~name:patch.name
      ~clips:(fun () ->
        render_changes_section
          ~header:(make_indent_at_level (indent_level + 1) ^ "Clips Changes:")
          ~item_fmt:{
            format_item = (fun (c : Clip.MidiClip.t) -> c.name);
            indent_level = indent_level + 2;
          }
          ~patch_fmt:{ format_patch = fun level patch -> render_midi_clip ~indent_level:level patch }
          patch.clips)
      ~automations:patch.automations
      ~devices:patch.devices
      ~mixer:patch.mixer
  in

  let render_audio_track (patch : Track.AudioTrack.Patch.t) =
    let open StructuredChangeRenderer in
    render_track_common indent_level
      ~header:(make_indent_at_level indent_level ^ "Audio Track Patch:")
      ~name:patch.name
      ~clips:(fun () ->
        render_changes_section
          ~header:(make_indent_at_level (indent_level + 1) ^ "Clips Changes:")
          ~item_fmt:{
            format_item = (fun (c : Clip.AudioClip.t) -> c.name);
            indent_level = indent_level + 2;
          }
          ~patch_fmt:{ format_patch = fun level patch -> render_audio_clip ~indent_level:level patch }
          patch.clips)
      ~automations:patch.automations
      ~devices:patch.devices
      ~mixer:patch.mixer
  in

  match patch with
  | Track.Patch.MidiPatch p -> render_midi_track p
  | Track.Patch.AudioPatch p -> render_audio_track p
