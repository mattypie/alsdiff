open Alsdiff_base
open Alsdiff_base.Diff


module TimeSignature = struct
  type t = { numer : int; denom : int } [@@deriving eq]
  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "RemoteableTimeSignature"; _ } ->
      let numer = Upath.get_int_attr "/Numerator" "Value" xml in
      let denom = Upath.get_int_attr "/Denominator" "Value" xml in
      { numer; denom }
    | _ -> raise (Xml.Invalid_Xml (xml, "Invalid XML element for creating TimeSignature"))

  module Patch = struct
    type t = {
      numer : int atomic_update;
      denom : int atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.numer &&
      is_unchanged_atomic_update p.denom
  end


  let diff (old_sig : t) (new_sig : t) : Patch.t =
    let { numer = old_numer; denom = old_denom } = old_sig in
    let { numer = new_numer; denom = new_denom } = new_sig in
    let numer = diff_atomic_value (module Equality.IntEq) old_numer new_numer in
    let denom = diff_atomic_value (module Equality.IntEq) old_denom new_denom in
    { numer; denom }
end


module MidiNote = struct
  type t = {
    id : int;
    note : int;
    time : float;
    duration : float;
    velocity : int;
    off_velocity : int;
  } [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash a = Hashtbl.hash a.id

  let create (note: int) (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MidiNoteEvent"; _ } ->
      let id = Xml.get_int_attr "NoteId" xml in
      let time = Xml.get_float_attr "Time" xml in
      let duration = Xml.get_float_attr "Duration" xml in
      let velocity = Xml.get_int_attr "Velocity" xml in
      let off_velocity = Xml.get_int_attr "OffVelocity" xml in
      { id; note; time; duration; velocity; off_velocity }
    | _ -> raise (Xml.Invalid_Xml (xml, "Invalid XML element for creating MidiNote"))


  module Patch = struct
    type t = {
      time : float atomic_update;
      duration : float atomic_update;
      velocity : int atomic_update;
      off_velocity : int atomic_update;
      note : int atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.time &&
      is_unchanged_atomic_update p.duration &&
      is_unchanged_atomic_update p.velocity &&
      is_unchanged_atomic_update p.off_velocity &&
      is_unchanged_atomic_update p.note

  end


  let diff (old_note : t) (new_note : t) : Patch.t =
    let  { id = _; time = old_time; duration = old_duration; velocity = old_velocity; off_velocity = old_off_velocity; note = old_note } : t = old_note in
    let { id = _; time = new_time; duration = new_duration; velocity = new_velocity; off_velocity = new_off_velocity; note = new_note } : t = new_note in

    let time_change = diff_atomic_value (module Equality.FloatEq) old_time new_time in
    let duration_change = diff_atomic_value (module Equality.FloatEq) old_duration new_duration in
    let velocity_change = diff_atomic_value (module Equality.IntEq) old_velocity new_velocity in
    let off_velocity_change = diff_atomic_value (module Equality.IntEq) old_off_velocity new_off_velocity in
    let note_change = diff_atomic_value (module Equality.IntEq) old_note new_note in
    {
      time = time_change;
      duration = duration_change;
      velocity = velocity_change;
      off_velocity = off_velocity_change;
      note = note_change;
    }

  type note_display_style = Sharp | Flat

  let get_note_name_from_int ?(style=Sharp) (note_int : int) : string =
    let note_names_sharp = [| "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B" |] in
    let note_names_flat = [| "C"; "Db"; "D"; "Eb"; "E"; "F"; "Gb"; "G"; "Ab"; "A"; "Bb"; "B" |] in

    let note_class = note_int mod 12 in
    let octave_num = note_int / 12 - 1 in (* MIDI octave adjustment *)

    let note_name = match style with
      | Sharp -> note_names_sharp.(note_class)
      | Flat -> note_names_flat.(note_class)
    in

    Printf.sprintf "%s%d" note_name octave_num

let get_note_name ?(style=Sharp) (note : t) : string =
  note.note |> get_note_name_from_int ~style

end

module Loop = struct
  type t = {
    start_time : float;
    end_time : float;
    on : bool;
  } [@@deriving eq]


  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "Loop"; _ } ->
      let start = Upath.get_float_attr "/LoopStart" "Value" xml in
      let end_ = Upath.get_float_attr "/LoopEnd" "Value" xml in
      let on = Upath.get_bool_attr "/LoopOn" "Value" xml in
      { start_time = start; end_time = end_; on; }
    | _ -> raise (Xml.Invalid_Xml (xml, "Invalid XML element for creating Loop"))


  module Patch = struct
    type t = {
      start_time : float atomic_update;
      end_time : float atomic_update;
      on : bool atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.start_time &&
      is_unchanged_atomic_update p.end_time &&
      is_unchanged_atomic_update p.on

  end


  let diff (old_loop : t) (new_loop : t) : Patch.t =
      let start_time_change = diff_atomic_value (module Equality.FloatEq) old_loop.start_time new_loop.start_time in
      let end_time_change = diff_atomic_value (module Equality.FloatEq) old_loop.end_time new_loop.end_time in
      let on_change = diff_atomic_value (module Equality.BoolEq) old_loop.on new_loop.on in
      { start_time = start_time_change; end_time = end_time_change; on = on_change }
end


module MidiClip = struct
  type t = {
    id : int;
    name : string;
    start_time : float;
    end_time : float;
    loop : Loop.t;
    signature : TimeSignature.t;
    notes : MidiNote.t list;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MidiClip"; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr "/Name" "Value" xml in
      let start_time = Upath.get_float_attr "/CurrentStart" "Value" xml in

      (* Extract end time from CurrentEnd *)
      let end_time = Upath.get_float_attr "/CurrentEnd" "Value" xml in

      (* Extract loop information *)
      let loop = Upath.find "/Loop" xml |> snd |> Loop.create in

      (* Extract time signature *)
      let signature = Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature" xml |> snd |> TimeSignature.create in

      (* Extract MIDI notes from KeyTracks *)
      let notes = Upath.find_all_seq "/Notes/KeyTracks/KeyTrack" xml
        |> Seq.map snd
        |> Seq.flat_map (fun keytrack ->
            let key = Upath.get_int_attr "MidiKey" "Value" keytrack in
            Upath.find_all_seq "/Notes/MidiNoteEvent" keytrack
              |> Seq.map snd
              |> Seq.map @@ MidiNote.create key)
        |> List.of_seq
      in

      { id; name; start_time; end_time; loop; signature; notes }
    | _ -> raise (Xml.Invalid_Xml (xml, "Expected MidiClip element"))

  let has_same_id a b = a.id = b.id
  let id_hash t = Hashtbl.hash t.id


  module Patch = struct
    type note_change = (MidiNote.t, MidiNote.Patch.t) structured_change

    type t = {
      name : string atomic_update;
      start_time : float atomic_update;
      end_time : float atomic_update;
      loop : Loop.Patch.t structured_update;
      signature : TimeSignature.Patch.t structured_update;
      notes : note_change list;
    }

    let is_empty p =
      is_unchanged_atomic_update p.name &&
      is_unchanged_atomic_update p.start_time &&
      is_unchanged_atomic_update p.end_time &&
      is_unchanged_update (module Loop.Patch) p.loop &&
      is_unchanged_update (module TimeSignature.Patch) p.signature &&
      List.for_all (is_unchanged_change (module MidiNote.Patch)) p.notes
  end

  let diff (old_clip : t) (new_clip : t) : Patch.t =
    let { id = old_id; name = old_name; start_time = old_start; end_time = old_end; loop = old_loop; signature = old_sig; notes = old_notes } = old_clip in
    let { id = new_id; name = new_name; start_time = new_start; end_time = new_end; loop = new_loop; signature = new_sig; notes = new_notes } = new_clip in

    (* Only compare clips with the same id *)
    if old_id <> new_id then
      failwith "cannot diff two clips with different Id"
    else
      let name_change = diff_atomic_value (module Equality.StringEq) old_name new_name in
      let start_time_change = diff_atomic_value (module Equality.FloatEq) old_start new_start in
      let end_time_change = diff_atomic_value (module Equality.FloatEq) old_end new_end in
      let signature_change =
        diff_complex_value (module TimeSignature) old_sig new_sig in
    (* Use diff_list for notes - cleaner and more consistent *)
    let notes_change =
      diff_list_id (module MidiNote) old_notes new_notes
    in
    let loop_change = diff_complex_value (module Loop) old_loop new_loop in

    {
      name = name_change;
      start_time = start_time_change;
      end_time = end_time_change;
      loop = loop_change;
      signature = signature_change;
      notes = notes_change;
    }
end


module SampleRef = struct
  type t = {
    file_path : string;
    crc : string;
    last_modified_date : int64; (* unix timestamp *)
  } [@@deriving eq]

  module Patch = struct
    type t = {
      file_path : string atomic_update;
      crc : string atomic_update;
      last_modified_date : int64 atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.file_path &&
      is_unchanged_atomic_update p.crc &&
      is_unchanged_atomic_update p.last_modified_date
  end

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "SampleRef"; _ } ->
      let last_modified_date = Upath.get_int64_attr "LastModDate" "Value" xml in
      let file_path = Upath.get_attr "FileRef/Path" "Value" xml in
      let crc = Upath.get_attr "FileRef/OriginalCrc" "Value" xml in
      { file_path; crc; last_modified_date }
    | _ -> raise (Xml.Invalid_Xml (xml, "Invalid XML element for creating SampleRef"))


  let diff (old_sample_ref : t) (new_sample_ref : t) : Patch.t =
    let { file_path = old_file_path; crc = old_crc; last_modified_date = old_date } = old_sample_ref in
    let { file_path = new_file_path; crc = new_crc; last_modified_date = new_date } = new_sample_ref in

    let file_path_change = diff_atomic_value (module Equality.StringEq) old_file_path new_file_path in
    let crc_change = diff_atomic_value (module Equality.StringEq) old_crc new_crc in
    let last_modified_date_change = diff_atomic_value (module Equality.Int64Eq) old_date new_date in
    { file_path = file_path_change; crc = crc_change; last_modified_date = last_modified_date_change }

end


module AudioClip = struct
  (* TODO:
     1. support warp related settings
     2. fades support
  *)
  type t = {
    id : int;
    name : string;
    start_time : float;
    end_time : float;
    loop : Loop.t;
    signature : TimeSignature.t;
    sample_ref : SampleRef.t;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "AudioClip"; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr "/Name" "Value" xml in
      let start_time = Upath.get_float_attr "/CurrentStart" "Value" xml in
      let end_time = Upath.get_float_attr "/CurrentEnd" "Value" xml in

      (* Extract loop information *)
      (* TODO: what the fuck does `StartRelative` means in the `Loop` element *)
      let loop = Upath.find "/Loop" xml |> snd |> Loop.create in
      (* Extract time signature *)
      (* TODO: support time signature automation *)
      let signature = Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature" xml |> snd |> TimeSignature.create in
      (* Extract sample reference *)
      let sample_ref = Upath.find "/SampleRef" xml |> snd |> SampleRef.create in
      { id; name; start_time; end_time; loop; signature; sample_ref }
    | _ -> raise (Xml.Invalid_Xml (xml, "Expected AudioClip element"))

  let has_same_id a b = a.id = b.id
  let id_hash t = Hashtbl.hash t.id


  module Patch = struct
    type t = {
      name : string atomic_update;
      start_time : float atomic_update;
      end_time : float atomic_update;
      loop : Loop.Patch.t structured_update;
      signature : TimeSignature.Patch.t structured_update;
      sample_ref : SampleRef.Patch.t structured_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.name &&
      is_unchanged_atomic_update p.start_time &&
      is_unchanged_atomic_update p.end_time &&
      is_unchanged_update (module Loop.Patch) p.loop &&
      is_unchanged_update (module TimeSignature.Patch) p.signature &&
      is_unchanged_update (module SampleRef.Patch) p.sample_ref
  end


  let diff (old_clip : t) (new_clip : t) : Patch.t =
    let { id = old_id; name = old_name; start_time = old_start; end_time = old_end; loop = old_loop; signature = old_sig; sample_ref = old_sample } = old_clip in
    let { id = new_id; name = new_name; start_time = new_start; end_time = new_end; loop = new_loop; signature = new_sig; sample_ref = new_sample } = new_clip in

    (* Only compare clips with the same id *)
    if old_id <> new_id then
      failwith "cannot diff two clips with different Id"
    else
      let name_change = diff_atomic_value (module Equality.StringEq) old_name new_name in
      let start_time_change = diff_atomic_value (module Equality.FloatEq) old_start new_start in
      let end_time_change = diff_atomic_value (module Equality.FloatEq) old_end new_end in
      let loop_change = diff_complex_value (module Loop) old_loop new_loop in
      let signature_change = diff_complex_value (module TimeSignature) old_sig new_sig in
      let sample_ref_change = diff_complex_value (module SampleRef) old_sample new_sample in
      {
        name = name_change;
        start_time = start_time_change;
        end_time = end_time_change;
        loop = loop_change;
        signature = signature_change;
        sample_ref = sample_ref_change;
      }
end
