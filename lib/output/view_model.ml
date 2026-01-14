open Alsdiff_live
open Alsdiff_base.Diff
open Ptime.Span

type note_display_style = Sharp | Flat [@@deriving yojson, jsonschema]

let get_note_name_from_int ?(style : note_display_style = Sharp) (note_int : int) : string =
  let note_names_sharp = [| "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B" |] in
  let note_names_flat = [| "C"; "Db"; "D"; "Eb"; "E"; "F"; "Gb"; "G"; "Ab"; "A"; "Bb"; "B" |] in

  let note_class = note_int mod 12 in
  let octave_num = note_int / 12 - 1 in (* MIDI octave adjustment *)

  let note_name = match style with
    | Sharp -> note_names_sharp.(note_class)
    | Flat -> note_names_flat.(note_class)
  in

  Printf.sprintf "%s%d" note_name octave_num

let get_note_name ?(style : note_display_style = Sharp) (note : int) : string =
  get_note_name_from_int note ~style

(** [format_unix_timestamp] converts a Unix timestamp (int) to a human-readable datetime string.
    Returns "Invalid timestamp" if the timestamp is out of range for ptime.
    @param ts Unix timestamp as int (seconds since epoch)
    @return Formatted datetime string in ISO 8601 format (e.g., "2024-01-15 14:30:00")
*)
let format_unix_timestamp (ts : int) : string =
  (* Convert int seconds to ptime *)
  let span = of_int_s ts in
  match Ptime.of_span span with
  | None -> "Invalid timestamp"
  | Some t ->
    (* Format as RFC 3339 with space instead of T, no decimal places for seconds *)
    Ptime.to_rfc3339 ~space:true ~frac_s:0 ~tz_offset_s:0 t

type field_value =
  | Fint of int
  | Ffloat of float
  | Fbool of bool
  | Fstring of string

let int_value x = Fint x
let float_value x = Ffloat x
let bool_value x = Fbool x
let string_value x = Fstring x


(** [option_to_list] converts an option to a list. *)
let option_to_list = function
  | Some x -> [x]
  | None -> []


type change_type =
  | Unchanged
  | Added
  | Removed
  | Modified
[@@deriving yojson]

type domain_type =
  | DTLiveset
  | DTTrack
  | DTDevice
  | DTClip
  | DTAutomation
  | DTMixer
  | DTRouting
  | DTLocator
  | DTParam
  | DTNote
  | DTEvent
  | DTSend
  | DTPreset
  | DTMacro
  | DTSnapshot
  | DTLoop
  | DTSignature
  | DTSampleRef
  | DTVersion
  | DTOther
[@@deriving yojson, jsonschema]


(** Unified type system (3 types instead of 4) - Element and Section unified into Item *)

type field = {
  name : string;
  change : change_type;
  domain_type : domain_type;
  oldval : field_value option;
  newval : field_value option;
}

and item = {
  name : string;
  change : change_type;
  domain_type : domain_type;
  children : view list;
}

and collection = {
  name : string;
  change : change_type;
  domain_type : domain_type;
  items : view list;
}

and view =
  | Field of field
  | Item of item
  | Collection of collection

type ('a, 'p) field_descriptor =
  | FieldDesc : {
      name : string;
      of_parent_value : 'a -> 'b;  (* Get the field value when parent is Added/Removed *)
      of_parent_patch : 'p -> 'b atomic_update;  (* Get the field update when parent is Modified *)
      wrapper : 'b -> field_value;
    } -> ('a, 'p) field_descriptor


(** ViewBuilder module - uses the unified 3-type system (Field, Item, Collection) *)
module ViewBuilder = struct

  (** [change_type_of c] extracts the change type from a structured change. *)
  let change_type_of (c : ('a, 'p) structured_change) : change_type =
    match c with
    | `Added _ -> Added
    | `Removed _ -> Removed
    | `Modified _ -> Modified
    | `Unchanged -> Unchanged

  let map_atomic_change (f : 'a -> 'b) (c : 'a atomic_change) : 'b atomic_change =
    match c with
    | `Added x -> `Added (f x)
    | `Removed x -> `Removed (f x)
    | `Modified { oldval; newval } -> `Modified { oldval = f oldval; newval = f newval }
    | `Unchanged -> `Unchanged

  let map_atomic_update (f : 'a -> 'b) (u : 'a atomic_update) : 'b atomic_update =
    match u with
    | `Modified { oldval; newval } -> `Modified { oldval = f oldval; newval = f newval }
    | `Unchanged -> `Unchanged

  (** [build_field c fd] build a [new_field] from the element change [c] with given field descriptor [fd].
      @param c the element change [('a, 'p) change]
      @param fd the field descriptor
      @param domain_type the domain type for this field
  *)
  let build_field
      (c : ('a, 'p) structured_change)
      (FieldDesc fd : ('a, 'p) field_descriptor)
      ~(domain_type : domain_type)
    : field =

    let name = fd.name in
    let (change_type, oldval, newval) =
      match c with
      | `Added a -> (Added, None, Some (fd.wrapper @@ fd.of_parent_value a))
      | `Removed r -> (Removed, Some (fd.wrapper @@ fd.of_parent_value r), None)
      | `Modified p ->
        let field_update = fd.of_parent_patch p in
        (match field_update with
         | `Modified { oldval; newval } -> (Modified, Some (fd.wrapper oldval), Some (fd.wrapper newval))
         | `Unchanged -> (Unchanged, None, None))
      | `Unchanged -> (Unchanged, None, None)
    in
    { name; change = change_type; domain_type; oldval; newval }


  (** [build_item_from_fields c ~name ~domain_type ~field_descs]
      builds a [item] with field children.

      @param c the element change
      @param name the item name
      @param domain_type the domain type for this item
      @param field_descs list of field descriptors
      @return a item with Field children
  *)
  let build_item_from_fields
      (c : ('a, 'p) structured_change)
      ~(name : string)
      ~(domain_type : domain_type)
      ~(field_descs : ('a, 'p) field_descriptor list)
    : item =
    let change_type = change_type_of c in
    let children = field_descs |> List.map (fun fd -> Field (build_field c fd ~domain_type)) in
    { name; change = change_type; domain_type; children }


  (** [build_item_from_children c ~name ~of_value ~of_patch ~build_value_children ~build_patch_children]
      builds a [item] with arbitrary children for a nested structured type.

      This is the new equivalent of [build_nested_section_view], but returns a [item] instead
      of [section_view], and can contain any [new_view] children (not just [view]).

      @param c the parent structured change
      @param name the item name
      @param of_value extracts the nested value from the parent value
      @param of_patch extracts the nested update from the parent patch
      @param build_value_children builds view list from nested value and change type
      @param build_patch_children builds view list from nested patch
      @param domain_type the domain type for this item
      @return Some item if there are children, None otherwise
  *)
  let build_item_from_children
      (c : ('parent, 'pp) structured_change)
      ~(name : string)
      ~(of_value : 'parent -> 'nested)
      ~(of_patch : 'pp -> 'np structured_update)
      ~(build_value_children : change_type -> 'nested -> view list)
      ~(build_patch_children : 'np -> view list)
      ~(domain_type : domain_type)
    : item option =
    match c with
    | `Added parent ->
      let nested_val = of_value parent in
      let children = build_value_children Added nested_val in
      if children = [] then None
      else Some { name; change = Added; domain_type; children }
    | `Removed parent ->
      let nested_val = of_value parent in
      let children = build_value_children Removed nested_val in
      if children = [] then None
      else Some { name; change = Removed; domain_type; children }
    | `Modified patch ->
      (match of_patch patch with
       | `Unchanged ->
         (* Nested content is unchanged but parent is Modified.
            Create a placeholder item with empty children - the rendering layer
            will decide whether to show it based on the preset. *)
         Some { name; change = Unchanged; domain_type; children = [] }
       | `Modified np ->
         let children = build_patch_children np in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children })
    | `Unchanged ->
      (* Parent is unchanged - we don't have access to the value to extract nested content.
         This is a fundamental limitation - unchanged items don't carry their values. *)
      None


  (** [build_item_from_children_with_change c ~name ~of_value ~of_patch ~build_value_children ~build_patch_children]
      builds a [item] for a nested structured type that may be added/removed independently.

      This is the new equivalent of [build_nested_section_view_with_change], but returns a [item].

      @param c the parent structured change
      @param name the item name
      @param of_value extracts the nested value from the parent value
      @param of_patch extracts the nested change from the parent patch
      @param build_value_children builds view list from nested value and change type
      @param build_patch_children builds view list from nested patch
      @param domain_type the domain type for this item
      @return Some item if there are children, None otherwise
  *)
  let build_item_from_children_with_change
      (type parent pp nested_actual np)
      (c : (parent, pp) structured_change)
      ~(name : string)
      ~(of_value : parent -> nested_actual option)
      ~(of_patch : pp -> (nested_actual, np) structured_change)
      ~(build_value_children : change_type -> nested_actual -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : item option =
    match c with
    | `Added parent ->
      (match of_value parent with
       | None -> None
       | Some nested_val ->
         let children = build_value_children Added nested_val in
         if children = [] then None
         else Some { name; change = Added; domain_type; children })
    | `Removed parent ->
      (match of_value parent with
       | None -> None
       | Some nested_val ->
         let children = build_value_children Removed nested_val in
         if children = [] then None
         else Some { name; change = Removed; domain_type; children })
    | `Modified patch ->
      (match of_patch patch with
       | `Unchanged -> None
       | `Added nested_val ->
         let children = build_value_children Added nested_val in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children }
       | `Removed nested_val ->
         let children = build_value_children Removed nested_val in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children }
       | `Modified np ->
         let children = build_patch_children np in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children })
    | `Unchanged -> None


  (** [build_collection c ~name ~of_value ~of_patch ~build_item]
      builds a [new_collection] for a list field containing structured items.

      This is the new equivalent of [build_collection_view], but:
      - Returns [new_collection] instead of [collection_view]
      - The [build_item] function should return a [item] (which gets wrapped in [Item])
      - Items in the collection can have full structure (not simplified [element_view])

      @param c the parent structured change
      @param name the collection name
      @param of_value extracts the item list from the parent value
      @param of_patch extracts the change list from the parent patch
      @param build_item builds a item from an item change
      @param domain_type the domain type for this collection
      @return Some new_collection if there are items, None otherwise
  *)
  let build_collection
      (c : ('parent, 'pp) structured_change)
      ~(name : string)
      ~(of_value : 'parent -> 'item list)
      ~(of_patch : 'pp -> ('item, 'ip) structured_change list)
      ~(build_item : ('item, 'ip) structured_change -> item)
      ~(domain_type : domain_type)
    : collection option =
    let change_type = change_type_of c in
    let items = match c with
      | `Added parent ->
        parent |> of_value |> List.map (fun item -> Item (build_item (`Added item)))
      | `Removed parent ->
        parent |> of_value |> List.map (fun item -> Item (build_item (`Removed item)))
      | `Modified patch ->
        patch |> of_patch |> List.map (fun item_change -> Item (build_item item_change))
      | `Unchanged -> []
    in
    (* Filter out Unchanged items and placeholder items (for unchanged items where we don't have values) *)
    let items = List.filter (fun (i : view) ->
        match i with
        | Item item -> item.change <> Unchanged && item.name <> ""
        | Collection col -> col.change <> Unchanged
        | Field _ -> true
      ) items in
    if items = [] then None
    else Some { name; change = change_type; domain_type; items }

end


(** [atomic_update_to_field_view] converts an [atomic_update] to a [field option].
    Returns [None] for unchanged fields.
    @param name the field name
    @param wrapper function to convert value to [field_value]
    @param domain_type the domain type for this field
    @param update the atomic update
*)
let atomic_update_to_field_view
    ~(name : string)
    ~(wrapper : 'a -> field_value)
    ~(domain_type : domain_type)
    (update : 'a atomic_update)
  : field option =
  match update with
  | `Unchanged -> None
  | `Modified (_patch : 'a atomic_patch) ->
    Some {
      name;
      change = Modified;
      domain_type;
      oldval = Some (wrapper _patch.oldval);
      newval = Some (wrapper _patch.newval)
    }


(** [structured_update_to_field_views] flattens a [structured_update] into multiple [field_view] items.
    @param build_fields function that takes the patch and returns a list of field options
    @param update the structured update
*)
let structured_update_to_field_views
    ~(build_fields : 'p -> field option list)
    (update : 'p structured_update)
  : field list =
  match update with
  | `Unchanged -> []
  | `Modified patch ->
    patch |> build_fields |> List.filter_map Fun.id


(* ==================== Unified Field Spec System ==================== *)

(** A unified field specification that can generate field views for both
    Added/Removed (from value) and Modified (from patch) cases.
    This eliminates the need for paired create_X_fields / create_X_patch_fields functions.
*)
type ('value, 'patch) unified_field_spec = {
  name : string;
  get_value : 'value -> field_value;                          (** Extract field value from parent *)
  get_patch : 'patch -> field_value atomic_update;            (** Extract field update from patch *)
}


(** [build_value_field_views specs change_type value] builds field views from a value.
    Used for Added/Removed cases.
    @param specs the list of unified field specs
    @param change_type the type of change (Added or Removed)
    @param value the parent value
    @param domain_type the domain type for these fields
*)
let build_value_field_views
    (specs : ('v, 'p) unified_field_spec list)
    (change_type : change_type)
    (value : 'v)
    ~(domain_type : domain_type)
  : view list =
  specs |> List.map (fun spec ->
      (Field {
          name = spec.name;
          change = change_type;
          domain_type;
          oldval = (if change_type = Removed then Some (spec.get_value value) else None);
          newval = (if change_type = Added then Some (spec.get_value value) else None);
        } : view))


(** [build_patch_field_views specs patch] builds field views from a patch.
    Used for Modified cases. Only returns fields that have actually changed.
    @param specs the list of unified field specs
    @param patch the parent patch
    @param domain_type the domain type for these fields
*)
let build_patch_field_views
    (specs : ('v, 'p) unified_field_spec list)
    (patch : 'p)
    ~(domain_type : domain_type)
  : view list =
  specs
  |> List.filter_map (fun spec ->
      let update = spec.get_patch patch in
      match update with
      | `Unchanged -> None
      | `Modified { oldval; newval } ->
        Some ((Field {
            name = spec.name;
            change = Modified;
            domain_type;
            oldval = Some oldval;
            newval = Some newval;
          } : view))
    )


(** Loop field specifications *)
let loop_field_specs : (Clip.Loop.t, Clip.Loop.Patch.t) unified_field_spec list = [
  { name = "Start Time";
    get_value = (fun l -> float_value l.start_time);
    get_patch = (fun p -> ViewBuilder.map_atomic_update float_value p.start_time) };
  { name = "End Time";
    get_value = (fun l -> float_value l.end_time);
    get_patch = (fun p -> ViewBuilder.map_atomic_update float_value p.end_time) };
  { name = "On";
    get_value = (fun l -> bool_value l.on);
    get_patch = (fun p -> ViewBuilder.map_atomic_update bool_value p.on) };
]

let create_loop_fields = build_value_field_views loop_field_specs ~domain_type:DTLoop
let create_loop_patch_fields = build_patch_field_views loop_field_specs ~domain_type:DTLoop


(** TimeSignature field specifications *)
let signature_field_specs : (Clip.TimeSignature.t, Clip.TimeSignature.Patch.t) unified_field_spec list = [
  { name = "Numerator";
    get_value = (fun s -> int_value s.numer);
    get_patch = (fun p -> ViewBuilder.map_atomic_update int_value p.numer) };
  { name = "Denominator";
    get_value = (fun s -> int_value s.denom);
    get_patch = (fun p -> ViewBuilder.map_atomic_update int_value p.denom) };
]

let create_signature_fields = build_value_field_views signature_field_specs ~domain_type:DTSignature
let create_signature_patch_fields = build_patch_field_views signature_field_specs ~domain_type:DTSignature

(* Default note name style for MIDI notes *)
let default_note_name_style = Sharp

(** [create_note_item] builds a [item] for a single note change (new type system).
    @param note_name_style the style to use for note names (Sharp or Flat)
    @param c the note structured change
*)
let create_note_item
    ?(note_name_style : note_display_style = default_note_name_style)
    (c : (Clip.MidiNote.t, Clip.MidiNote.Patch.t) structured_change)
  : item =
  let open Clip.MidiNote in
  let field_descs = [
    FieldDesc {
      name = "Time";
      of_parent_value = (fun x -> x.time);
      of_parent_patch = (fun x -> x.Patch.time);
      wrapper = float_value;
    };
    FieldDesc {
      name = "Duration";
      of_parent_value = (fun x -> x.duration);
      of_parent_patch = (fun x -> x.Patch.duration);
      wrapper = float_value;
    };
    FieldDesc {
      name = "Velocity";
      of_parent_value = (fun x -> x.velocity);
      of_parent_patch = (fun x -> x.Patch.velocity);
      wrapper = float_value;
    };
    FieldDesc {
      name = "Note";
      of_parent_value = (fun x -> x.note);
      of_parent_patch = (fun x -> x.Patch.note);
      wrapper = int_value;
    };
    FieldDesc {
      name = "Off Velocity";
      of_parent_value = (fun x -> x.off_velocity);
      of_parent_patch = (fun x -> x.Patch.off_velocity);
      wrapper = float_value;
    };
  ]
  in
  let note_name = match c with
    | `Added n ->
      let name = get_note_name_from_int ~style:note_name_style n.note in
      Printf.sprintf "Note %s (%d)" name n.note
    | `Removed n ->
      let name = get_note_name_from_int ~style:note_name_style n.note in
      Printf.sprintf "Note %s (%d)" name n.note
    | `Modified _ -> "Note"
    | `Unchanged -> "Note"
  in
  ViewBuilder.build_item_from_fields c ~name:note_name ~domain_type:DTNote ~field_descs


(** [create_midi_clip_item] creates a [item] from a MidiClip structured change (new type system).
    @param c the midi clip structured change
*)
let create_midi_clip_item
    (c : (Clip.MidiClip.t, Clip.MidiClip.Patch.t) structured_change)
  : item =

  let change_type = ViewBuilder.change_type_of c in

  let section_name = match c with
    | `Added clip -> Printf.sprintf "MidiClip (#%d): %s" clip.Clip.MidiClip.id clip.Clip.MidiClip.name
    | `Removed clip -> Printf.sprintf "MidiClip (#%d): %s" clip.Clip.MidiClip.id clip.Clip.MidiClip.name
    | `Modified patch ->
      (match patch.name with
       | `Modified { newval; _ } -> Printf.sprintf "MidiClip (#%d): %s" patch.Clip.MidiClip.Patch.id newval
       | `Unchanged -> Printf.sprintf "MidiClip (#%d)" patch.Clip.MidiClip.Patch.id)
    | `Unchanged -> "MidiClip"
  in

  (* Field descriptors for atomic fields *)
  let name_field_desc = FieldDesc {
      name = "Name";
      of_parent_value = (fun (x : Clip.MidiClip.t) -> x.Clip.MidiClip.name);
      of_parent_patch = (fun (p : Clip.MidiClip.Patch.t) -> p.name);
      wrapper = string_value;
    } in

  let start_time_field_desc = FieldDesc {
      name = "Start Time";
      of_parent_value = (fun (x : Clip.MidiClip.t) -> x.Clip.MidiClip.start_time);
      of_parent_patch = (fun (p : Clip.MidiClip.Patch.t) -> p.start_time);
      wrapper = float_value;
    } in

  let end_time_field_desc = FieldDesc {
      name = "End Time";
      of_parent_value = (fun (x : Clip.MidiClip.t) -> x.Clip.MidiClip.end_time);
      of_parent_patch = (fun (p : Clip.MidiClip.Patch.t) -> p.end_time);
      wrapper = float_value;
    } in

  (* Build atomic field views as new_view children *)
  let atomic_children : view list =
    let name_f = Field (ViewBuilder.build_field c name_field_desc ~domain_type:DTClip) in
    let start_f = Field (ViewBuilder.build_field c start_time_field_desc ~domain_type:DTClip) in
    let end_f = Field (ViewBuilder.build_field c end_time_field_desc ~domain_type:DTClip) in
    [name_f; start_f; end_f]
    |> List.filter (function
        | Field fv -> fv.change <> Unchanged
        | _ -> true)
  in

  (* Build Loop item using the new combinator *)
  let loop_item = ViewBuilder.build_item_from_children c
      ~name:"Loop"
      ~of_value:(fun (clip : Clip.MidiClip.t) -> clip.loop)
      ~of_patch:(fun (patch : Clip.MidiClip.Patch.t) -> patch.loop)
      ~build_value_children:(fun ct nested ->
          create_loop_fields ct nested )
      ~build_patch_children:(fun np ->
          create_loop_patch_fields np )
      ~domain_type:DTLoop
  in

  (* Build TimeSignature item using the new combinator *)
  let signature_item = ViewBuilder.build_item_from_children c
      ~name:"TimeSignature"
      ~of_value:(fun (clip : Clip.MidiClip.t) -> clip.signature)
      ~of_patch:(fun (patch : Clip.MidiClip.Patch.t) -> patch.signature)
      ~build_value_children:(fun ct nested ->
          create_signature_fields ct nested )
      ~build_patch_children:(fun np ->
          create_signature_patch_fields np )
      ~domain_type:DTSignature
  in

  (* Build Notes collection using the new combinator *)
  let notes_collection = ViewBuilder.build_collection c
      ~name:"Notes"
      ~of_value:(fun (clip : Clip.MidiClip.t) -> clip.notes)
      ~of_patch:(fun (patch : Clip.MidiClip.Patch.t) -> patch.notes)
      ~build_item:create_note_item
      ~domain_type:DTNote
  in

  (* Build children list *)
  let children =
    atomic_children
    @ (loop_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (signature_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (notes_collection |> Option.map (fun c -> Collection c) |> option_to_list)
  in

  { name = section_name; change = change_type; domain_type = DTClip; children }


(** SampleRef field specifications *)
let sample_ref_field_specs : (Clip.SampleRef.t, Clip.SampleRef.Patch.t) unified_field_spec list = [
  { name = "File Path";
    get_value = (fun sr -> string_value sr.file_path);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.file_path) };
  { name = "CRC";
    get_value = (fun sr -> string_value sr.crc);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.crc) };
  { name = "Last Modified";
    get_value = (fun sr -> string_value (format_unix_timestamp sr.last_modified_date));
    get_patch = (fun p -> ViewBuilder.map_atomic_update (fun x -> string_value (format_unix_timestamp x)) p.last_modified_date) };
]

let create_sample_ref_fields = build_value_field_views sample_ref_field_specs ~domain_type:DTSampleRef
let create_sample_ref_patch_fields = build_patch_field_views sample_ref_field_specs ~domain_type:DTSampleRef


(** [create_audio_clip_item] creates a [item] from an AudioClip structured change (new type system).
    @param c the audio clip structured change
*)
let create_audio_clip_item
    (c : (Clip.AudioClip.t, Clip.AudioClip.Patch.t) structured_change)
  : item =

  let change_type = ViewBuilder.change_type_of c in

  let section_name = match c with
    | `Added clip -> Printf.sprintf "AudioClip (#%d): %s" clip.Clip.AudioClip.id clip.Clip.AudioClip.name
    | `Removed clip -> Printf.sprintf "AudioClip (#%d): %s" clip.Clip.AudioClip.id clip.Clip.AudioClip.name
    | `Modified patch ->
      (match patch.name with
       | `Modified { newval; _ } -> Printf.sprintf "AudioClip (#%d): %s" patch.Clip.AudioClip.Patch.id newval
       | `Unchanged -> Printf.sprintf "AudioClip (#%d)" patch.Clip.AudioClip.Patch.id)
    | `Unchanged -> "AudioClip"
  in

  (* Field descriptors for atomic fields *)
  let name_field_desc = FieldDesc {
      name = "Name";
      of_parent_value = (fun (x : Clip.AudioClip.t) -> x.Clip.AudioClip.name);
      of_parent_patch = (fun (p : Clip.AudioClip.Patch.t) -> p.name);
      wrapper = string_value;
    } in

  let start_time_field_desc = FieldDesc {
      name = "Start Time";
      of_parent_value = (fun (x : Clip.AudioClip.t) -> x.Clip.AudioClip.start_time);
      of_parent_patch = (fun (p : Clip.AudioClip.Patch.t) -> p.start_time);
      wrapper = float_value;
    } in

  let end_time_field_desc = FieldDesc {
      name = "End Time";
      of_parent_value = (fun (x : Clip.AudioClip.t) -> x.Clip.AudioClip.end_time);
      of_parent_patch = (fun (p : Clip.AudioClip.Patch.t) -> p.end_time);
      wrapper = float_value;
    } in

  (* Build atomic field views as new_view children *)
  let atomic_children : view list =
    let name_f = Field (ViewBuilder.build_field c name_field_desc ~domain_type:DTClip) in
    let start_f = Field (ViewBuilder.build_field c start_time_field_desc ~domain_type:DTClip) in
    let end_f = Field (ViewBuilder.build_field c end_time_field_desc ~domain_type:DTClip) in
    [name_f; start_f; end_f]
    |> List.filter (function
        | Field fv -> fv.change <> Unchanged
        | _ -> true)
  in

  (* Build Loop item using the new combinator *)
  let loop_item = ViewBuilder.build_item_from_children c
      ~name:"Loop"
      ~of_value:(fun (clip : Clip.AudioClip.t) -> clip.loop)
      ~of_patch:(fun (patch : Clip.AudioClip.Patch.t) -> patch.loop)
      ~build_value_children:(fun ct nested ->
          create_loop_fields ct nested )
      ~build_patch_children:(fun np ->
          create_loop_patch_fields np )
      ~domain_type:DTLoop
  in

  (* Build TimeSignature item using the new combinator *)
  let signature_item = ViewBuilder.build_item_from_children c
      ~name:"TimeSignature"
      ~of_value:(fun (clip : Clip.AudioClip.t) -> clip.signature)
      ~of_patch:(fun (patch : Clip.AudioClip.Patch.t) -> patch.signature)
      ~build_value_children:(fun ct nested ->
          create_signature_fields ct nested )
      ~build_patch_children:(fun np ->
          create_signature_patch_fields np )
      ~domain_type:DTSignature
  in

  (* Build SampleRef item using the new combinator *)
  let sample_ref_item = ViewBuilder.build_item_from_children c
      ~name:"SampleRef"
      ~of_value:(fun (clip : Clip.AudioClip.t) -> clip.sample_ref)
      ~of_patch:(fun (patch : Clip.AudioClip.Patch.t) -> patch.sample_ref)
      ~build_value_children:(fun ct nested ->
          create_sample_ref_fields ct nested )
      ~build_patch_children:(fun np ->
          create_sample_ref_patch_fields np )
      ~domain_type:DTSampleRef
  in

  (* Build children list *)
  let children =
    atomic_children
    @ (loop_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (signature_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (sample_ref_item |> Option.map (fun i -> Item i) |> option_to_list)
  in

  { name = section_name; change = change_type; domain_type = DTClip; children }


(** [create_events_item] builds a [item] for an envelope event change (new type system).
    @param c the envelope event structured change
*)
let create_events_item
    (c : (Automation.EnvelopeEvent.t, Automation.EnvelopeEvent.Patch.t) structured_change)
  : item =
  let open Automation in
  let field_descs = [
    FieldDesc {
      name = "Time";
      of_parent_value = (fun x -> x.EnvelopeEvent.time);
      of_parent_patch = (fun x -> x.EnvelopeEvent.Patch.time);
      wrapper = float_value;
    };
    FieldDesc {
      name = "Value";
      of_parent_value = (fun x -> x.EnvelopeEvent.value);
      of_parent_patch = (fun x -> x.EnvelopeEvent.Patch.value);
      wrapper = float_value;
    }
  ]
  in
  ViewBuilder.build_item_from_fields c ~name:"EnvelopeEvent" ~domain_type:DTEvent ~field_descs


(* ================== Device View Functions ==================== *)

(** [param_value_atomic_to_field_value] converts a param_value atomic_update to field_value atomic_update *)
let param_value_atomic_to_field_value (update : Device.param_value atomic_update) : field_value atomic_update =
  match update with
  | `Modified { oldval; newval } ->
    let convert = function
      | Device.Float f -> Ffloat f
      | Device.Int i -> Fint i
      | Device.Bool b -> Fbool b
      | Device.Enum (e, _) -> Fint e
    in
    `Modified { oldval = convert oldval; newval = convert newval }
  | `Unchanged -> `Unchanged


(** [param_value_to_field_value] converts a Device.param_value to a field_value *)
let param_value_to_field_value (v : Device.param_value) : field_value =
  match v with
  | Float f -> Ffloat f
  | Int i -> Fint i
  | Bool b -> Fbool b
  | Enum (e, _) -> Fint e


(** [make_generic_param_field_descs] creates field descriptors for types that wrap GenericParam.
    This function generates the standard Name, Value, Automation, and Modulation field descriptors
    for any type that has a GenericParam as a base field.

    @param get_base extracts the GenericParam from the parent value type
    @param get_base_patch extracts the GenericParam.Patch update from the parent patch type
    @param include_name whether to include the Name field (Macro doesn't use it)
    @return a list of field descriptors
*)
let make_generic_param_field_descs
    (type t patch)
    ~(get_base : t -> Device.GenericParam.t)
    ~(get_base_patch : patch -> Device.GenericParam.Patch.t structured_update)
    ~(include_name : bool)
  : (t, patch) field_descriptor list =
  let module GP = Device.GenericParam in
  let name_desc =
    if include_name then
      [FieldDesc {
          name = "Name";
          of_parent_value = (fun x -> (get_base x).name);
          of_parent_patch = (fun p ->
              match get_base_patch p with
              | `Modified gpp -> gpp.GP.Patch.name
              | `Unchanged -> `Unchanged);
          wrapper = string_value;
        }]
    else []
  in
  let value_desc = FieldDesc {
      name = "Value";
      of_parent_value = (fun x -> param_value_to_field_value (get_base x).value);
      of_parent_patch = (fun p ->
          match get_base_patch p with
          | `Modified gpp -> param_value_atomic_to_field_value gpp.GP.Patch.value
          | `Unchanged -> `Unchanged);
      wrapper = Fun.id;
    } in
  let automation_desc = FieldDesc {
      name = "Automation";
      of_parent_value = (fun x -> (get_base x).automation);
      of_parent_patch = (fun p ->
          match get_base_patch p with
          | `Modified gpp -> gpp.GP.Patch.automation
          | `Unchanged -> `Unchanged);
      wrapper = int_value;
    } in
  let modulation_desc = FieldDesc {
      name = "Modulation";
      of_parent_value = (fun x -> (get_base x).modulation);
      of_parent_patch = (fun p ->
          match get_base_patch p with
          | `Modified gpp -> gpp.GP.Patch.modulation
          | `Unchanged -> `Unchanged);
      wrapper = int_value;
    } in
  name_desc @ [value_desc; automation_desc; modulation_desc]


(** [get_param_name_from_change] extracts the parameter name from a structured change
    for types that wrap GenericParam.
    @param get_base extracts the GenericParam from the parent value type
    @param default_name the fallback name when the specific name cannot be extracted
    @param c the structured change
*)
let get_param_name_from_change
    (type t patch)
    ~(get_base : t -> Device.GenericParam.t)
    ~(default_name : string)
    (c : (t, patch) structured_change)
  : string =
  match c with
  | `Added v -> (get_base v).Device.GenericParam.name
  | `Removed v -> (get_base v).Device.GenericParam.name
  | `Modified _ -> default_name
  | `Unchanged -> default_name


(** [create_device_param_item] builds a [item] for a device parameter change (new type system). *)
let create_device_param_item
    (c : (Device.DeviceParam.t, Device.DeviceParam.Patch.t) structured_change)
  : item =
  let get_base (x : Device.DeviceParam.t) = x.base in
  let get_base_patch (p : Device.DeviceParam.Patch.t) = p.base in
  let field_descs = make_generic_param_field_descs
      ~get_base ~get_base_patch ~include_name:true
  in
  let param_name = get_param_name_from_change ~get_base ~default_name:"Parameter" c in
  ViewBuilder.build_item_from_fields c ~name:param_name ~domain_type:DTParam ~field_descs


(** PresetRef field specifications *)
let preset_ref_field_specs : (Device.PresetRef.t, Device.PresetRef.Patch.t) unified_field_spec list = [
  { name = "Name";
    get_value = (fun pr -> string_value pr.name);
    get_patch = (fun _ -> `Unchanged) };  (* Name doesn't change in patch *)
  { name = "Relative Path";
    get_value = (fun pr -> string_value pr.relative_path);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.relative_path) };
  { name = "Pack Name";
    get_value = (fun pr -> string_value pr.pack_name);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.pack_name) };
]

let create_preset_ref_fields = build_value_field_views preset_ref_field_specs ~domain_type:DTPreset
let create_preset_ref_patch_fields = build_patch_field_views preset_ref_field_specs ~domain_type:DTPreset


(** PatchRef field specifications *)
let patch_ref_field_specs : (Device.PatchRef.t, Device.PatchRef.Patch.t) unified_field_spec list = [
  { name = "Name";
    get_value = (fun pr -> string_value pr.name);
    get_patch = (fun _ -> `Unchanged) };  (* Name doesn't change in patch *)
  { name = "Relative Path";
    get_value = (fun pr -> string_value pr.relative_path);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.relative_path) };
  { name = "Pack Name";
    get_value = (fun pr -> string_value pr.pack_name);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.pack_name) };
  { name = "Last Modified";
    get_value = (fun _ -> Fstring "");  (* Not available in PatchRef.t *)
    get_patch = (fun p -> ViewBuilder.map_atomic_update (fun x -> string_value (format_unix_timestamp x)) p.last_mod_date) };
]

let create_patch_ref_fields = build_value_field_views patch_ref_field_specs ~domain_type:DTPreset
let create_patch_ref_patch_fields = build_patch_field_views patch_ref_field_specs ~domain_type:DTPreset


(** [create_plugin_param_item] builds a [item] for a plugin parameter change (new type system). *)
let create_plugin_param_item
    (c : (Device.PluginParam.t, Device.PluginParam.Patch.t) structured_change)
  : item =
  let get_base (x : Device.PluginParam.t) = x.base in
  let get_base_patch (p : Device.PluginParam.Patch.t) = p.base in
  (* Index field is specific to PluginParam *)
  let index_field = FieldDesc {
      name = "Index";
      of_parent_value = (fun (x : Device.PluginParam.t) -> x.index);
      of_parent_patch = (fun (p : Device.PluginParam.Patch.t) -> p.index);
      wrapper = int_value;
    } in
  let base_field_descs = make_generic_param_field_descs
      ~get_base ~get_base_patch ~include_name:true
  in
  let field_descs = index_field :: base_field_descs in
  let param_name = get_param_name_from_change ~get_base ~default_name:"PluginParam" c in
  ViewBuilder.build_item_from_fields c ~name:param_name ~domain_type:DTParam ~field_descs


(** [create_m4l_param_item] builds a [item] for a Max4Live parameter change (new type system). *)
let create_m4l_param_item
    (c : (Device.Max4LiveParam.t, Device.Max4LiveParam.Patch.t) structured_change)
  : item =
  let get_base (x : Device.Max4LiveParam.t) = x.base in
  let get_base_patch (p : Device.Max4LiveParam.Patch.t) = p.base in
  (* Index field is specific to Max4LiveParam *)
  let index_field = FieldDesc {
      name = "Index";
      of_parent_value = (fun (x : Device.Max4LiveParam.t) -> x.index);
      of_parent_patch = (fun (p : Device.Max4LiveParam.Patch.t) -> p.index);
      wrapper = int_value;
    } in
  let base_field_descs = make_generic_param_field_descs
      ~get_base ~get_base_patch ~include_name:true
  in
  let field_descs = index_field :: base_field_descs in
  let param_name = get_param_name_from_change ~get_base ~default_name:"M4LParam" c in
  ViewBuilder.build_item_from_fields c ~name:param_name ~domain_type:DTParam ~field_descs


(** [create_macro_item] builds a [item] for a Macro change (new type system). *)
let create_macro_item
    (c : (Device.Macro.t, Device.Macro.Patch.t) structured_change)
  : item =
  let get_base (x : Device.Macro.t) = x.base in
  let get_base_patch (p : Device.Macro.Patch.t) = p.base in
  (* Macro doesn't display the Name field *)
  let field_descs = make_generic_param_field_descs
      ~get_base ~get_base_patch ~include_name:false
  in
  ViewBuilder.build_item_from_fields c ~name:"Macro" ~domain_type:DTMacro ~field_descs


(** [create_snapshot_item] builds a [item] for a Snapshot change (new type system). *)
let create_snapshot_item
    (c : (Device.Snapshot.t, Device.Snapshot.Patch.t) structured_change)
  : item =
  let open Device.Snapshot in
  let field_descs = [
    FieldDesc {
      name = "Name";
      of_parent_value = (fun x -> x.name);
      of_parent_patch = (fun x -> x.Patch.name);
      wrapper = string_value;
    };
  ]
  in
  let snapshot_name = match c with
    | `Added s -> s.name
    | `Removed s -> s.name
    | `Modified _ -> "Snapshot"
    | `Unchanged -> "Snapshot"
  in
  ViewBuilder.build_item_from_fields c ~name:snapshot_name ~domain_type:DTSnapshot ~field_descs


(* ==================== Device View Template Infrastructure ==================== *)

(** Configuration for building a device section *)

(** [build_device_section_name] generates the section name for a device.
    Format: "DeviceType: name" when available, just "DeviceType" otherwise.
*)
let build_device_section_name
    (type device patch)
    ~(device_type_name : string)
    ~(get_device_name : device -> string)
    ~(get_display_name_patch : patch -> string atomic_update)
    (c : (device, patch) structured_change)
  : string =
  match c with
  | `Added d -> device_type_name ^ ": " ^ get_device_name d
  | `Removed d -> device_type_name ^ ": " ^ get_device_name d
  | `Modified patch ->
    (match get_display_name_patch patch with
     | `Modified { newval; _ } -> device_type_name ^ ": " ^ newval
     | `Unchanged -> device_type_name)
  | `Unchanged -> device_type_name


(** [build_display_name_field] creates the display name field view for a device.
    Returns None if the display name is unchanged.
*)


(** [build_custom_sections] builds all custom sections for a device.
    Filters out None results and converts to view list.
*)


(** [create_collection_section_config] creates a config for a collection section.
    @param name Section name
    @param of_value Extract collection from device value
    @param of_patch Extract collection changes from patch
    @param build_element Element view builder
*)


(** [create_preset_section_config] creates the standard preset section config.
    This can be shared by all device types that use PresetRef.
*)


(** Configuration for building a device section - new type system *)
type ('device, 'patch) device_section_config = {
  name : string;
  build : ('device, 'patch) structured_change -> view option;
}


(** [create_preset_section_config] creates the standard preset section config (new type system).
    This is the new type system version of create_preset_section_config.
*)
let create_preset_section_config
    (type device patch)
    ~(of_value : device -> Device.PresetRef.t option)
    ~(of_patch : patch -> (Device.PresetRef.t, Device.PresetRef.Patch.t) structured_change)
    ~(domain_type : domain_type)
  : (device, patch) device_section_config =
  {
    name = "Preset";
    build = (fun c ->
        ViewBuilder.build_item_from_children_with_change c
          ~name:"Preset"
          ~of_value
          ~of_patch
          ~build_value_children:(fun ct nested ->
              create_preset_ref_fields ct nested )
          ~build_patch_children:(fun np ->
              create_preset_ref_patch_fields np )
          ~domain_type
        |> Option.map (fun i -> Item i)
      );
  }


(** [build_device_view] creates a device section view from a structured change.

    @param device_type_name The type name for section header (e.g., "RegularDevice")
    @param get_device_name Extract device_name from device value
    @param get_display_name Extract display_name from device value
    @param get_display_name_patch Extract display_name update from patch
    @param preset_config Optional preset section configuration
    @param custom_sections List of custom device-specific sections
    @param domain_type The domain type for this device
    @param c The device structured change
    @return A section_view for the device
*)


(* ==================== Device Item Template Infrastructure (New Type System) ==================== *)

(** [build_display_name_field] creates the display name field view for a device (new type system).
    Returns None if the display name is unchanged.
*)
let build_display_name_field
    (type device patch)
    ~(get_display_name : device -> string)
    ~(get_display_name_patch : patch -> string atomic_update)
    ~(domain_type : domain_type)
    (c : (device, patch) structured_change)
  : view option =
  let field_desc = FieldDesc {
      name = "Display Name";
      of_parent_value = get_display_name;
      of_parent_patch = get_display_name_patch;
      wrapper = string_value;
    } in
  let field = ViewBuilder.build_field c field_desc ~domain_type in
  if field.change = Unchanged then None
  else Some (Field field)

(** [build_custom_sections] builds all custom sections for a device (new type system).
    Filters out None results and converts to view list.
*)
let build_custom_sections
    (type device patch)
    (sections : (device, patch) device_section_config list)
    (c : (device, patch) structured_change)
  : view list =
  sections
  |> List.filter_map (fun config -> config.build c)

(** [create_collection_section_config] creates a config for a collection section (new type system).
    @param name Section name
    @param of_value Extract collection from device value
    @param of_patch Extract collection changes from patch
    @param build_item Item builder - returns item
*)
let create_collection_section_config
    (type device patch elem elem_patch)
    ~(name : string)
    ~(of_value : device -> elem list)
    ~(of_patch : patch -> (elem, elem_patch) structured_change list)
    ~(build_item : (elem, elem_patch) structured_change -> item)
    ~(domain_type : domain_type)
  : (device, patch) device_section_config =
  {
    name;
    build = (fun c ->
        ViewBuilder.build_collection c
          ~name
          ~of_value
          ~of_patch
          ~build_item
          ~domain_type
        |> Option.map (fun col -> Collection col)
      );
  }

(** [build_device_view] builds a device view as a item.
    This is the new type system version of build_device_view.
*)
let build_device_view
    (type device patch)
    ~(device_type_name : string)
    ~(get_device_name : device -> string)
    ~(get_display_name : device -> string)
    ~(get_display_name_patch : patch -> string atomic_update)
    ~(preset_config : (device, patch) device_section_config option)
    ~(custom_sections : (device, patch) device_section_config list)
    ~(domain_type : domain_type)
    (c : (device, patch) structured_change)
  : item =
  let change_type = ViewBuilder.change_type_of c in

  (* Build section name *)
  let section_name = build_device_section_name
      ~device_type_name
      ~get_device_name
      ~get_display_name_patch
      c
  in

  (* Build display name field *)
  let display_name_view = build_display_name_field
      ~get_display_name
      ~get_display_name_patch
      ~domain_type
      c
    |> option_to_list
  in

  (* Build preset section if configured *)
  let preset_view = match preset_config with
    | None -> []
    | Some config ->
      config.build c
      |> option_to_list
  in

  (* Build custom sections *)
  let custom_views = build_custom_sections custom_sections c in

  (* Combine all children *)
  let children = display_name_view @ preset_view @ custom_views in

  { name = section_name; change = change_type; domain_type; children }


(* ==================== Device View Functions ==================== *)

(** [create_regular_device_item] creates a [item] from a RegularDevice structured change (new type system). *)
let create_regular_device_item
    (c : (Device.RegularDevice.t, Device.RegularDevice.Patch.t) structured_change)
  : item =

  let params_config = create_collection_section_config
      ~name:"Parameters"
      ~of_value:(fun (d : Device.RegularDevice.t) -> d.params)
      ~of_patch:(fun (p : Device.RegularDevice.Patch.t) -> p.params)
      ~build_item:create_device_param_item
      ~domain_type:DTParam
  in

  build_device_view
    ~device_type_name:"RegularDevice"
    ~get_device_name:(fun (d : Device.RegularDevice.t) -> d.device_name)
    ~get_display_name:(fun (d : Device.RegularDevice.t) -> d.display_name)
    ~get_display_name_patch:(fun (p : Device.RegularDevice.Patch.t) -> p.display_name)
    ~preset_config:None
    ~custom_sections:[params_config]
    ~domain_type:DTDevice
    c


(** [create_plugin_device_item] creates a [item] from a PluginDevice structured change (new type system). *)
let create_plugin_device_item
    (c : (Device.PluginDevice.t, Device.PluginDevice.Patch.t) structured_change)
  : item =

  let params_config = create_collection_section_config
      ~name:"Parameters"
      ~of_value:(fun (d : Device.PluginDevice.t) -> d.params)
      ~of_patch:(fun (p : Device.PluginDevice.Patch.t) -> p.params)
      ~build_item:create_plugin_param_item
      ~domain_type:DTParam
  in

  (* Custom enabled section builder - for nested GenericParam handling *)
  let enabled_config : (Device.PluginDevice.t, Device.PluginDevice.Patch.t) device_section_config = {
    name = "Enabled";
    build = (fun c ->
        match c with
        | `Modified patch ->
          (match patch.enabled with
           | `Unchanged -> None
           | `Modified ep ->
             (match ep.base with
              | `Unchanged -> None
              | `Modified gpp ->
                let value_field = atomic_update_to_field_view
                    ~name:"Value"
                    ~wrapper:(fun v -> match v with
                        | Device.Float f -> Ffloat f
                        | Device.Int i -> Fint i
                        | Device.Bool b -> Fbool b
                        | Device.Enum (e, _) -> Fint e)
                    ~domain_type:DTParam
                    gpp.value
                in
                match value_field with
                | None -> None
                | Some fv ->
                  Some (Item {
                      name = "Enabled";
                      change = Modified;
                      domain_type = DTParam;
                      children = [Field fv]
                    })))
        | _ -> None);
  } in

  build_device_view
    ~device_type_name:"PluginDevice"
    ~get_device_name:(fun (d : Device.PluginDevice.t) -> d.device_name)
    ~get_display_name:(fun (d : Device.PluginDevice.t) -> d.display_name)
    ~get_display_name_patch:(fun (p : Device.PluginDevice.Patch.t) -> p.display_name)
    ~preset_config:(Some (create_preset_section_config
                            ~of_value:(fun (d : Device.PluginDevice.t) -> d.preset)
                            ~of_patch:(fun (p : Device.PluginDevice.Patch.t) -> p.preset)
                            ~domain_type:DTPreset))
    ~custom_sections:[enabled_config; params_config]
    ~domain_type:DTDevice
    c


(** [create_max4live_device_item] creates a [item] from a Max4LiveDevice structured change (new type system). *)
let create_max4live_device_item
    (c : (Device.Max4LiveDevice.t, Device.Max4LiveDevice.Patch.t) structured_change)
  : item =

  let params_config = create_collection_section_config
      ~name:"Parameters"
      ~of_value:(fun (d : Device.Max4LiveDevice.t) -> d.params)
      ~of_patch:(fun (p : Device.Max4LiveDevice.Patch.t) -> p.params)
      ~build_item:create_m4l_param_item
      ~domain_type:DTParam
  in

  (* Custom patch_ref section builder - manual handling because patch_ref uses structured_change *)
  let patch_ref_config : (Device.Max4LiveDevice.t, Device.Max4LiveDevice.Patch.t) device_section_config = {
    name = "PatchRef";
    build = (fun c ->
        match c with
        | `Added d ->
          let fields = create_patch_ref_fields Added d.patch_ref in
          if fields = [] then None
          else Some (Item {
              name = "PatchRef";
              change = Added;
              domain_type = DTPreset;
              children = fields
            })
        | `Removed d ->
          let fields = create_patch_ref_fields Removed d.patch_ref in
          if fields = [] then None
          else Some (Item {
              name = "PatchRef";
              change = Removed;
              domain_type = DTPreset;
              children = fields
            })
        | `Modified patch ->
          (match patch.patch_ref with
           | `Unchanged -> None
           | `Added pr ->
             let fields = create_patch_ref_fields Added pr in
             if fields = [] then None
             else Some (Item {
                 name = "PatchRef";
                 change = Modified;
                 domain_type = DTPreset;
                 children = fields
               })
           | `Removed pr ->
             let fields = create_patch_ref_fields Removed pr in
             if fields = [] then None
             else Some (Item {
                 name = "PatchRef";
                 change = Modified;
                 domain_type = DTPreset;
                 children = fields
               })
           | `Modified pr_patch ->
             let fields = create_patch_ref_patch_fields pr_patch in
             if fields = [] then None
             else Some (Item {
                 name = "PatchRef";
                 change = Modified;
                 domain_type = DTPreset;
                 children = fields
               }))
        | `Unchanged -> None);
  } in

  build_device_view
    ~device_type_name:"Max4LiveDevice"
    ~get_device_name:(fun (d : Device.Max4LiveDevice.t) -> d.device_name)
    ~get_display_name:(fun (d : Device.Max4LiveDevice.t) -> d.display_name)
    ~get_display_name_patch:(fun (p : Device.Max4LiveDevice.Patch.t) -> p.display_name)
    ~preset_config:(Some (create_preset_section_config
                            ~of_value:(fun (d : Device.Max4LiveDevice.t) -> d.preset)
                            ~of_patch:(fun (p : Device.Max4LiveDevice.Patch.t) -> p.preset)
                            ~domain_type:DTPreset))
    ~custom_sections:[patch_ref_config; params_config]
    ~domain_type:DTDevice
    c


(** [create_group_device_item] creates a [item] from a GroupDevice structured change (new type system). *)
let create_group_device_item
    (c : (Device.GroupDevice.t, Device.GroupDevice.Patch.t) structured_change)
  : item =

  let macros_config = create_collection_section_config
      ~name:"Macros"
      ~of_value:(fun (d : Device.GroupDevice.t) -> d.macros)
      ~of_patch:(fun (p : Device.GroupDevice.Patch.t) -> p.macros)
      ~build_item:create_macro_item
      ~domain_type:DTMacro
  in

  let snapshots_config = create_collection_section_config
      ~name:"Snapshots"
      ~of_value:(fun (d : Device.GroupDevice.t) -> d.snapshots)
      ~of_patch:(fun (p : Device.GroupDevice.Patch.t) -> p.snapshots)
      ~build_item:create_snapshot_item
      ~domain_type:DTSnapshot
  in

  build_device_view
    ~device_type_name:"GroupDevice"
    ~get_device_name:(fun (d : Device.GroupDevice.t) -> d.device_name)
    ~get_display_name:(fun (d : Device.GroupDevice.t) -> d.display_name)
    ~get_display_name_patch:(fun (p : Device.GroupDevice.Patch.t) -> p.display_name)
    ~preset_config:(Some (create_preset_section_config
                            ~of_value:(fun (d : Device.GroupDevice.t) -> d.preset)
                            ~of_patch:(fun (p : Device.GroupDevice.Patch.t) -> p.preset)
                            ~domain_type:DTPreset))
    ~custom_sections:[macros_config; snapshots_config]
    ~domain_type:DTDevice
    c


(* ==================== Track View Helpers ==================== *)

(** GenericParam field specifications using unified_field_spec system *)
let generic_param_field_specs : (Device.GenericParam.t, Device.GenericParam.Patch.t) unified_field_spec list = [
  { name = "Value";
    get_value = (fun p -> param_value_to_field_value p.value);
    get_patch = (fun p -> param_value_atomic_to_field_value p.value) };
  { name = "Automation";
    get_value = (fun p -> int_value p.automation);
    get_patch = (fun p -> ViewBuilder.map_atomic_update int_value p.automation) };
  { name = "Modulation";
    get_value = (fun p -> int_value p.modulation);
    get_patch = (fun p -> ViewBuilder.map_atomic_update int_value p.modulation) };
]

let create_generic_param_fields = build_value_field_views generic_param_field_specs ~domain_type:DTParam
let create_generic_param_patch_fields = build_patch_field_views generic_param_field_specs ~domain_type:DTParam


(** Routing field specifications *)
let routing_field_specs : (Track.Routing.t, Track.Routing.Patch.t) unified_field_spec list =
  let route_type_to_string = function
    | Track.Routing.MidiIn -> "MidiIn"
    | Track.Routing.MidiOut -> "MidiOut"
    | Track.Routing.AudioIn -> "AudioIn"
    | Track.Routing.AudioOut -> "AudioOut"
  in
  [
    { name = "Type";
      get_value = (fun r -> string_value (route_type_to_string r.route_type));
      get_patch = (fun p -> ViewBuilder.map_atomic_update (fun x -> string_value (route_type_to_string x)) p.route_type) };
    { name = "Target";
      get_value = (fun r -> string_value r.target);
      get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.target) };
  ]

let create_routing_fields = build_value_field_views routing_field_specs ~domain_type:DTRouting
let create_routing_patch_fields = build_patch_field_views routing_field_specs ~domain_type:DTRouting


(** [create_routing_set_fields] creates field views from a RoutingSet value.
    @param change_type the type of change (Added/Removed)
    @param routings the RoutingSet value
*)
let create_routing_set_fields (change_type : change_type) (routings : Track.RoutingSet.t) : view list =
  let open Track.RoutingSet in
  [
    Item {
      name = "Audio In";
      change = change_type;
      domain_type = DTRouting;
      children = create_routing_fields change_type routings.audio_in;
    };
    Item {
      name = "Audio Out";
      change = change_type;
      domain_type = DTRouting;
      children = create_routing_fields change_type routings.audio_out;
    };
    Item {
      name = "Midi In";
      change = change_type;
      domain_type = DTRouting;
      children = create_routing_fields change_type routings.midi_in;
    };
    Item {
      name = "Midi Out";
      change = change_type;
      domain_type = DTRouting;
      children = create_routing_fields change_type routings.midi_out;
    };
  ]


(** [create_routing_set_patch_fields] creates field views from a RoutingSet patch.
    @param patch the RoutingSet patch
*)
let create_routing_set_patch_fields (patch : Track.RoutingSet.Patch.t) : view list =
  let open Track.RoutingSet.Patch in
  let make_section name routing_update =
    match routing_update with
    | `Unchanged -> None
    | `Modified routing_patch ->
      let fields = create_routing_patch_fields routing_patch in
      if fields = [] then None
      else Some { name; change = Modified; domain_type = DTRouting; children = fields }
  in
  [
    make_section "Audio In" patch.audio_in;
    make_section "Audio Out" patch.audio_out;
    make_section "Midi In" patch.midi_in;
    make_section "Midi Out" patch.midi_out;
  ]
  |> List.filter_map Fun.id
  |> List.map (fun s -> Item s)


(* ==================== Mixer View Helpers ==================== *)

(** [build_mixer_value_fields ct m] builds section views for a Mixer value.
    Used for Added/Removed cases in track views.
    @param ct the change type (Added or Removed)
    @param m the Mixer value
*)
let build_mixer_value_fields (ct : change_type) (m : Track.Mixer.t) : view list =
  let open Track.Mixer in
  [
    Item { name = "Volume"; change = ct; domain_type = DTMixer; children = create_generic_param_fields ct m.volume };
    Item { name = "Pan"; change = ct; domain_type = DTMixer; children = create_generic_param_fields ct m.pan };
    Item { name = "Mute"; change = ct; domain_type = DTMixer; children = create_generic_param_fields ct m.mute };
    Item { name = "Solo"; change = ct; domain_type = DTMixer; children = create_generic_param_fields ct m.solo };
  ]


(** [build_mixer_patch_fields mp] builds section views for a Mixer patch.
    Used for Modified cases in track views.
    @param mp the Mixer patch
*)
let build_mixer_patch_fields (mp : Track.Mixer.Patch.t) : view list =
  let make_section name update =
    match update with
    | `Modified p ->
      let views = create_generic_param_patch_fields p in
      if views = [] then None
      else Some (Item { name; change = Modified; domain_type = DTMixer; children = views })
    | `Unchanged -> None
  in
  [
    make_section "Volume" mp.volume;
    make_section "Pan" mp.pan;
    make_section "Mute" mp.mute;
    make_section "Solo" mp.solo;
  ]
  |> List.filter_map Fun.id


(** [build_main_mixer_value_fields ct mm] builds section views for a MainMixer value.
    Used for Added/Removed cases in MainTrack view.
    @param ct the change type (Added or Removed)
    @param mm the MainMixer value
*)
let build_main_mixer_value_fields (ct : change_type) (mm : Track.MainMixer.t) : view list =
  let base_views = build_mixer_value_fields ct mm.base in
  let global_views = [
    Item { name = "Tempo"; change = ct; domain_type = DTMixer; children = create_generic_param_fields ct mm.tempo };
    Item { name = "Time Signature"; change = ct; domain_type = DTMixer; children = create_generic_param_fields ct mm.time_signature };
    Item { name = "Crossfade"; change = ct; domain_type = DTMixer; children = create_generic_param_fields ct mm.crossfade };
    Item { name = "Global Groove"; change = ct; domain_type = DTMixer; children = create_generic_param_fields ct mm.global_groove };
  ] in
  base_views @ global_views


(** [build_main_mixer_patch_fields mmp] builds section views for a MainMixer patch.
    Used for Modified cases in MainTrack view.
    @param mmp the MainMixer patch
*)
let build_main_mixer_patch_fields (mmp : Track.MainMixer.Patch.t) : view list =
  let base_views = match mmp.base with
    | `Modified base_mp -> build_mixer_patch_fields base_mp
    | `Unchanged -> []
  in
  let make_section name update =
    match update with
    | `Modified p ->
      let views = create_generic_param_patch_fields p in
      if views = [] then None
      else Some (Item { name; change = Modified; domain_type = DTMixer; children = views })
    | `Unchanged -> None
  in
  let global_views = [
    make_section "Tempo" mmp.tempo;
    make_section "Time Signature" mmp.time_signature;
    make_section "Crossfade" mmp.crossfade;
    make_section "Global Groove" mmp.global_groove;
  ] |> List.filter_map Fun.id in
  base_views @ global_views


(* ==================== Track Element Views ==================== *)

(** [create_send_item] builds a [item] for a send change (new type system).
    @param c the send structured change
*)
let create_send_item
    (c : (Track.Send.t, Track.Send.Patch.t) structured_change)
  : item =
  let open Track.Send in
  (* For immutable fields, return Unchanged in patch - they never change *)
  let field_descs = [
    FieldDesc {
      name = "Id";
      of_parent_value = (fun x -> x.id);
      of_parent_patch = (fun _ -> `Unchanged);
      wrapper = int_value;
    };
  ]
  in
  let send_name = match c with
    | `Added s -> Printf.sprintf "Send (to track %d)" s.id
    | `Removed s -> Printf.sprintf "Send (to track %d)" s.id
    | `Modified _ -> "Send"
    | `Unchanged -> "Send"
  in
  ViewBuilder.build_item_from_fields c ~name:send_name ~field_descs ~domain_type:DTSend


(** [create_automation_item] builds a [item] for an automation change (new type system).
    @param get_pointee_name function to resolve pointee IDs to names
    @param c the automation structured change
*)
let create_automation_item
    ~(get_pointee_name : int -> string)
    (c : (Automation.t, Automation.Patch.t) structured_change)
  : item =
  let open Automation in
  let change_type = ViewBuilder.change_type_of c in
  let automation_name = match c with
    | `Added a -> Printf.sprintf "Automation (id=%d, target=%s)" a.id (get_pointee_name a.target)
    | `Removed r -> Printf.sprintf "Automation (id=%d, target=%s)" r.id (get_pointee_name r.target)
    | `Modified patch -> Printf.sprintf "Automation (id=%d, target=%s)" patch.id (get_pointee_name patch.target)
    | `Unchanged -> "Automation"
  in

  (* Build event field views for Modified automation *)
  let event_children : view list = match c with
    | `Modified patch ->
      (* Build a map of event_id to event_change for Modified events (which don't have id directly) *)
      (* We'll use index since events are ordered by id *)
      patch.events |> List.mapi (fun i event_change ->
          let event_id = match event_change with
            | `Added e -> e.Automation.EnvelopeEvent.id
            | `Removed e -> e.Automation.EnvelopeEvent.id
            | `Modified _ -> i (* Use index as placeholder, will show as "Event[i]" *)
            | `Unchanged -> -1
          in
          let prefix = Printf.sprintf "Event[%d]" event_id in

          (match event_change with
           | `Added e ->
             Some (Field { name = prefix ^ " Added"; change = Added; domain_type = DTEvent; oldval = None;
                           newval = Some (Fstring (Printf.sprintf "Time=%.2f, Value=%.2f" e.Automation.EnvelopeEvent.time e.Automation.EnvelopeEvent.value)) })
           | `Removed e ->
             Some (Field { name = prefix ^ " Removed"; change = Removed; domain_type = DTEvent;
                           oldval = Some (Fstring (Printf.sprintf "Time=%.2f, Value=%.2f" e.Automation.EnvelopeEvent.time e.Automation.EnvelopeEvent.value));
                           newval = None })
           | `Modified ep ->
             (* Create fields for modified events showing time and/or value changes *)
             let time_field = match ep.Automation.EnvelopeEvent.Patch.time with
               | `Unchanged -> None
               | `Modified { oldval; newval } ->
                 Some (Field { name = prefix ^ " Time"; change = Modified; domain_type = DTEvent;
                               oldval = Some (Ffloat oldval); newval = Some (Ffloat newval) })
             in
             let value_field = match ep.Automation.EnvelopeEvent.Patch.value with
               | `Unchanged -> None
               | `Modified { oldval; newval } ->
                 Some (Field { name = prefix ^ " Value"; change = Modified; domain_type = DTEvent;
                               oldval = Some (Ffloat oldval); newval = Some (Ffloat newval) })
             in
             (* Combine both fields if they exist *)
             (match time_field, value_field with
              | None, None -> None
              | Some tf, None -> Some tf
              | None, Some vf -> Some vf
              | Some (Field tf), Some (Field vf) -> Some (Field { name = prefix; change = Modified; domain_type = DTEvent;
                                                                  oldval = Some (Fstring (Printf.sprintf "Time: %.2f->%.2f Value: %.2f->%.2f"
                                                                                            (match tf.oldval with Some (Ffloat f) -> f | _ -> 0.)
                                                                                            (match tf.newval with Some (Ffloat f) -> f | _ -> 0.)
                                                                                            (match vf.oldval with Some (Ffloat f) -> f | _ -> 0.)
                                                                                            (match vf.newval with Some (Ffloat f) -> f | _ -> 0.)));
                                                                  newval = None })
              | Some _, Some _ -> None (* Should not happen *)
             )
           | `Unchanged -> None)
        ) |> List.filter_map Fun.id
    | `Added _ | `Removed _ | `Unchanged -> []
  in

  { name = automation_name; change = change_type; domain_type = DTAutomation; children = event_children }


(** [create_device_item] builds a [item] for a device change (new type system).
    @param c the device structured change
*)
let create_device_item
    (c : (Device.t, Device.Patch.t) structured_change)
  : item =
  let get_device_id = function
    | Device.Regular d -> d.id
    | Device.Plugin d -> d.id
    | Device.Max4Live d -> d.id
    | Device.Group d -> d.id
  in
  let get_device_name = function
    | Device.Regular d -> d.device_name
    | Device.Plugin d -> d.device_name
    | Device.Max4Live d -> d.device_name
    | Device.Group d -> d.device_name
  in
  let get_display_name = function
    | Device.Regular d -> d.display_name
    | Device.Plugin d -> d.display_name
    | Device.Max4Live d -> d.display_name
    | Device.Group d -> d.display_name
  in
  (* Extract identity from patches *)
  let get_id_from_patch = function
    | Device.Patch.RegularPatch p -> p.id
    | Device.Patch.PluginPatch p -> p.id
    | Device.Patch.Max4LivePatch p -> p.id
    | Device.Patch.GroupPatch p -> p.id
  in
  let get_name_from_patch = function
    | Device.Patch.RegularPatch p -> p.device_name
    | Device.Patch.PluginPatch p -> p.device_name
    | Device.Patch.Max4LivePatch p -> p.device_name
    | Device.Patch.GroupPatch p -> p.device_name
  in
  let get_display_name_from_patch = function
    | Device.Patch.RegularPatch p ->
      (match p.display_name with
       | `Modified { newval; _ } -> newval
       | `Unchanged -> "")
    | Device.Patch.PluginPatch p ->
      (match p.display_name with
       | `Modified { newval; _ } -> newval
       | `Unchanged -> "")
    | Device.Patch.Max4LivePatch p ->
      (match p.display_name with
       | `Modified { newval; _ } -> newval
       | `Unchanged -> "")
    | Device.Patch.GroupPatch p ->
      (match p.display_name with
       | `Modified { newval; _ } -> newval
       | `Unchanged -> "")
  in

  (* Field descriptors exclude identity fields (no Id, Name) *)
  let field_descs = [] (* Empty - identity shown in name instead *)
  in


  (* Format device name with identity inline: "device_name (#id): display_name" *)
  let device_name = match c with
    | `Added d ->
      Printf.sprintf "%s (#%d): %s" (get_device_name d) (get_device_id d) (get_display_name d)
    | `Removed d ->
      Printf.sprintf "%s (#%d): %s" (get_device_name d) (get_device_id d) (get_display_name d)
    | `Modified patch ->
      let display = get_display_name_from_patch patch in
      let display_name = if display = "" then get_name_from_patch patch else display in
      Printf.sprintf "%s (#%d): %s" (get_name_from_patch patch) (get_id_from_patch patch) display_name
    | `Unchanged -> "Device"
  in

  ViewBuilder.build_item_from_fields c ~name:device_name ~field_descs ~domain_type:DTDevice


(* ==================== Track Component Section Views ==================== *)

(** [create_mixer_item] creates a [item] from a Mixer structured change (new type system).
    @param c the mixer structured change
*)
let create_mixer_item
    (c : (Track.Mixer.t, Track.Mixer.Patch.t) structured_change)
  : item option =
  let open Track.Mixer in

  let volume_item = ViewBuilder.build_item_from_children c
      ~name:"Volume"
      ~of_value:(fun m -> m.volume)
      ~of_patch:(fun p -> p.volume)
      ~build_value_children:(fun ct vol ->
          create_generic_param_fields ct vol )
      ~build_patch_children:(fun vol_patch ->
          create_generic_param_patch_fields vol_patch )
      ~domain_type:DTMixer
  in

  let pan_item = ViewBuilder.build_item_from_children c
      ~name:"Pan"
      ~of_value:(fun m -> m.pan)
      ~of_patch:(fun p -> p.pan)
      ~build_value_children:(fun ct vol ->
          create_generic_param_fields ct vol )
      ~build_patch_children:(fun vol_patch ->
          create_generic_param_patch_fields vol_patch )
      ~domain_type:DTMixer
  in

  let mute_item = ViewBuilder.build_item_from_children c
      ~name:"Mute"
      ~of_value:(fun m -> m.mute)
      ~of_patch:(fun p -> p.mute)
      ~build_value_children:(fun ct vol ->
          create_generic_param_fields ct vol )
      ~build_patch_children:(fun vol_patch ->
          create_generic_param_patch_fields vol_patch )
      ~domain_type:DTMixer
  in

  let solo_item = ViewBuilder.build_item_from_children c
      ~name:"Solo"
      ~of_value:(fun m -> m.solo)
      ~of_patch:(fun p -> p.solo)
      ~build_value_children:(fun ct vol ->
          create_generic_param_fields ct vol )
      ~build_patch_children:(fun vol_patch ->
          create_generic_param_patch_fields vol_patch )
      ~domain_type:DTMixer
  in

  let sends_collection = ViewBuilder.build_collection c
      ~name:"Sends"
      ~of_value:(fun m -> m.sends)
      ~of_patch:(fun p -> p.sends)
      ~build_item:create_send_item
      ~domain_type:DTSend
  in

  let children =
    (volume_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (pan_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (mute_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (solo_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (sends_collection |> Option.map (fun c -> Collection c) |> option_to_list)
  in

  if children = [] then None
  else Some { name = "Mixer"; change = ViewBuilder.change_type_of c; domain_type = DTMixer; children }


(** [create_main_mixer_item] creates a [item] from a MainMixer structured change (new type system).
    @param c the main mixer structured change
*)
let create_main_mixer_item
    (c : (Track.MainMixer.t, Track.MainMixer.Patch.t) structured_change)
  : item option =
  let open Track.MainMixer in

  (* Build base mixer nested item - simplified with key params *)
  let base_mixer_item = ViewBuilder.build_item_from_children c
      ~name:"Base Mixer"
      ~of_value:(fun mm -> mm.base.Track.Mixer.volume)
      ~of_patch:(fun p ->
          match p.base with
          | `Modified mp -> mp.Track.Mixer.Patch.volume
          | `Unchanged -> `Unchanged)
      ~build_value_children:(fun ct vol ->
          create_generic_param_fields ct vol )
      ~build_patch_children:(fun vol_patch ->
          create_generic_param_patch_fields vol_patch )
      ~domain_type:DTMixer
  in

  let tempo_item = ViewBuilder.build_item_from_children c
      ~name:"Tempo"
      ~of_value:(fun mm -> mm.tempo)
      ~of_patch:(fun p -> p.tempo)
      ~build_value_children:(fun ct vol ->
          create_generic_param_fields ct vol )
      ~build_patch_children:(fun vol_patch ->
          create_generic_param_patch_fields vol_patch )
      ~domain_type:DTMixer
  in

  let time_sig_item = ViewBuilder.build_item_from_children c
      ~name:"Time Signature"
      ~of_value:(fun mm -> mm.time_signature)
      ~of_patch:(fun p -> p.time_signature)
      ~build_value_children:(fun ct vol ->
          create_generic_param_fields ct vol )
      ~build_patch_children:(fun vol_patch ->
          create_generic_param_patch_fields vol_patch )
      ~domain_type:DTMixer
  in

  let crossfade_item = ViewBuilder.build_item_from_children c
      ~name:"Crossfade"
      ~of_value:(fun mm -> mm.crossfade)
      ~of_patch:(fun p -> p.crossfade)
      ~build_value_children:(fun ct vol ->
          create_generic_param_fields ct vol )
      ~build_patch_children:(fun vol_patch ->
          create_generic_param_patch_fields vol_patch )
      ~domain_type:DTMixer
  in

  let groove_item = ViewBuilder.build_item_from_children c
      ~name:"Global Groove"
      ~of_value:(fun mm -> mm.global_groove)
      ~of_patch:(fun p -> p.global_groove)
      ~build_value_children:(fun ct vol ->
          create_generic_param_fields ct vol )
      ~build_patch_children:(fun vol_patch ->
          create_generic_param_patch_fields vol_patch )
      ~domain_type:DTMixer
  in

  let children =
    (base_mixer_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (tempo_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (time_sig_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (crossfade_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (groove_item |> Option.map (fun i -> Item i) |> option_to_list)
  in

  if children = [] then None
  else Some { name = "Main Mixer"; change = ViewBuilder.change_type_of c; domain_type = DTMixer; children }


(* ==================== Track View Template Infrastructure ==================== *)

(** [build_track_section_name] generates the section name for a track.
    Format: "TrackType: name #id" when available, just "TrackType #id" otherwise.
*)
let build_track_section_name
    (type track patch)
    ~(track_type_name : string)
    ~(get_name : track -> string)
    ~(get_current_name_patch : patch -> string)
    ~(get_id : track -> int)
    ~(get_id_patch : patch -> int)
    (c : (track, patch) structured_change)
  : string =
  match c with
  | `Added t -> Printf.sprintf "%s (#%d): %s" track_type_name (get_id t) (get_name t)
  | `Removed t -> Printf.sprintf "%s (#%d): %s" track_type_name (get_id t) (get_name t)
  | `Modified patch -> Printf.sprintf "%s (#%d): %s" track_type_name (get_id_patch patch) (get_current_name_patch patch)
  | `Unchanged -> track_type_name


(** [build_name_field] creates the name field view for a track.
    Returns None if the name is unchanged.
*)


(** [create_track_collection_config] creates a config for a track collection section.
    @param name Section name
    @param of_value Extract collection from track value
    @param of_patch Extract collection changes from patch
    @param build_element Element view builder
*)


(** [create_track_mixer_config] creates a config for a mixer or routings section.
    @param name Section name
    @param of_value Extract nested value from track value
    @param of_patch Extract nested update from track patch
    @param build_value_fields Build views for Added/Removed cases
    @param build_patch_fields Build views for Modified case
*)


(** [build_track_view] creates a track section view from a structured change.

    @param track_type_name The type name for section header (e.g., "MidiTrack")
    @param get_name Extract name from track value
    @param get_name_patch Extract name update from patch
    @param clips_config Optional clips collection configuration
    @param mixer_config Mixer section configuration
    @param extra_sections List of additional sections (automations, devices, routings)
    @param c The track structured change
    @return A section_view for the track
*)


(* ==================== Track Item Template Infrastructure (New Type System) ==================== *)

(** [create_track_collection_config] creates a config for a track collection section (new type system).
    @param name Section name
    @param of_value Extract collection from track value
    @param of_patch Extract collection changes from patch
    @param build_item Item builder - returns item
*)
let create_track_collection_config
    (type track patch elem elem_patch)
    ~(name : string)
    ~(of_value : track -> elem list)
    ~(of_patch : patch -> (elem, elem_patch) structured_change list)
    ~(build_item : (elem, elem_patch) structured_change -> item)
    ~domain_type
  : (track, patch) device_section_config =
  {
    name;
    build = (fun c ->
        ViewBuilder.build_collection c
          ~name
          ~of_value
          ~of_patch
          ~build_item
          ~domain_type
        |> Option.map (fun col -> Collection col)
      );
  }


(** [create_track_mixer_config] creates a config for a mixer/routings section (new type system).
    @param name Section name
    @param of_value Extract nested value from track value
    @param of_patch Extract nested update from track patch
    @param build_value_children Build children for Added/Removed cases
    @param build_patch_children Build children for Modified case
*)
let create_track_mixer_config
    (type track patch nested_actual np)
    ~(name : string)
    ~(of_value : track -> nested_actual)
    ~(of_patch : patch -> np structured_update)
    ~(build_value_children : change_type -> nested_actual -> view list)
    ~(build_patch_children : np -> view list)
    ~domain_type
  : (track, patch) device_section_config =
  {
    name;
    build = (fun c ->
        ViewBuilder.build_item_from_children c
          ~name
          ~of_value
          ~of_patch
          ~build_value_children
          ~build_patch_children
          ~domain_type
        |> Option.map (fun i -> Item i)
      );
  }


(** [build_track_view] builds a track view as a item.
    This is the new type system version of build_track_view.

    @param track_type_name The type name for section header (e.g., "MidiTrack")
    @param get_name Extract name from track value
    @param get_name_patch Extract name update from patch
    @param get_current_name_patch Extract current name from patch
    @param get_id Extract id from track value
    @param get_id_patch Extract id update from patch
    @param clips_config Optional clips collection configuration
    @param mixer_config Mixer section configuration
    @param extra_sections List of additional sections (automations, devices, routings)
    @param c The track structured change
    @return A item for the track
*)
let build_track_view
    (type track patch)
    ~(track_type_name : string)
    ~(get_name : track -> string)
    ~(get_name_patch : patch -> string atomic_update)
    ~(get_current_name_patch : patch -> string)
    ~(get_id : track -> int)
    ~(get_id_patch : patch -> int)
    ?(clips_config : (track, patch) device_section_config option)
    ~(mixer_config : (track, patch) device_section_config)
    ~(extra_sections : (track, patch) device_section_config list)
    (c : (track, patch) structured_change)
  : item =
  let change_type = ViewBuilder.change_type_of c in

  (* Build section name *)
  let section_name = build_track_section_name
      ~track_type_name
      ~get_name
      ~get_current_name_patch
      ~get_id
      ~get_id_patch
      c
  in

  (* Build name field *)
  let name_field : view option =
    let field_desc = FieldDesc {
        name = "Name";
        of_parent_value = get_name;
        of_parent_patch = get_name_patch;
        wrapper = string_value;
      } in
    let field = ViewBuilder.build_field c field_desc ~domain_type:DTTrack in
    if field.change = Unchanged then None
    else Some (Field field)
  in

  (* Build clips collection if configured *)
  let clips_view : view list = match clips_config with
    | None -> []
    | Some config ->
      config.build c
      |> option_to_list
  in

  (* Build mixer section *)
  let mixer_view : view list = mixer_config.build c |> option_to_list in

  (* Build extra sections (automations, devices, routings) *)
  let extra_views : view list = build_custom_sections extra_sections c in

  (* Combine all children *)
  let children = (name_field |> option_to_list) @ clips_view @ mixer_view @ extra_views in

  { name = section_name; change = change_type; domain_type = DTTrack; children }


(* ==================== Full Track Views ==================== *)

(** [create_midi_track_item] creates a [item] from a MidiTrack structured change (new type system).
    @param get_pointee_name function to resolve pointee IDs to names
    @param c the MIDI track structured change
*)
let create_midi_track_item
    ~(get_pointee_name : int -> string)
    (c : (Track.MidiTrack.t, Track.MidiTrack.Patch.t) structured_change)
  : item =

  let clips_config = create_track_collection_config
      ~name:"Clips"
      ~of_value:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.clips)
      ~of_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.clips)
      ~build_item:create_midi_clip_item
      ~domain_type:DTClip
  in

  let mixer_config : (_, _) device_section_config = create_track_mixer_config
      ~name:"Mixer"
      ~of_value:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.mixer)
      ~of_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.mixer)
      ~build_value_children:(fun ct mix ->
          build_mixer_value_fields ct mix )
      ~build_patch_children:(fun mix_patch ->
          build_mixer_patch_fields mix_patch )
      ~domain_type:DTMixer
  in

  let automations_config = create_track_collection_config
      ~name:"Automations"
      ~of_value:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.automations)
      ~of_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.automations)
      ~build_item:(create_automation_item ~get_pointee_name)
      ~domain_type:DTAutomation
  in

  let devices_config = create_track_collection_config
      ~name:"Devices"
      ~of_value:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.devices)
      ~of_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.devices)
      ~build_item:create_device_item
      ~domain_type:DTDevice
  in

  let routings_config = create_track_mixer_config
      ~name:"Routings"
      ~of_value:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.routings)
      ~of_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.routings)
      ~build_value_children:(fun ct routes ->
          create_routing_set_fields ct routes )
      ~build_patch_children:(fun routes_patch ->
          create_routing_set_patch_fields routes_patch )
      ~domain_type:DTRouting
  in

  build_track_view
    ~track_type_name:"MidiTrack"
    ~get_name:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.name)
    ~get_name_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.name)
    ~get_current_name_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.current_name)
    ~get_id:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.id)
    ~get_id_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.id)
    ~clips_config
    ~mixer_config
    ~extra_sections:[automations_config; devices_config; routings_config]
    c

(** [create_audio_like_track_item] creates a [item] for AudioTrack-like structured changes.
    Shared implementation for AudioTrack and GroupTrack (which share the same internal structure).
    @param get_pointee_name function to resolve pointee IDs to names
    @param track_type_name The display type name (e.g., "AudioTrack" or "Group")
    @param c the track structured change
*)
let create_audio_like_track_item
    ~(get_pointee_name : int -> string)
    ~track_type_name
    (c : (Track.AudioTrack.t, Track.AudioTrack.Patch.t) structured_change)
  : item =

  let clips_config = create_track_collection_config
      ~name:"Clips"
      ~of_value:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.clips)
      ~of_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.clips)
      ~build_item:create_audio_clip_item
      ~domain_type:DTClip
  in

  let mixer_config = create_track_mixer_config
      ~name:"Mixer"
      ~of_value:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.mixer)
      ~of_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.mixer)
      ~build_value_children:(fun ct mix -> build_mixer_value_fields ct mix)
      ~build_patch_children:(fun mix_patch -> build_mixer_patch_fields mix_patch)
      ~domain_type:DTMixer
  in

  let automations_config = create_track_collection_config
      ~name:"Automations"
      ~of_value:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.automations)
      ~of_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.automations)
      ~build_item:(create_automation_item ~get_pointee_name)
      ~domain_type:DTAutomation
  in

  let devices_config = create_track_collection_config
      ~name:"Devices"
      ~of_value:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.devices)
      ~of_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.devices)
      ~build_item:create_device_item
      ~domain_type:DTDevice
  in

  let routings_config = create_track_mixer_config
      ~name:"Routings"
      ~of_value:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.routings)
      ~of_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.routings)
      ~build_value_children:(fun ct routes -> create_routing_set_fields ct routes)
      ~build_patch_children:(fun routes_patch -> create_routing_set_patch_fields routes_patch)
      ~domain_type:DTRouting
  in

  build_track_view
    ~track_type_name
    ~get_name:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.name)
    ~get_name_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.name)
    ~get_current_name_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.current_name)
    ~get_id:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.id)
    ~get_id_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.id)
    ~clips_config
    ~mixer_config
    ~extra_sections:[automations_config; devices_config; routings_config]
    c


(** [create_audio_track_item] creates a [item] from an AudioTrack structured change (new type system).
    @param get_pointee_name function to resolve pointee IDs to names
    @param c the audio track structured change
*)
let create_audio_track_item
    ~(get_pointee_name : int -> string)
    (c : (Track.AudioTrack.t, Track.AudioTrack.Patch.t) structured_change)
  : item =
  create_audio_like_track_item ~get_pointee_name ~track_type_name:"AudioTrack" c


(** [create_group_track_item] creates a [item] from a GroupTrack structured change (new type system).
    Group tracks use the same internal structure as AudioTrack but have a different type name.
    @param get_pointee_name function to resolve pointee IDs to names
    @param c the group track structured change (represented as AudioTrack internally)
*)
let create_group_track_item
    ~(get_pointee_name : int -> string)
    (c : (Track.AudioTrack.t, Track.AudioTrack.Patch.t) structured_change)
  : item =
  create_audio_like_track_item ~get_pointee_name ~track_type_name:"Group" c


(** [create_main_track_item] creates a [item] from a MainTrack structured change (new type system).
    @param get_pointee_name function to resolve pointee IDs to names
    @param c the main track structured change
*)
let create_main_track_item
    ~(get_pointee_name : int -> string)
    (c : (Track.MainTrack.t, Track.MainTrack.Patch.t) structured_change)
  : item =

  (* NO clips collection for MainTrack *)

  let mixer_config : (_, _) device_section_config = create_track_mixer_config
      ~name:"Main Mixer"
      ~of_value:(fun (t : Track.MainTrack.t) -> t.Track.MainTrack.mixer)
      ~of_patch:(fun (p : Track.MainTrack.Patch.t) -> p.mixer)
      ~build_value_children:(fun ct mix ->
          build_main_mixer_value_fields ct mix )
      ~build_patch_children:(fun mix_patch ->
          build_main_mixer_patch_fields mix_patch )
      ~domain_type:DTMixer
  in

  let automations_config = create_track_collection_config
      ~name:"Automations"
      ~of_value:(fun (t : Track.MainTrack.t) -> t.Track.MainTrack.automations)
      ~of_patch:(fun (p : Track.MainTrack.Patch.t) -> p.automations)
      ~build_item:(create_automation_item ~get_pointee_name)
      ~domain_type:DTAutomation
  in

  let devices_config = create_track_collection_config
      ~name:"Devices"
      ~of_value:(fun (t : Track.MainTrack.t) -> t.Track.MainTrack.devices)
      ~of_patch:(fun (p : Track.MainTrack.Patch.t) -> p.devices)
      ~build_item:create_device_item
      ~domain_type:DTDevice
  in

  let routings_config = create_track_mixer_config
      ~name:"Routings"
      ~of_value:(fun (t : Track.MainTrack.t) -> t.Track.MainTrack.routings)
      ~of_patch:(fun (p : Track.MainTrack.Patch.t) -> p.routings)
      ~build_value_children:(fun ct routes ->
          create_routing_set_fields ct routes )
      ~build_patch_children:(fun routes_patch ->
          create_routing_set_patch_fields routes_patch )
      ~domain_type:DTRouting
  in

  build_track_view
    ~track_type_name:"MainTrack"
    ~get_name:(fun (t : Track.MainTrack.t) -> t.Track.MainTrack.name)
    ~get_name_patch:(fun (p : Track.MainTrack.Patch.t) -> p.name)
    ~get_current_name_patch:(fun (p : Track.MainTrack.Patch.t) -> p.current_name)
    ~get_id:(fun (_ : Track.MainTrack.t) -> 0)
    ~get_id_patch:(fun (_ : Track.MainTrack.Patch.t) -> 0)
    ~mixer_config
    ~extra_sections:[automations_config; devices_config; routings_config]
    c


(* ==================== Liveset View ==================== *)

(** [create_locator_item] builds a [item] for a locator change (new type system).
    @param c the locator structured change
*)
let create_locator_item
    (c : (Liveset.Locator.t, Liveset.Locator.Patch.t) structured_change)
  : item =
  let field_descs = [
    FieldDesc {
      name = "Id";
      of_parent_value = (fun (x : Liveset.Locator.t) -> x.Liveset.Locator.id);
      of_parent_patch = (fun _ -> `Unchanged);
      wrapper = int_value;
    };
    FieldDesc {
      name = "Name";
      of_parent_value = (fun (x : Liveset.Locator.t) -> x.Liveset.Locator.name);
      of_parent_patch = (fun (p : Liveset.Locator.Patch.t) -> p.name);
      wrapper = string_value;
    };
    FieldDesc {
      name = "Time";
      of_parent_value = (fun (x : Liveset.Locator.t) -> x.Liveset.Locator.time);
      of_parent_patch = (fun (p : Liveset.Locator.Patch.t) -> p.time);
      wrapper = float_value;
    };
  ]
  in
  let locator_name = match c with
    | `Added l -> Printf.sprintf "Locator (id=%d)" l.Liveset.Locator.id
    | `Removed l -> Printf.sprintf "Locator (id=%d)" l.Liveset.Locator.id
    | `Modified _ -> "Locator"
    | `Unchanged -> "Locator"
  in
  ViewBuilder.build_item_from_fields c ~name:locator_name ~field_descs ~domain_type:DTLocator


(** Version field specifications *)
let version_field_specs : (Liveset.Version.t, Liveset.Version.Patch.t) unified_field_spec list = [
  { name = "Major";
    get_value = (fun v -> string_value v.major);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.major) };
  { name = "Minor";
    get_value = (fun v -> string_value v.minor);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.minor) };
  { name = "Revision";
    get_value = (fun v -> string_value v.revision);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.revision) };
]

let create_version_fields = build_value_field_views version_field_specs ~domain_type:DTVersion
let create_version_patch_fields = build_patch_field_views version_field_specs ~domain_type:DTVersion


(** [create_liveset_item] creates a [item] from a Liveset structured change (new type system).
    @param c the liveset structured change
*)
let create_liveset_item
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : item =

  let change_type = ViewBuilder.change_type_of c in

  (* Extract liveset for pointee name resolution *)
  let get_pointee_name = match c with
    | `Added ls -> (fun id -> Liveset.get_pointee_name_from_table ls.Liveset.pointees id)
    | `Removed ls -> (fun id -> Liveset.get_pointee_name_from_table ls.Liveset.pointees id)
    | `Modified patch ->
      (* Use pointees tables from the patch for name resolution *)
      (fun id ->
         match Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.new_pointees id with
         | Some name -> name
         | None ->
           match Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.old_pointees id with
           | Some name -> name
           | None -> Printf.sprintf "<Pointee %d>" id)
    | `Unchanged -> fun id -> Printf.sprintf "<Pointee %d>" id
  in

  (* Build section name from liveset name *)
  let section_name = match c with
    | `Added ls -> "LiveSet: " ^ ls.name
    | `Removed ls -> "LiveSet: " ^ ls.name
    | `Modified patch ->
      (match patch.name with
       | `Modified { newval; _ } -> "LiveSet: " ^ newval
       | `Unchanged -> "LiveSet")
    | `Unchanged -> "LiveSet"
  in

  (* Build atomic field views for name, version, creator *)
  let name_field_desc = FieldDesc {
      name = "Name";
      of_parent_value = (fun (ls : Liveset.t) -> ls.name);
      of_parent_patch = (fun (p : Liveset.Patch.t) -> p.name);
      wrapper = string_value;
    } in

  let creator_field_desc = FieldDesc {
      name = "Creator";
      of_parent_value = (fun (ls : Liveset.t) -> ls.creator);
      of_parent_patch = (fun (p : Liveset.Patch.t) -> p.creator);
      wrapper = string_value;
    } in

  let atomic_children : view list =
    [
      Field (ViewBuilder.build_field c name_field_desc ~domain_type:DTLiveset);
      Field (ViewBuilder.build_field c creator_field_desc ~domain_type:DTLiveset);
    ]
    |> List.filter (function
        | Field fv -> fv.change <> Unchanged
        | _ -> true)
  in

  (* Build Version section using the new combinator *)
  let version_item = ViewBuilder.build_item_from_children c
      ~name:"Version"
      ~of_value:(fun (ls : Liveset.t) -> ls.version)
      ~of_patch:(fun (p : Liveset.Patch.t) -> p.version)
      ~build_value_children:(fun ct ver ->
          create_version_fields ct ver )
      ~build_patch_children:(fun ver_patch ->
          create_version_patch_fields ver_patch )
      ~domain_type:DTVersion
  in

  (* Build Main Track section - special handling for singleton track *)
  let main_track_item = ViewBuilder.build_item_from_children_with_change c
      ~name:"Main Track"
      ~of_value:(fun (ls : Liveset.t) ->
          (* Extract MainTrack from Track.t *)
          match ls.Liveset.main with
          | Track.Main t -> Some t
          | _ -> None)
      ~of_patch:(fun (p : Liveset.Patch.t) ->
          (* Extract main track change from tracks - it's the one with Main/MainPatch variant *)
          let find_main_track = function
            | `Added (Track.Main t) -> Some (`Added t)
            | `Removed (Track.Main t) -> Some (`Removed t)
            | `Modified (Track.Patch.MainPatch pt) -> Some (`Modified pt)
            | _ -> None
          in
          match List.find_map find_main_track p.tracks with
          | Some main_change -> main_change
          | None -> `Unchanged)
      ~build_value_children:(fun ct (main_track : Track.MainTrack.t) ->
          [Item (create_main_track_item ~get_pointee_name (match ct with
               | Added -> `Added main_track
               | Removed -> `Removed main_track
               | Unchanged -> failwith "Invalid change type for value"
               | Modified -> failwith "Invalid change type for value"))])
      ~build_patch_children:(fun pt ->
          [Item (create_main_track_item ~get_pointee_name (`Modified pt))])
      ~domain_type:DTTrack
  in

  (* Build Tracks sections - added directly as items, not wrapped in a collection *)
  let build_tracks_items (c : (Liveset.t, Liveset.Patch.t) structured_change) : view list =
    (* Get track changes based on the liveset change type *)
    let track_changes = match c with
      | `Added ls ->
        (* Added liveset - all tracks are Added *)
        ls.Liveset.tracks
        |> List.filter (fun t -> match t with
            | Track.Main _ | Track.Return _ -> false
            | _ -> true)
        |> List.map (fun t -> match t with
            | Track.Midi mt -> `Added (Track.Midi mt)
            | Track.Audio at -> `Added (Track.Audio at)
            | Track.Group gt -> `Added (Track.Group gt)
            | _ -> failwith "Unexpected track type")
      | `Removed ls ->
        (* Removed liveset - all tracks are Removed *)
        ls.Liveset.tracks
        |> List.filter (fun t -> match t with
            | Track.Main _ | Track.Return _ -> false
            | _ -> true)
        |> List.map (fun t -> match t with
            | Track.Midi mt -> `Removed (Track.Midi mt)
            | Track.Audio at -> `Removed (Track.Audio at)
            | Track.Group gt -> `Removed (Track.Group gt)
            | _ -> failwith "Unexpected track type")
      | `Modified patch ->
        (* Modified liveset - use the track changes from the patch *)
        patch.tracks
        |> List.filter (fun tc -> match tc with
            | `Added (Track.Main _) -> false
            | `Removed (Track.Main _) -> false
            | `Modified (Track.Patch.MainPatch _) -> false
            | `Added (Track.Return _) -> false
            | `Removed (Track.Return _) -> false
            | _ -> true)
      | `Unchanged -> []
    in
    (* Build track items directly (not wrapped as elements) *)
    List.filter_map (fun tc ->
        match tc with
        | `Added (Track.Midi t) -> Some (Item (create_midi_track_item ~get_pointee_name (`Added t)))
        | `Removed (Track.Midi t) -> Some (Item (create_midi_track_item ~get_pointee_name (`Removed t)))
        | `Modified (Track.Patch.MidiPatch pt) -> Some (Item (create_midi_track_item ~get_pointee_name (`Modified pt)))
        | `Added (Track.Audio t) -> Some (Item (create_audio_track_item ~get_pointee_name (`Added t)))
        | `Removed (Track.Audio t) -> Some (Item (create_audio_track_item ~get_pointee_name (`Removed t)))
        | `Added (Track.Group t) -> Some (Item (create_group_track_item ~get_pointee_name (`Added t)))
        | `Removed (Track.Group t) -> Some (Item (create_group_track_item ~get_pointee_name (`Removed t)))
        | `Modified (Track.Patch.AudioPatch pt) -> Some (Item (create_audio_track_item ~get_pointee_name (`Modified pt)))
        | `Unchanged -> None
        | _ -> failwith "Unexpected track type"
      ) track_changes
  in

  (* Build Returns sections - added directly as items, not wrapped in a collection *)
  let build_returns_items (c : (Liveset.t, Liveset.Patch.t) structured_change) : view list =
    (* Get return track changes based on the liveset change type *)
    let return_changes = match c with
      | `Added ls ->
        ls.Liveset.returns |> List.map (fun t ->
            match t with
            | Track.Return rt -> `Added (Track.Return rt)
            | _ -> failwith "Unexpected non-Return track in returns list"
          )
      | `Removed ls ->
        ls.Liveset.returns |> List.map (fun t ->
            match t with
            | Track.Return rt -> `Removed (Track.Return rt)
            | _ -> failwith "Unexpected non-Return track in returns list"
          )
      | `Modified patch -> patch.returns
      | `Unchanged -> []
    in
    (* Build return track items directly *)
    List.filter_map (fun rc ->
        match rc with
        | `Added (Track.Return t) -> Some (Item (create_audio_track_item ~get_pointee_name (`Added t)))
        | `Removed (Track.Return t) -> Some (Item (create_audio_track_item ~get_pointee_name (`Removed t)))
        | `Modified (Track.Patch.AudioPatch pt) -> Some (Item (create_audio_track_item ~get_pointee_name (`Modified pt)))
        | `Unchanged -> None
        | _ -> failwith "Unexpected track type in Liveset returns"
      ) return_changes
  in

  (* Build Locators collection *)
  let locators_collection = ViewBuilder.build_collection c
      ~name:"Locators"
      ~of_value:(fun (ls : Liveset.t) -> ls.Liveset.locators)
      ~of_patch:(fun (p : Liveset.Patch.t) -> p.locators)
      ~build_item:create_locator_item
      ~domain_type:DTLocator
  in

  (* Combine all children *)
  let children =
    atomic_children
    @ (version_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (main_track_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ build_tracks_items c  (* Tracks added directly as items *)
    @ build_returns_items c  (* Returns added directly as items *)
    @ (locators_collection |> Option.map (fun c -> Collection c) |> option_to_list)
  in

  { name = section_name; change = change_type; domain_type = DTLiveset; children }
