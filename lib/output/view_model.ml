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


(* ==================== Helper Functions for Field Descriptors ==================== *)

(** Helper functions for creating field descriptors with common wrappers *)
let int_field_desc name ~of_parent_value ~of_parent_patch =
  FieldDesc { name; of_parent_value; of_parent_patch; wrapper = int_value }

let float_field_desc name ~of_parent_value ~of_parent_patch =
  FieldDesc { name; of_parent_value; of_parent_patch; wrapper = float_value }

let string_field_desc name ~of_parent_value ~of_parent_patch =
  FieldDesc { name; of_parent_value; of_parent_patch; wrapper = string_value }

let bool_field_desc name ~of_parent_value ~of_parent_patch =
  FieldDesc { name; of_parent_value; of_parent_patch; wrapper = bool_value }


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


(** [event_value_to_field_value] converts an Automation.event_value to a field_value *)
let event_value_to_field_value v =
  match v with
  | Automation.FloatEvent f -> Ffloat f
  | Automation.IntEvent i -> Fint i
  | Automation.EnumEvent e -> Fint e

(** [event_value_atomic_to_field_value] converts an event_value atomic_update to field_value atomic_update *)
let event_value_atomic_to_field_value (update : Automation.event_value atomic_update) : field_value atomic_update =
  match update with
  | `Modified { oldval; newval } ->
    `Modified { oldval = event_value_to_field_value oldval; newval = event_value_to_field_value newval }
  | `Unchanged -> `Unchanged


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
      of_parent_value = (fun x -> event_value_to_field_value x.EnvelopeEvent.value);
      of_parent_patch = (fun x -> event_value_atomic_to_field_value x.EnvelopeEvent.Patch.value);
      wrapper = Fun.id;
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


(** A specification for building a child section of an Item *)
type ('parent, 'patch) section_spec = {
  name : string;
  build : ('parent, 'patch) structured_change -> view option;
}


(** Spec module - combinators for building section_spec values declaratively *)
module Spec = struct

  (** [inline_fields ~specs ~domain_type] builds inline field views from unified field specs.
      Returns Field views wrapped in an Item with empty name (""), automatically filtering
      out Unchanged fields. The empty name signals to [build_item_from_specs] that this
      Item's children should be inlined directly into the parent.
  *)
  let inline_fields
      (type v p)
      ~(specs : (v, p) unified_field_spec list)
      ~(domain_type : domain_type)
    : (v, p) section_spec =
    {
      name = "";
      build = (fun c ->
          let fields = match c with
            | `Added value ->
              build_value_field_views specs Added value ~domain_type
            | `Removed value ->
              build_value_field_views specs Removed value ~domain_type
            | `Modified patch ->
              build_patch_field_views specs patch ~domain_type
            | `Unchanged -> []
          in
          (* Filter out Unchanged fields *)
          let filtered = List.filter (function
              | Field f -> f.change <> Unchanged
              | _ -> true
            ) fields in
          if filtered = [] then None
          else Some (Item { name = ""; change = ViewBuilder.change_type_of c; domain_type; children = filtered })
        );
    }

  (** [child ~name ~of_value ~of_patch ~build_value_children ~build_patch_children ~domain_type]
      builds a section_spec for a nested item (e.g., Mixer, Loop).
  *)
  let child
      (type parent patch nested np)
      ~(name : string)
      ~(of_value : parent -> nested)
      ~(of_patch : patch -> np structured_update)
      ~(build_value_children : change_type -> nested -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
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

  (** [child_optional ~name ~of_value ~of_patch ~build_value_children ~build_patch_children ~domain_type]
      builds a section_spec for a nested item that can be added/removed independently.
  *)
  let child_optional
      (type parent patch nested np)
      ~(name : string)
      ~(of_value : parent -> nested option)
      ~(of_patch : patch -> (nested, np) structured_change)
      ~(build_value_children : change_type -> nested -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
    {
      name;
      build = (fun c ->
          ViewBuilder.build_item_from_children_with_change c
            ~name
            ~of_value
            ~of_patch
            ~build_value_children
            ~build_patch_children
            ~domain_type
          |> Option.map (fun i -> Item i)
        );
    }

  (** [collection ~name ~of_value ~of_patch ~build_item ~domain_type]
      builds a section_spec for a collection of items.
  *)
  let collection
      (type parent patch elem ep)
      ~(name : string)
      ~(of_value : parent -> elem list)
      ~(of_patch : patch -> (elem, ep) structured_change list)
      ~(build_item : (elem, ep) structured_change -> item)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
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

end


(** [build_item_from_specs ~name ~domain_type ~specs c] builds an item from a list of section specs.
    This is the main entry point for declaratively building complex items.
    Each spec in the list is applied to the change, and the resulting views are concatenated.

    For inline_fields specs (name = ""), the children are extracted and added directly.
    For other specs, the resulting view is added as-is.
*)
let build_item_from_specs
    (type parent patch)
    ~(name : string)
    ~(domain_type : domain_type)
    ~(specs : (parent, patch) section_spec list)
    (c : (parent, patch) structured_change)
  : item =
  let change_type = ViewBuilder.change_type_of c in
  let children = specs |> List.filter_map (fun spec ->
      match spec.build c with
      | None -> None
      | Some view ->
        (* For inline_fields (name = ""), extract children directly *)
        if spec.name = "" then
          match view with
          | Item { children; _ } -> Some children
          | _ -> Some [view]
        else
          Some [view]
    ) |> List.flatten in
  { name; change = change_type; domain_type; children }


(** [build_item_from_specs_opt] is like [build_item_from_specs] but returns None if no children.
    Use this for optional items like Mixer that should not appear if empty.
*)
let build_item_from_specs_opt
    (type parent patch)
    ~(name : string)
    ~(domain_type : domain_type)
    ~(specs : (parent, patch) section_spec list)
    (c : (parent, patch) structured_change)
  : item option =
  let item = build_item_from_specs ~name ~domain_type ~specs c in
  if item.children = [] then None else Some item


(* ==================== MidiClip Specs (using new infrastructure) ==================== *)

(** MidiClip field specifications *)
let midi_clip_field_specs : (Clip.MidiClip.t, Clip.MidiClip.Patch.t) unified_field_spec list = [
  { name = "Name";
    get_value = (fun c -> string_value c.Clip.MidiClip.name);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.Clip.MidiClip.Patch.name) };
  { name = "Start Time";
    get_value = (fun c -> float_value c.Clip.MidiClip.start_time);
    get_patch = (fun p -> ViewBuilder.map_atomic_update float_value p.Clip.MidiClip.Patch.start_time) };
  { name = "End Time";
    get_value = (fun c -> float_value c.Clip.MidiClip.end_time);
    get_patch = (fun p -> ViewBuilder.map_atomic_update float_value p.Clip.MidiClip.Patch.end_time) };
]

(** [build_clip_section_name ~clip_type ~get_id ~get_name ~get_patch_id ~get_patch_name c]
    builds a section name for any clip type.
    @param clip_type The clip type label (e.g., "MidiClip", "AudioClip")
    @param get_id Extracts the ID from a clip value
    @param get_name Extracts the name from a clip value
    @param get_patch_id Extracts the ID from a clip patch
    @param get_patch_name Extracts the name atomic update from a clip patch
*)
let build_clip_section_name
    (type v p)
    ~(clip_type : string)
    ~(get_id : v -> int)
    ~(get_name : v -> string)
    ~(get_patch_id : p -> int)
    ~(get_patch_name : p -> string atomic_update)
    (c : (v, p) structured_change)
  : string =
  match c with
  | `Added clip -> Printf.sprintf "%s (#%d): %s" clip_type (get_id clip) (get_name clip)
  | `Removed clip -> Printf.sprintf "%s (#%d): %s" clip_type (get_id clip) (get_name clip)
  | `Modified patch ->
    (match get_patch_name patch with
     | `Modified { newval; _ } -> Printf.sprintf "%s (#%d): %s" clip_type (get_patch_id patch) newval
     | `Unchanged -> Printf.sprintf "%s (#%d)" clip_type (get_patch_id patch))
  | `Unchanged -> clip_type

(** [build_midi_clip_section_name] builds the section name for a MidiClip. *)
let build_midi_clip_section_name =
  build_clip_section_name
    ~clip_type:"MidiClip"
    ~get_id:(fun c -> c.Clip.MidiClip.id)
    ~get_name:(fun c -> c.Clip.MidiClip.name)
    ~get_patch_id:(fun p -> p.Clip.MidiClip.Patch.id)
    ~get_patch_name:(fun p -> p.Clip.MidiClip.Patch.name)

(** MidiClip section specs - defines the structure of a MidiClip item *)
let midi_clip_section_specs : (Clip.MidiClip.t, Clip.MidiClip.Patch.t) section_spec list = [
  Spec.inline_fields ~specs:midi_clip_field_specs ~domain_type:DTClip;
  Spec.child ~name:"Loop"
    ~of_value:(fun (c : Clip.MidiClip.t) -> c.loop)
    ~of_patch:(fun (p : Clip.MidiClip.Patch.t) -> p.loop)
    ~build_value_children:create_loop_fields
    ~build_patch_children:create_loop_patch_fields
    ~domain_type:DTLoop;
  Spec.child ~name:"TimeSignature"
    ~of_value:(fun (c : Clip.MidiClip.t) -> c.signature)
    ~of_patch:(fun (p : Clip.MidiClip.Patch.t) -> p.signature)
    ~build_value_children:create_signature_fields
    ~build_patch_children:create_signature_patch_fields
    ~domain_type:DTSignature;
  Spec.collection ~name:"Notes"
    ~of_value:(fun (c : Clip.MidiClip.t) -> c.notes)
    ~of_patch:(fun (p : Clip.MidiClip.Patch.t) -> p.notes)
    ~build_item:create_note_item
    ~domain_type:DTNote;
]

(** [create_midi_clip_item] creates a [item] from a MidiClip structured change (new type system).
    @param c the midi clip structured change
*)
let create_midi_clip_item
    (c : (Clip.MidiClip.t, Clip.MidiClip.Patch.t) structured_change)
  : item =
  let name = build_midi_clip_section_name c in
  build_item_from_specs ~name ~domain_type:DTClip ~specs:midi_clip_section_specs c


(* ==================== AudioClip Specs (using new infrastructure) ==================== *)

(** AudioClip field specifications *)
let audio_clip_field_specs : (Clip.AudioClip.t, Clip.AudioClip.Patch.t) unified_field_spec list = [
  { name = "Name";
    get_value = (fun c -> string_value c.Clip.AudioClip.name);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.Clip.AudioClip.Patch.name) };
  { name = "Start Time";
    get_value = (fun c -> float_value c.Clip.AudioClip.start_time);
    get_patch = (fun p -> ViewBuilder.map_atomic_update float_value p.Clip.AudioClip.Patch.start_time) };
  { name = "End Time";
    get_value = (fun c -> float_value c.Clip.AudioClip.end_time);
    get_patch = (fun p -> ViewBuilder.map_atomic_update float_value p.Clip.AudioClip.Patch.end_time) };
]

(** [build_audio_clip_section_name] builds the section name for an AudioClip. *)
let build_audio_clip_section_name =
  build_clip_section_name
    ~clip_type:"AudioClip"
    ~get_id:(fun c -> c.Clip.AudioClip.id)
    ~get_name:(fun c -> c.Clip.AudioClip.name)
    ~get_patch_id:(fun p -> p.Clip.AudioClip.Patch.id)
    ~get_patch_name:(fun p -> p.Clip.AudioClip.Patch.name)

(** AudioClip section specs - defines the structure of an AudioClip item *)
let audio_clip_section_specs : (Clip.AudioClip.t, Clip.AudioClip.Patch.t) section_spec list = [
  Spec.inline_fields ~specs:audio_clip_field_specs ~domain_type:DTClip;
  Spec.child ~name:"Loop"
    ~of_value:(fun (c : Clip.AudioClip.t) -> c.loop)
    ~of_patch:(fun (p : Clip.AudioClip.Patch.t) -> p.loop)
    ~build_value_children:create_loop_fields
    ~build_patch_children:create_loop_patch_fields
    ~domain_type:DTLoop;
  Spec.child ~name:"TimeSignature"
    ~of_value:(fun (c : Clip.AudioClip.t) -> c.signature)
    ~of_patch:(fun (p : Clip.AudioClip.Patch.t) -> p.signature)
    ~build_value_children:create_signature_fields
    ~build_patch_children:create_signature_patch_fields
    ~domain_type:DTSignature;
  Spec.child ~name:"SampleRef"
    ~of_value:(fun (c : Clip.AudioClip.t) -> c.sample_ref)
    ~of_patch:(fun (p : Clip.AudioClip.Patch.t) -> p.sample_ref)
    ~build_value_children:create_sample_ref_fields
    ~build_patch_children:create_sample_ref_patch_fields
    ~domain_type:DTSampleRef;
]

(** [create_audio_clip_item] creates a [item] from an AudioClip structured change (new type system).
    @param c the audio clip structured change
*)
let create_audio_clip_item
    (c : (Clip.AudioClip.t, Clip.AudioClip.Patch.t) structured_change)
  : item =
  let name = build_audio_clip_section_name c in
  build_item_from_specs ~name ~domain_type:DTClip ~specs:audio_clip_section_specs c


(** Alias for backwards compatibility - will be removed after refactoring *)
type ('device, 'patch) device_section_config = ('device, 'patch) section_spec


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
          ~build_value_children:create_preset_ref_fields
          ~build_patch_children:create_preset_ref_patch_fields
          ~domain_type
        |> Option.map (fun i -> Item i)
      );
  }


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

  let params_config = Spec.collection
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

  let params_config = Spec.collection
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

  let params_config = Spec.collection
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

  let macros_config = Spec.collection
      ~name:"Macros"
      ~of_value:(fun (d : Device.GroupDevice.t) -> d.macros)
      ~of_patch:(fun (p : Device.GroupDevice.Patch.t) -> p.macros)
      ~build_item:create_macro_item
      ~domain_type:DTMacro
  in

  let snapshots_config = Spec.collection
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
let format_event_value v =
  match v with
  | Automation.FloatEvent f -> Printf.sprintf "%.2f" f
  | Automation.IntEvent i -> string_of_int i
  | Automation.EnumEvent e -> Printf.sprintf "%d" e

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
                           newval = Some (Fstring (Printf.sprintf "Time=%.2f, Value=%s"
                                                     e.Automation.EnvelopeEvent.time
                                                     (format_event_value e.Automation.EnvelopeEvent.value))) })
           | `Removed e ->
             Some (Field { name = prefix ^ " Removed"; change = Removed; domain_type = DTEvent;
                           oldval = Some (Fstring (Printf.sprintf "Time=%.2f, Value=%s"
                                                     e.Automation.EnvelopeEvent.time
                                                     (format_event_value e.Automation.EnvelopeEvent.value)));
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
                               oldval = Some (event_value_to_field_value oldval); newval = Some (event_value_to_field_value newval) })
             in
             (* Combine both fields if they exist *)
             (match time_field, value_field with
              | None, None -> None
              | Some tf, None -> Some tf
              | None, Some vf -> Some vf
              | Some (Field tf), Some (Field vf) -> Some (Field { name = prefix; change = Modified; domain_type = DTEvent;
                                                                  oldval = Some (Fstring (Printf.sprintf "Time: %.2f->%.2f Value: %s->%s"
                                                                                            (match tf.oldval with Some (Ffloat f) -> f | _ -> 0.)
                                                                                            (match tf.newval with Some (Ffloat f) -> f | _ -> 0.)
                                                                                            (match vf.oldval with Some (Ffloat f) -> Printf.sprintf "%.2f" f | Some (Fint i) -> string_of_int i | _ -> "?")
                                                                                            (match vf.newval with Some (Ffloat f) -> Printf.sprintf "%.2f" f | Some (Fint i) -> string_of_int i | _ -> "?")));
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

(** Mixer section specs - defines the structure of a Mixer item *)
let mixer_section_specs : (Track.Mixer.t, Track.Mixer.Patch.t) section_spec list = [
  Spec.child ~name:"Volume"
    ~of_value:(fun (m : Track.Mixer.t) -> m.volume)
    ~of_patch:(fun (p : Track.Mixer.Patch.t) -> p.volume)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.child ~name:"Pan"
    ~of_value:(fun (m : Track.Mixer.t) -> m.pan)
    ~of_patch:(fun (p : Track.Mixer.Patch.t) -> p.pan)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.child ~name:"Mute"
    ~of_value:(fun (m : Track.Mixer.t) -> m.mute)
    ~of_patch:(fun (p : Track.Mixer.Patch.t) -> p.mute)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.child ~name:"Solo"
    ~of_value:(fun (m : Track.Mixer.t) -> m.solo)
    ~of_patch:(fun (p : Track.Mixer.Patch.t) -> p.solo)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.collection ~name:"Sends"
    ~of_value:(fun (m : Track.Mixer.t) -> m.sends)
    ~of_patch:(fun (p : Track.Mixer.Patch.t) -> p.sends)
    ~build_item:create_send_item
    ~domain_type:DTSend;
]

(** [create_mixer_item] creates a [item] from a Mixer structured change (new type system).
    @param c the mixer structured change
*)
let create_mixer_item
    (c : (Track.Mixer.t, Track.Mixer.Patch.t) structured_change)
  : item option =
  build_item_from_specs_opt ~name:"Mixer" ~domain_type:DTMixer ~specs:mixer_section_specs c


(** MainMixer section specs - defines the structure of a MainMixer item *)
let main_mixer_section_specs : (Track.MainMixer.t, Track.MainMixer.Patch.t) section_spec list = [
  Spec.child ~name:"Volume"
    ~of_value:(fun (mm : Track.MainMixer.t) -> mm.base.Track.Mixer.volume)
    ~of_patch:(fun (p : Track.MainMixer.Patch.t) ->
        match p.base with
        | `Modified mp -> mp.Track.Mixer.Patch.volume
        | `Unchanged -> `Unchanged)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.child ~name:"Pan"
    ~of_value:(fun (mm : Track.MainMixer.t) -> mm.base.Track.Mixer.pan)
    ~of_patch:(fun (p : Track.MainMixer.Patch.t) ->
        match p.base with
        | `Modified mp -> mp.Track.Mixer.Patch.pan
        | `Unchanged -> `Unchanged)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.child ~name:"Mute"
    ~of_value:(fun (mm : Track.MainMixer.t) -> mm.base.Track.Mixer.mute)
    ~of_patch:(fun (p : Track.MainMixer.Patch.t) ->
        match p.base with
        | `Modified mp -> mp.Track.Mixer.Patch.mute
        | `Unchanged -> `Unchanged)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.child ~name:"Solo"
    ~of_value:(fun (mm : Track.MainMixer.t) -> mm.base.Track.Mixer.solo)
    ~of_patch:(fun (p : Track.MainMixer.Patch.t) ->
        match p.base with
        | `Modified mp -> mp.Track.Mixer.Patch.solo
        | `Unchanged -> `Unchanged)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.child ~name:"Tempo"
    ~of_value:(fun (mm : Track.MainMixer.t) -> mm.tempo)
    ~of_patch:(fun (p : Track.MainMixer.Patch.t) -> p.tempo)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.child ~name:"Time Signature"
    ~of_value:(fun (mm : Track.MainMixer.t) -> mm.time_signature)
    ~of_patch:(fun (p : Track.MainMixer.Patch.t) -> p.time_signature)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.child ~name:"Crossfade"
    ~of_value:(fun (mm : Track.MainMixer.t) -> mm.crossfade)
    ~of_patch:(fun (p : Track.MainMixer.Patch.t) -> p.crossfade)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
  Spec.child ~name:"Global Groove"
    ~of_value:(fun (mm : Track.MainMixer.t) -> mm.global_groove)
    ~of_patch:(fun (p : Track.MainMixer.Patch.t) -> p.global_groove)
    ~build_value_children:create_generic_param_fields
    ~build_patch_children:create_generic_param_patch_fields
    ~domain_type:DTMixer;
]

(** [create_main_mixer_item] creates a [item] from a MainMixer structured change (new type system).
    @param c the main mixer structured change
*)
let create_main_mixer_item
    (c : (Track.MainMixer.t, Track.MainMixer.Patch.t) structured_change)
  : item option =
  build_item_from_specs_opt ~name:"Main Mixer" ~domain_type:DTMixer ~specs:main_mixer_section_specs c


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


(* ==================== Track Item Template Infrastructure (New Type System) ==================== *)


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

  let clips_config = Spec.collection
      ~name:"Clips"
      ~of_value:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.clips)
      ~of_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.clips)
      ~build_item:create_midi_clip_item
      ~domain_type:DTClip
  in

  let mixer_config = Spec.child
      ~name:"Mixer"
      ~of_value:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.mixer)
      ~of_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.mixer)
      ~build_value_children:build_mixer_value_fields
      ~build_patch_children:build_mixer_patch_fields
      ~domain_type:DTMixer
  in

  let automations_config = Spec.collection
      ~name:"Automations"
      ~of_value:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.automations)
      ~of_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.automations)
      ~build_item:(create_automation_item ~get_pointee_name)
      ~domain_type:DTAutomation
  in

  let devices_config = Spec.collection
      ~name:"Devices"
      ~of_value:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.devices)
      ~of_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.devices)
      ~build_item:create_device_item
      ~domain_type:DTDevice
  in

  let routings_config = Spec.child
      ~name:"Routings"
      ~of_value:(fun (t : Track.MidiTrack.t) -> t.Track.MidiTrack.routings)
      ~of_patch:(fun (p : Track.MidiTrack.Patch.t) -> p.routings)
      ~build_value_children:create_routing_set_fields
      ~build_patch_children:create_routing_set_patch_fields
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

  let clips_config = Spec.collection
      ~name:"Clips"
      ~of_value:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.clips)
      ~of_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.clips)
      ~build_item:create_audio_clip_item
      ~domain_type:DTClip
  in

  let mixer_config = Spec.child
      ~name:"Mixer"
      ~of_value:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.mixer)
      ~of_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.mixer)
      ~build_value_children:build_mixer_value_fields
      ~build_patch_children:build_mixer_patch_fields
      ~domain_type:DTMixer
  in

  let automations_config = Spec.collection
      ~name:"Automations"
      ~of_value:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.automations)
      ~of_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.automations)
      ~build_item:(create_automation_item ~get_pointee_name)
      ~domain_type:DTAutomation
  in

  let devices_config = Spec.collection
      ~name:"Devices"
      ~of_value:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.devices)
      ~of_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.devices)
      ~build_item:create_device_item
      ~domain_type:DTDevice
  in

  let routings_config = Spec.child
      ~name:"Routings"
      ~of_value:(fun (t : Track.AudioTrack.t) -> t.Track.AudioTrack.routings)
      ~of_patch:(fun (p : Track.AudioTrack.Patch.t) -> p.routings)
      ~build_value_children:create_routing_set_fields
      ~build_patch_children:create_routing_set_patch_fields
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

  let mixer_config = Spec.child
      ~name:"Main Mixer"
      ~of_value:(fun (t : Track.MainTrack.t) -> t.Track.MainTrack.mixer)
      ~of_patch:(fun (p : Track.MainTrack.Patch.t) -> p.mixer)
      ~build_value_children:build_main_mixer_value_fields
      ~build_patch_children:build_main_mixer_patch_fields
      ~domain_type:DTMixer
  in

  let automations_config = Spec.collection
      ~name:"Automations"
      ~of_value:(fun (t : Track.MainTrack.t) -> t.Track.MainTrack.automations)
      ~of_patch:(fun (p : Track.MainTrack.Patch.t) -> p.automations)
      ~build_item:(create_automation_item ~get_pointee_name)
      ~domain_type:DTAutomation
  in

  let devices_config = Spec.collection
      ~name:"Devices"
      ~of_value:(fun (t : Track.MainTrack.t) -> t.Track.MainTrack.devices)
      ~of_patch:(fun (p : Track.MainTrack.Patch.t) -> p.devices)
      ~build_item:create_device_item
      ~domain_type:DTDevice
  in

  let routings_config = Spec.child
      ~name:"Routings"
      ~of_value:(fun (t : Track.MainTrack.t) -> t.Track.MainTrack.routings)
      ~of_patch:(fun (p : Track.MainTrack.Patch.t) -> p.routings)
      ~build_value_children:create_routing_set_fields
      ~build_patch_children:create_routing_set_patch_fields
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


(* ==================== Liveset Helper Functions ==================== *)

(** [make_pointee_resolver c] creates a pointee name resolver function from a liveset change.
    This is used to resolve automation target IDs to human-readable names.
*)
let make_pointee_resolver
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : int -> string =
  match c with
  | `Added ls -> (fun id -> Liveset.get_pointee_name_from_table ls.Liveset.pointees id)
  | `Removed ls -> (fun id -> Liveset.get_pointee_name_from_table ls.Liveset.pointees id)
  | `Modified patch ->
    (fun id ->
       match Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.new_pointees id with
       | Some name -> name
       | None ->
         match Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.old_pointees id with
         | Some name -> name
         | None -> Printf.sprintf "<Pointee %d>" id)
  | `Unchanged -> fun id -> Printf.sprintf "<Pointee %d>" id


(** [dispatch_track_change ~get_pointee_name tc] dispatches a track change to the appropriate
    track item builder based on track type.
    Returns None for Unchanged or Main tracks (Main tracks are handled separately).
*)
let dispatch_track_change
    ~(get_pointee_name : int -> string)
    (tc : (Track.t, Track.Patch.t) structured_change)
  : view option =
  match tc with
  (* Midi tracks *)
  | `Added (Track.Midi t) -> Some (Item (create_midi_track_item ~get_pointee_name (`Added t)))
  | `Removed (Track.Midi t) -> Some (Item (create_midi_track_item ~get_pointee_name (`Removed t)))
  | `Modified (Track.Patch.MidiPatch pt) -> Some (Item (create_midi_track_item ~get_pointee_name (`Modified pt)))
  (* Audio tracks *)
  | `Added (Track.Audio t) -> Some (Item (create_audio_track_item ~get_pointee_name (`Added t)))
  | `Removed (Track.Audio t) -> Some (Item (create_audio_track_item ~get_pointee_name (`Removed t)))
  | `Modified (Track.Patch.AudioPatch pt) -> Some (Item (create_audio_track_item ~get_pointee_name (`Modified pt)))
  (* Group tracks *)
  | `Added (Track.Group t) -> Some (Item (create_group_track_item ~get_pointee_name (`Added t)))
  | `Removed (Track.Group t) -> Some (Item (create_group_track_item ~get_pointee_name (`Removed t)))
  (* Return tracks - use audio track builder since ReturnTrack = AudioTrack *)
  | `Added (Track.Return t) -> Some (Item (create_audio_track_item ~get_pointee_name (`Added t)))
  | `Removed (Track.Return t) -> Some (Item (create_audio_track_item ~get_pointee_name (`Removed t)))
  (* Main tracks - handled separately in create_liveset_item *)
  | `Added (Track.Main _) | `Removed (Track.Main _) | `Modified (Track.Patch.MainPatch _) -> None
  | `Unchanged -> None


(** [build_liveset_tracks_items ~get_pointee_name c] builds view items for all regular tracks
    (Midi, Audio, Group) in a liveset change. Main and Return tracks are handled separately.
*)
let build_liveset_tracks_items
    ~(get_pointee_name : int -> string)
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : view list =
  let is_regular_track = function
    | Track.Main _ | Track.Return _ -> false
    | _ -> true
  in
  let is_regular_track_change = function
    | `Added (Track.Main _) | `Removed (Track.Main _) | `Modified (Track.Patch.MainPatch _) -> false
    | `Added (Track.Return _) | `Removed (Track.Return _) -> false
    | _ -> true
  in
  let track_changes = match c with
    | `Added ls ->
      ls.Liveset.tracks
      |> List.filter is_regular_track
      |> List.map (fun t -> `Added t)
    | `Removed ls ->
      ls.Liveset.tracks
      |> List.filter is_regular_track
      |> List.map (fun t -> `Removed t)
    | `Modified patch ->
      patch.tracks |> List.filter is_regular_track_change
    | `Unchanged -> []
  in
  List.filter_map (dispatch_track_change ~get_pointee_name) track_changes


(** [build_liveset_returns_items ~get_pointee_name c] builds view items for all return tracks
    in a liveset change.
*)
let build_liveset_returns_items
    ~(get_pointee_name : int -> string)
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : view list =
  let return_changes = match c with
    | `Added ls ->
      ls.Liveset.returns |> List.map (fun t -> `Added t)
    | `Removed ls ->
      ls.Liveset.returns |> List.map (fun t -> `Removed t)
    | `Modified patch -> patch.returns
    | `Unchanged -> []
  in
  List.filter_map (dispatch_track_change ~get_pointee_name) return_changes


(** Liveset field specifications for atomic fields (Name, Creator) *)
let liveset_field_specs : (Liveset.t, Liveset.Patch.t) unified_field_spec list = [
  { name = "Name";
    get_value = (fun ls -> string_value ls.Liveset.name);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.Liveset.Patch.name) };
  { name = "Creator";
    get_value = (fun ls -> string_value ls.Liveset.creator);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.Liveset.Patch.creator) };
]


(** [create_liveset_item] creates a [item] from a Liveset structured change (new type system).
    @param c the liveset structured change
*)
let create_liveset_item
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : item =

  let change_type = ViewBuilder.change_type_of c in
  let get_pointee_name = make_pointee_resolver c in

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

  (* Build atomic fields using liveset_field_specs *)
  let atomic_children =
    (match c with
     | `Added v -> build_value_field_views liveset_field_specs Added v ~domain_type:DTLiveset
     | `Removed v -> build_value_field_views liveset_field_specs Removed v ~domain_type:DTLiveset
     | `Modified p -> build_patch_field_views liveset_field_specs p ~domain_type:DTLiveset
     | `Unchanged -> [])
    |> List.filter (function Field fv -> fv.change <> Unchanged | _ -> true)
  in

  (* Build Version section *)
  let version_item = ViewBuilder.build_item_from_children c
      ~name:"Version"
      ~of_value:(fun (ls : Liveset.t) -> ls.version)
      ~of_patch:(fun (p : Liveset.Patch.t) -> p.version)
      ~build_value_children:create_version_fields
      ~build_patch_children:create_version_patch_fields
      ~domain_type:DTVersion
  in

  (* Build Main Track section - special handling for singleton track *)
  let main_track_item = ViewBuilder.build_item_from_children_with_change c
      ~name:"Main Track"
      ~of_value:(fun (ls : Liveset.t) ->
          match ls.Liveset.main with
          | Track.Main t -> Some t
          | _ -> None)
      ~of_patch:(fun (p : Liveset.Patch.t) ->
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
    @ build_liveset_tracks_items ~get_pointee_name c
    @ build_liveset_returns_items ~get_pointee_name c
    @ (locators_collection |> Option.map (fun c -> Collection c) |> option_to_list)
  in

  { name = section_name; change = change_type; domain_type = DTLiveset; children }
