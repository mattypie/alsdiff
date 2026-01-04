open View_model

(** How much detail to show for a particular diff item. *)
type detail_level =
  | None     (** Completely hide the item - not rendered at all *)
  | Summary  (** Show name + change symbol, but no field details *)
  | Compact  (** Show name + change symbol, but no field details (same as Summary for elements/collections) *)
  | Full     (** Show name + change symbol + all fields/sub-views *)

type detail_config = {
  added : detail_level;
  removed : detail_level;
  modified : detail_level;
  unchanged : detail_level;

  (* NEW: type-based control - list of (domain_type, detail_level) pairs *)
  (* Empty list means no type overrides - use change-type defaults *)
  type_modes : (domain_type * detail_level) list;

  max_collection_items : int option;
  show_unchanged_fields : bool;

  (* Customizable prefixes for each change type *)
  prefix_added : string;
  prefix_removed : string;
  prefix_modified : string;
  prefix_unchanged : string;
}

(* Helper to get detail level for a change type (legacy, no type override) *)
let get_detail_level_by_change (cfg : detail_config) (ct : change_type) : detail_level =
  match ct with
  | Unchanged -> cfg.unchanged
  | Added -> cfg.added
  | Removed -> cfg.removed
  | Modified -> cfg.modified

(* Helper to get effective detail level considering type-based overrides *)
(* Type-based control takes precedence over change-type control *)
let get_effective_detail (cfg : detail_config) (ct : change_type) (dt : domain_type) : detail_level =
  (* Step 1: Check type-specific override via list lookup *)
  match List.assoc_opt dt cfg.type_modes with
  | Some level -> level           (* Type mode found, use it *)
  | None ->
    (* Step 2: Fall back to change-type control *)
    get_detail_level_by_change cfg ct

(* Backward-compatible helper - defaults to DTOther for views without domain_type consideration *)
let get_detail_level (cfg : detail_config) (ct : change_type) : detail_level =
  get_detail_level_by_change cfg ct

(* Helper to check if we should render based on detail level *)
let should_render_level (level : detail_level) : bool =
  match level with
  | None -> false
  | Summary | Compact | Full -> true

(* Helper to check if we should show fields for an element *)
let should_show_fields (cfg : detail_config) (elem : element_view) : bool =
  let level = get_effective_detail cfg elem.change elem.domain_type in
  level = Full && elem.fields <> []

(* Helper to filter and limit collection elements *)
let filter_collection_elements
    (cfg : detail_config)
    (col : collection_view)
  : element_view list =
  let level : detail_level = get_effective_detail cfg col.change col.domain_type in
  if not (should_render_level level) then []
  else
    let filtered : element_view list =
      List.filter (fun (e : element_view) ->
          let elem_level : detail_level = get_effective_detail cfg e.change e.domain_type in
          should_render_level elem_level
        ) col.elements
    in
    match cfg.max_collection_items with
    | None -> filtered
    | Some n -> List.take n filtered

(* Preset configurations for common use cases *)

(* Legacy Compact equivalent: hide fields, show compact view *)
let compact = {
  added = Compact;
  removed = Compact;
  modified = Compact;
  unchanged = None;
  type_modes = [];
  max_collection_items = None;
  show_unchanged_fields = false;
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "";
}

(* Legacy Full equivalent: show all details *)
let full = {
  added = Full;
  removed = Full;
  modified = Full;
  unchanged = None;
  type_modes = [];
  max_collection_items = None;
  show_unchanged_fields = false;
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "";
}

(* MIDI-friendly: don't show details for removed clips *)
let midi_friendly = {
  added = Full;
  removed = Summary;  (* Just show "MidiClip: Name" when deleted *)
  modified = Full;
  unchanged = None;
  type_modes = [];
  max_collection_items = Some 50;  (* Limit note output *)
  show_unchanged_fields = false;
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "";
}

(* Quiet mode: minimal output *)
let quiet = {
  added = Summary;
  removed = Summary;
  modified = Summary;           (* Compact *)
  unchanged = None;
  type_modes = [(DTLiveset, Compact)];  (* Show LiveSet sub-views, but children stay in Summary *)
  max_collection_items = Some 10;
  show_unchanged_fields = false;
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "";
}

(* Verbose mode: show everything including unchanged *)
let verbose = {
  added = Full;
  removed = Full;
  modified = Full;
  unchanged = Full;
  type_modes = [];
  max_collection_items = None;
  show_unchanged_fields = true;
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "";
}

(* ==================== Type-Based Presets ==================== *)

(* Device-focused: detailed devices/params, summary clips *)
let device_focused = {
  midi_friendly with
  type_modes = [
    (DTDevice, Full);
    (DTParam, Full);
    (DTClip, Summary);
    (DTNote, None);
  ];
}

(* Producer: detailed tracks/devices, compact params *)
let producer = {
  midi_friendly with
  type_modes = [
    (DTTrack, Full);
    (DTDevice, Full);
    (DTParam, Compact);
    (DTClip, Summary);
  ];
}

(* No clips: hide clips and notes entirely *)
let no_clips = {
  midi_friendly with
  type_modes = [
    (DTClip, None);
    (DTNote, None);
  ];
}

(* Track-only: show only changes down to Track level, hide everything below (devices, clips, etc.) *)
let track_only = {
  quiet with
  type_modes = [
    (DTDevice, None);
    (DTClip, None);
    (DTNote, None);
    (DTParam, None);
    (DTAutomation, None);
    (DTMixer, None);
    (DTRouting, None);
    (DTSend, None);
    (DTPreset, None);
    (DTMacro, None);
    (DTSnapshot, None);
    (DTLoop, None);
    (DTSignature, None);
    (DTSampleRef, None);
    (DTEvent, None);
  ];
}

(* Helper to create a config with custom prefixes *)
let with_prefixes ~(added:string) ~(removed:string) ~(modified:string) ~(unchanged:string)
    (cfg : detail_config) : detail_config =
  { cfg with
    prefix_added = added;
    prefix_removed = removed;
    prefix_modified = modified;
    prefix_unchanged = unchanged;
  }

(* Change type formatting with customizable symbols *)
let pp_change_type cfg fmt = function
  | Unchanged -> Fmt.pf fmt "%s" cfg.prefix_unchanged
  | Added -> Fmt.pf fmt "%s" cfg.prefix_added
  | Removed -> Fmt.pf fmt "%s" cfg.prefix_removed
  | Modified -> Fmt.pf fmt "%s" cfg.prefix_modified

(* Field value formatting *)
let pp_field_value fmt = function
  | Fint i -> Fmt.pf fmt "%d" i
  | Ffloat f -> Fmt.pf fmt "%.2f" f
  | Fbool b -> Fmt.pf fmt "%b" b
  | Fstring s -> Fmt.pf fmt "%s" s

(* Field view rendering *)
let pp_field cfg fmt (field : field_view) =
  (* Always render if called - filtering happens at parent level *)
  Fmt.pf fmt "@[<h>  %a %s: " (pp_change_type cfg) field.change field.name;
  match field.oldval, field.newval with
  | Some old_v, Some new_v ->
    Fmt.pf fmt "%a -> %a@]" pp_field_value old_v pp_field_value new_v
  | Some old_v, None ->
    Fmt.pf fmt "%a@]" pp_field_value old_v
  | None, Some new_v ->
    Fmt.pf fmt "%a@]" pp_field_value new_v
  | None, None -> Fmt.pf fmt "@]"

(* Element view rendering *)
let pp_element cfg fmt (elem : element_view) =
  let level = get_effective_detail cfg elem.change elem.domain_type in
  if not (should_render_level level) then ()
  else
    (* Summary mode: name + change symbol *)
  if level = Summary then
    Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) elem.change elem.name
    (* Compact mode: name + change symbol *)
  else if level = Compact then
    Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) elem.change elem.name
    (* Full mode: name + symbol + fields *)
  else
    Fmt.pf fmt "@[<v>%a %s" (pp_change_type cfg) elem.change elem.name;
  if should_show_fields cfg elem then (
    Fmt.cut fmt ();
    Fmt.list ~sep:Fmt.cut (pp_field cfg) fmt elem.fields
  );
  Fmt.pf fmt "@]"

(* Collection view rendering *)
let pp_collection cfg fmt (col : collection_view) =
  let level = get_effective_detail cfg col.change col.domain_type in
  if not (should_render_level level) then ()
  else
    let elements = filter_collection_elements cfg col in
    if elements = [] then ()
    else
      (* Summary mode: name + change symbol *)
    if level = Summary then
      Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) col.change col.name
      (* Compact mode: name + symbol, elements names + symbols *)
    else if level = Compact then
      Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) col.change col.name
      (* Full mode: show all elements with their details *)
    else
      Fmt.pf fmt "@[<v 2>%a %s" (pp_change_type cfg) col.change col.name;
    List.iter (fun e ->
        Fmt.pf fmt "@\n";
        pp_element cfg fmt e
      ) elements;
    Fmt.pf fmt "@]"

(* Section view rendering *)
let rec pp_section cfg fmt (section : section_view) =
  let level = get_effective_detail cfg section.change section.domain_type in
  if not (should_render_level level) then ()
  else
    (* Filter sub_views based on show_unchanged_fields and detail levels *)
    let sub_views = if cfg.show_unchanged_fields
      then section.sub_views
      else List.filter (fun v ->
          match v with
          | Field f -> f.change <> Unchanged
          | Element e -> e.change <> Unchanged
          | Collection c -> c.change <> Unchanged
          | Section s -> s.change <> Unchanged
        ) section.sub_views
    in
    (* Further filter to remove views that will render nothing due to type_modes *)
    let sub_views = List.filter (fun v ->
        match v with
        | Field f -> should_render_level (get_effective_detail cfg f.change f.domain_type)
        | Element e -> should_render_level (get_effective_detail cfg e.change e.domain_type)
        | Collection c ->
          let col_level = get_effective_detail cfg c.change c.domain_type in
          should_render_level col_level &&
          (* Also check if any elements would render *)
          (filter_collection_elements cfg c) <> []
        | Section s -> should_render_level (get_effective_detail cfg s.change s.domain_type)
      ) sub_views in
    (* Render section header *)
    begin
      if level = Summary then
        Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) section.change section.name
      else if level = Compact then
        Fmt.pf fmt "@[<v 2>%a %s" (pp_change_type cfg) section.change section.name
      else
        Fmt.pf fmt "@[<v 2>%a %s" (pp_change_type cfg) section.change section.name
    end;
    (* Render sub-views for Compact and Full modes *)
    if level <> Summary then (
      List.iter (fun view ->
          Fmt.pf fmt "@\n";
          pp_view cfg fmt view
        ) sub_views;
      Fmt.pf fmt "@]"
    )

(* Main view rendering function *)
and pp_view cfg fmt = function
  | Field field -> pp_field cfg fmt field
  | Element elem -> pp_element cfg fmt elem
  | Collection col -> pp_collection cfg fmt col
  | Section sect -> pp_section cfg fmt sect

(* Top-level pp function - main entry point *)
let pp cfg fmt view =
  pp_view cfg fmt view

(* Top-level render function for string output *)
let render_to_string cfg view =
  Fmt.str "%a" (pp_view cfg) view
