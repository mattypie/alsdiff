open View_model

(** How much detail to show for a particular diff item. *)
type detail_level =
  | None     (** Completely hide the item - not rendered at all *)
  | Summary  (** Show name + change symbol + count of changed items (no field details) *)
  | Compact  (** Show name + change symbol, but no field details (same as Summary for elements/collections) *)
  | Full     (** Show name + change symbol + all fields/sub-views *)

(** Breakdown of changes by type for Summary mode *)
type change_breakdown = {
  added: int;
  removed: int;
  modified: int;
}

(* Per-change-type override for domain types *)
(* None means use the base change_type default *)
type per_change_override = {
  added : detail_level option;
  removed : detail_level option;
  modified : detail_level option;
  unchanged : detail_level option;
}

type detail_config = {
  added : detail_level;
  removed : detail_level;
  modified : detail_level;
  unchanged : detail_level;

  (* Type-based overrides with per-change control.
     Each domain type can optionally override detail levels for specific change types.
     Use `uniform_override level` to set all changes to the same level (preserves legacy behavior).
     Use `override ~added:Full ~removed:Summary ()` for fine-grained control.
     None means use the base change_type default.
   *)
  type_overrides : (domain_type * per_change_override) list;

  max_collection_items : int option;
  show_unchanged_fields : bool;

  (* Customizable prefixes for each change type *)
  prefix_added : string;
  prefix_removed : string;
  prefix_modified : string;
  prefix_unchanged : string;

  (* Note name display style for MIDI notes *)
  note_name_style : Alsdiff_live.Clip.MidiNote.note_display_style;
}

(* Helper: Create a per_change_override with all fields set to None *)
let no_override () = {
  added = None;
  removed = None;
  modified = None;
  unchanged = None;
}

(* Helper: Create a per_change_override with specific values *)
(* Use optional arguments with None as default *)
let override ?(added:(detail_level option)=None) ?(removed:(detail_level option)=None) ?(modified:(detail_level option)=None) ?(unchanged:(detail_level option)=None) () = {
  added;
  removed;
  modified;
  unchanged;
}

(* Helper: Create a uniform override (same level for all changes) *)
(* Preserves current type_modes behavior *)
let uniform_override level = {
  added = Some level;
  removed = Some level;
  modified = Some level;
  unchanged = Some level;
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
  (* Step 1: Check type-specific override *)
  match List.assoc_opt dt cfg.type_overrides with
  | Some overrides ->
    (* Step 2: Check for change-specific override within this type *)
    (match ct with
     | Added -> begin match overrides.added with
       | Some level -> level
       | None -> cfg.added
       end
     | Removed -> begin match overrides.removed with
       | Some level -> level
       | None -> cfg.removed
       end
     | Modified -> begin match overrides.modified with
       | Some level -> level
       | None -> cfg.modified
       end
     | Unchanged -> begin match overrides.unchanged with
       | Some level -> level
       | None -> cfg.unchanged
       end
    )
  | None ->
    (* Step 3: Fall back to change-type base default *)
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
let should_show_fields (cfg : detail_config) (elem : item) : bool =
  let level = get_effective_detail cfg elem.change elem.domain_type in
  level = Full && elem.children <> []

(* Helper to check if an item is element-like (all children are Field views) *)
let is_element_like_item (elem : item) : bool =
  elem.children <> [] &&
  List.for_all (fun (v : view) ->
    match v with
    | Field _ -> true
    | Item _ -> false
    | Collection _ -> false
  ) elem.children

(* Helper to filter and limit collection elements *)
let filter_collection_elements
    (cfg : detail_config)
    (col : collection)
  : item list =
  let level : detail_level = get_effective_detail cfg col.change col.domain_type in
  if not (should_render_level level) then []
  else
    let filtered : item list =
      List.filter_map (fun (v : view) ->
          match v with
          | Item e ->
              let elem_level : detail_level = get_effective_detail cfg e.change e.domain_type in
              if should_render_level elem_level then Some e else None
          | _ -> None
        ) col.items
    in
    match cfg.max_collection_items with
    | None -> filtered
    | Some n -> List.take n filtered

(* Count changed fields in an element *)
let count_changed_fields (elem : item) : int =
  List.filter (fun (v : view) ->
    match v with
    | Field f -> f.change <> Unchanged
    | _ -> false
  ) elem.children
  |> List.length

(* Count filtered elements in a collection *)
let count_changed_elements (cfg : detail_config) (col : collection) : int =
  let filtered = filter_collection_elements cfg col in
  List.length filtered

(* Count filtered sub-views in a section *)
let count_changed_sub_views (cfg : detail_config) (section : item) : int =
  (* First filter: remove unchanged if show_unchanged_fields is false *)
  let sub_views = if cfg.show_unchanged_fields
    then section.children
    else List.filter (fun v ->
        match v with
        | Field f -> f.change <> Unchanged
        | Item e -> e.change <> Unchanged
        | Collection c -> c.change <> Unchanged
      ) section.children
  in
  (* Second filter: remove views that won't render due to type_overrides *)
  let sub_views = List.filter (fun v ->
      match v with
      | Field f -> should_render_level (get_effective_detail cfg f.change f.domain_type)
      | Item e -> should_render_level (get_effective_detail cfg e.change e.domain_type)
      | Collection c ->
        let col_level = get_effective_detail cfg c.change c.domain_type in
        should_render_level col_level &&
        (filter_collection_elements cfg c) <> []
    ) sub_views
  in
  List.length sub_views

(* ==================== Change Breakdown for Summary Mode ==================== *)

(* Calculate total from a breakdown *)
let total_breakdown (b : change_breakdown) : int =
  b.added + b.removed + b.modified

(* Format breakdown as string, omitting zero counts *)
let format_breakdown (breakdown : change_breakdown) : string =
  let parts = [
    if breakdown.added > 0 then Some (Printf.sprintf "%d Added" breakdown.added) else None;
    if breakdown.removed > 0 then Some (Printf.sprintf "%d Removed" breakdown.removed) else None;
    if breakdown.modified > 0 then Some (Printf.sprintf "%d Modified" breakdown.modified) else None;
  ] |> List.filter_map Fun.id in
  match parts with
  | [] -> ""
  | parts -> "(" ^ String.concat ", " parts ^ ")"

(* Increment the appropriate counter based on change_type *)
let increment_breakdown (acc : change_breakdown) (ct : change_type) : change_breakdown =
  match ct with
  | Added -> { acc with added = acc.added + 1 }
  | Removed -> { acc with removed = acc.removed + 1 }
  | Modified -> { acc with modified = acc.modified + 1 }
  | Unchanged -> acc

(* Count fields by change type *)
let count_fields_breakdown (elem : item) : change_breakdown =
  List.fold_left (fun (acc : change_breakdown) (v : view) ->
    match v with
    | Field f -> increment_breakdown acc f.change
    | _ -> acc
  ) ({ added = 0; removed = 0; modified = 0 } : change_breakdown) elem.children

(* Count filtered elements by change type *)
let count_elements_breakdown (cfg : detail_config) (col : collection) : change_breakdown =
  let filtered = filter_collection_elements cfg col in
  List.fold_left (fun (acc : change_breakdown) (e : item) -> increment_breakdown acc e.change)
    ({ added = 0; removed = 0; modified = 0 } : change_breakdown) filtered

(* Count filtered sub-views by change type *)
let count_sub_views_breakdown (cfg : detail_config) (section : item) : change_breakdown =
  (* First filter: remove unchanged if show_unchanged_fields is false *)
  let sub_views = if cfg.show_unchanged_fields
    then section.children
    else List.filter (fun v ->
        match v with
        | Field f -> f.change <> Unchanged
        | Item e -> e.change <> Unchanged
        | Collection c -> c.change <> Unchanged
      ) section.children
  in
  (* Second filter: remove views that won't render due to type_overrides *)
  let sub_views = List.filter (fun v ->
      match v with
      | Field f -> should_render_level (get_effective_detail cfg f.change f.domain_type)
      | Item e -> should_render_level (get_effective_detail cfg e.change e.domain_type)
      | Collection c ->
        let col_level = get_effective_detail cfg c.change c.domain_type in
        should_render_level col_level &&
        (filter_collection_elements cfg c) <> []
    ) sub_views
  in
  (* Count by change type using helper *)
  List.fold_left (fun (acc : change_breakdown) v ->
    match v with
    | Field f -> increment_breakdown acc f.change
    | Item e -> increment_breakdown acc e.change
    | Collection c -> increment_breakdown acc c.change
  ) ({ added = 0; removed = 0; modified = 0 } : change_breakdown) sub_views

(* Preset configurations for common use cases *)

(* Legacy Compact equivalent: hide fields, show compact view *)
let compact = {
  added = Compact;
  removed = Compact;
  modified = Compact;
  unchanged = None;
  type_overrides = [];
  max_collection_items = None;
  show_unchanged_fields = false;
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "";
  note_name_style = Alsdiff_live.Clip.MidiNote.Sharp;
}

(* Legacy Full equivalent: show all details *)
let full = {
  added = Full;
  removed = Full;
  modified = Full;
  unchanged = None;
  type_overrides = [];
  max_collection_items = None;
  show_unchanged_fields = false;
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "";
  note_name_style = Alsdiff_live.Clip.MidiNote.Sharp;
}

(* MIDI-friendly: don't show details for removed clips *)
let midi_friendly = {
  added = Full;
  removed = Summary;  (* Just show "MidiClip: Name" when deleted *)
  modified = Full;
  unchanged = None;
  type_overrides = [];
  max_collection_items = Some 50;  (* Limit note output *)
  show_unchanged_fields = false;
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "";
  note_name_style = Alsdiff_live.Clip.MidiNote.Sharp;
}

(* Quiet mode: minimal output *)
let quiet = {
  added = Summary;
  removed = Summary;
  modified = Summary;           (* Compact *)
  unchanged = None;
  type_overrides = [(DTLiveset, uniform_override Compact)];  (* Show LiveSet sub-views, but children stay in Summary *)
  max_collection_items = Some 10;
  show_unchanged_fields = false;
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "";
  note_name_style = Alsdiff_live.Clip.MidiNote.Sharp;
}

(* Verbose mode: show everything including unchanged *)
let verbose = {
  added = Full;
  removed = Full;
  modified = Full;
  unchanged = Full;
  type_overrides = [];
  max_collection_items = None;
  show_unchanged_fields = true;
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "";
  note_name_style = Alsdiff_live.Clip.MidiNote.Sharp;
}

(* ==================== Type-Based Presets ==================== *)

(* Device-focused: detailed devices/params, summary clips *)
let device_focused = {
  midi_friendly with
  type_overrides = [
    (DTDevice, uniform_override Full);
    (DTParam, uniform_override Full);
    (DTClip, uniform_override Summary);
    (DTNote, uniform_override None);
  ];
}

(* Producer: detailed tracks/devices, compact params *)
let producer = {
  midi_friendly with
  type_overrides = [
    (DTTrack, uniform_override Full);
    (DTDevice, uniform_override Full);
    (DTParam, uniform_override Compact);
    (DTClip, uniform_override Summary);
  ];
}

(* No clips: hide clips and notes entirely *)
let no_clips = {
  midi_friendly with
  type_overrides = [
    (DTClip, uniform_override None);
    (DTNote, uniform_override None);
  ];
}

(* Track-only: show only changes down to Track level, hide everything below (devices, clips, etc.) *)
let track_only = {
  quiet with
  type_overrides = [
    (DTDevice, uniform_override None);
    (DTClip, uniform_override None);
    (DTNote, uniform_override None);
    (DTParam, uniform_override None);
    (DTAutomation, uniform_override None);
    (DTMixer, uniform_override None);
    (DTRouting, uniform_override None);
    (DTSend, uniform_override None);
    (DTPreset, uniform_override None);
    (DTMacro, uniform_override None);
    (DTSnapshot, uniform_override None);
    (DTLoop, uniform_override None);
    (DTSignature, uniform_override None);
    (DTSampleRef, uniform_override None);
    (DTEvent, uniform_override None);
  ];
}

(* NEW: Device-aware preset - demonstrates per-change control *)
(* Full details for added/modified devices, summary for removed *)
let device_aware = {
  midi_friendly with
  type_overrides = [
    (DTDevice, override
      ~added:(Some Full)
      ~removed:(Some Summary)
      ~modified:(Some Full)
      ());
    (DTParam, override
      ~added:(Some Full)
      ~removed:None
      ~modified:(Some Full)
      ());
  ];
}

(* Clip change-aware preset *)
(* Full for added clips (new content), summary for removed (just knowing they're gone) *)
let clip_change_aware = {
  midi_friendly with
  type_overrides = [
    (DTClip, override
      ~added:(Some Full)
      ~removed:(Some Summary)
      ~modified:(Some Full)
      ());
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

(* Builder: Start from a preset and modify type overrides *)
(* Merges with existing override if present *)
(* Note: Pass None (not passing the argument) to preserve existing value *)
let with_type_override cfg dt
    ~(added:detail_level option option) ~(removed:detail_level option option) ~(modified:detail_level option option) ~(unchanged:detail_level option option) =
  let existing = match List.assoc_opt dt cfg.type_overrides with
    | Some ov -> ov
    | None -> no_override ()
  in
  (* Merge options: Some (Some v) means set to v, Some None means set to None, None means keep existing *)
  let new_override = override
    ~added:(match added with Some (Some v) -> Some v | Some None -> None | None -> existing.added)
    ~removed:(match removed with Some (Some v) -> Some v | Some None -> None | None -> existing.removed)
    ~modified:(match modified with Some (Some v) -> Some v | Some None -> None | None -> existing.modified)
    ~unchanged:(match unchanged with Some (Some v) -> Some v | Some None -> None | None -> existing.unchanged)
    ()
  in
  (* Remove old entry if exists, add new one *)
  let filtered = List.filter (fun (d, _) -> d <> dt) cfg.type_overrides in
  { cfg with type_overrides = (dt, new_override) :: filtered }

(* Helper: Convert domain_type to string for debugging/validation *)
let domain_type_to_string (dt : domain_type) : string =
  match dt with
  | DTLiveset -> "Liveset"
  | DTTrack -> "Track"
  | DTDevice -> "Device"
  | DTClip -> "Clip"
  | DTAutomation -> "Automation"
  | DTMixer -> "Mixer"
  | DTRouting -> "Routing"
  | DTLocator -> "Locator"
  | DTParam -> "Param"
  | DTNote -> "Note"
  | DTEvent -> "Event"
  | DTSend -> "Send"
  | DTPreset -> "Preset"
  | DTMacro -> "Macro"
  | DTSnapshot -> "Snapshot"
  | DTLoop -> "Loop"
  | DTSignature -> "Signature"
  | DTSampleRef -> "SampleRef"
  | DTVersion -> "Version"
  | DTOther -> "Other"

(* Validate config - check for suspicious patterns *)
(* Returns list of warning messages *)
let validate_config (cfg : detail_config) : string list =
  cfg.type_overrides
  |> List.filter_map (fun ((dt : domain_type), (ov : per_change_override)) ->
      if ov.added = None && ov.removed = None && ov.modified = None && ov.unchanged = None then
        Some (Printf.sprintf "Type override for %s has all None values (no effect)"
          (domain_type_to_string dt))
      else
        None)

(* Change type formatting with customizable symbols *)
let pp_change_type cfg fmt = function
  | Unchanged -> Fmt.pf fmt "%s" cfg.prefix_unchanged
  | Added -> Fmt.pf fmt "%s" cfg.prefix_added
  | Removed -> Fmt.pf fmt "%s" cfg.prefix_removed
  | Modified -> Fmt.pf fmt "%s" cfg.prefix_modified

(* Render a breakdown in Summary mode format *)
let render_summary_breakdown cfg fmt breakdown name_symbol change_type =
  let total = total_breakdown breakdown in
  if total > 0 then
    let count_str = format_breakdown breakdown in
    Fmt.pf fmt "@[%a %s %s@]" (pp_change_type cfg) change_type name_symbol count_str
  else
    Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) change_type name_symbol

(* Field value formatting *)
let pp_field_value fmt = function
  | Fint i -> Fmt.pf fmt "%d" i
  | Ffloat f -> Fmt.pf fmt "%.2f" f
  | Fbool b -> Fmt.pf fmt "%b" b
  | Fstring s -> Fmt.pf fmt "%s" s

(* Field view rendering *)
let pp_field cfg fmt (field : field) =
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
let rec pp_item cfg fmt (elem : item) =
  let level = get_effective_detail cfg elem.change elem.domain_type in
  if not (should_render_level level) then ()
  else
    (* Summary mode: name + change symbol *)
  if level = Summary then
    render_summary_breakdown cfg fmt (count_fields_breakdown elem) elem.name elem.change
    (* Compact mode: name + change symbol *)
  else if level = Compact then
    Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) elem.change elem.name
    (* Full mode: name + symbol + fields *)
  else
    Fmt.pf fmt "@[<v>%a %s" (pp_change_type cfg) elem.change elem.name;
  if should_show_fields cfg elem then (
    Fmt.cut fmt ();
    (* Render Field children *)
    let fields = List.filter_map (fun (v : view) ->
      match v with
      | Field f -> Some f
      | _ -> None
    ) elem.children in
    Fmt.list ~sep:Fmt.cut (pp_field cfg) fmt fields;
    (* Render Item and Collection children *)
    let nested = List.filter (fun (v : view) ->
      match v with
      | Item _ | Collection _ -> true
      | _ -> false
    ) elem.children in
    List.iter (fun v ->
      Fmt.cut fmt ();
      match v with
      | Item e -> pp_item cfg fmt e
      | Collection c -> pp_collection cfg fmt c
      | _ -> ()
    ) nested
  );
  Fmt.pf fmt "@]"

(* Collection view rendering *)
and pp_collection cfg fmt (col : collection) =
  let level = get_effective_detail cfg col.change col.domain_type in
  if not (should_render_level level) then ()
  else
    let elements = filter_collection_elements cfg col in
    if elements = [] then ()
    else
      (* Summary mode: name + change symbol *)
    if level = Summary then
      render_summary_breakdown cfg fmt (count_elements_breakdown cfg col) col.name col.change
      (* Compact mode: name + symbol, elements names + symbols *)
    else if level = Compact then
      Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) col.change col.name
      (* Full mode: show all elements with their details *)
    else
      Fmt.pf fmt "@[<v 2>%a %s" (pp_change_type cfg) col.change col.name;
    List.iter (fun e ->
        Fmt.pf fmt "@\n";
        pp_item cfg fmt e
      ) elements;
    Fmt.pf fmt "@]"

(* Section view rendering *)
let rec pp_section cfg fmt (section : item) =
  let level = get_effective_detail cfg section.change section.domain_type in
  if not (should_render_level level) then ()
  else
    (* Filter sub_views based on show_unchanged_fields and detail levels *)
    let sub_views = if cfg.show_unchanged_fields
      then section.children
      else List.filter (fun v ->
          match v with
          | Field f -> f.change <> Unchanged
          | Item e -> e.change <> Unchanged
          | Collection c -> c.change <> Unchanged
        ) section.children
    in
    (* Further filter to remove views that will render nothing due to type_overrides *)
    let sub_views = List.filter (fun v ->
        match v with
        | Field f -> should_render_level (get_effective_detail cfg f.change f.domain_type)
        | Item e -> should_render_level (get_effective_detail cfg e.change e.domain_type)
        | Collection c ->
          let col_level = get_effective_detail cfg c.change c.domain_type in
          should_render_level col_level &&
          (* Also check if any elements would render *)
          (filter_collection_elements cfg c) <> []
      ) sub_views in
    (* Render section header *)
    begin
      if level = Summary then
        (* Don't show count for LiveSet - it shows sub-views in Summary mode *)
        if section.domain_type <> DTLiveset then (
          render_summary_breakdown cfg fmt (count_sub_views_breakdown cfg section) section.name section.change
        ) else (
          Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) section.change section.name
        )
      else if level = Compact then
        Fmt.pf fmt "@[<v 2>%a %s" (pp_change_type cfg) section.change section.name
      else
        Fmt.pf fmt "@[<v 2>%a %s" (pp_change_type cfg) section.change section.name
    end;
    (* Render sub-views for Compact and Full modes, OR for LiveSet in Summary mode *)
    if level <> Summary || section.domain_type = DTLiveset then (
      let views_to_render = match level with
        | Compact ->
            (* Compact mode: only show Item and Collection subviews, no Fields *)
            List.filter (fun v ->
              match v with
              | Item _ | Collection _ -> true
              | Field _ -> false
            ) sub_views
        | Full | Summary | None -> sub_views
      in
      List.iter (fun view ->
          Fmt.pf fmt "@\n";
          pp_view cfg fmt view
        ) views_to_render;
      Fmt.pf fmt "@]"
    )

(* Main view rendering function *)
and pp_view cfg fmt = function
  | Field field -> pp_field cfg fmt field
  | Item elem ->
    (* Dispatch to pp_item for element-like items, pp_section for section-like items *)
    if is_element_like_item elem then
      pp_item cfg fmt elem
    else
      pp_section cfg fmt elem
  | Collection col -> pp_collection cfg fmt col

(* Top-level pp function - main entry point *)
let pp cfg fmt view =
  pp_view cfg fmt view

(* Top-level render function for string output *)
let render_to_string cfg view =
  Fmt.str "%a" (pp_view cfg) view
