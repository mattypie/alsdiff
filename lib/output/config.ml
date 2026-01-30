open View_model

(** How much detail to show for a particular diff item. *)
type detail_level =
  | Ignore   (** Completely hide the item - not rendered at all *)
  | Summary  (** Show name + change symbol + count of changed items (no field details) *)
  | Compact  (** Show name + change symbol, but no field details (same as Summary for elements/collections) *)
  | Inline   (** Show name + change symbol + all fields inline on single line *)
  | Full     (** Show name + change symbol + all fields/sub-views (multiline) *)
[@@deriving yojson { strict = false }, jsonschema]

(** Breakdown of changes by type for Summary mode *)
type change_breakdown = {
  added: int;
  removed: int;
  modified: int;
}
[@@deriving yojson, jsonschema]

(* Per-change-type override for domain types *)
(* None means use the base change_type default *)
type per_change_override = {
  added : detail_level option; [@ref "detail_level"] [@default None]
  removed : detail_level option; [@ref "detail_level"] [@default None]
  modified : detail_level option; [@ref "detail_level"] [@default None]
  unchanged : detail_level option; [@ref "detail_level"] [@default None]
}
[@@deriving yojson, jsonschema]

(* Type-based override entry - wraps domain_type and per_change_override *)
(* Using a record instead of tuple enables [@ref ...] attribute for cleaner JSON schema *)
type type_override_entry = {
  domain_type : domain_type; [@ref "domain_type"]
  override : per_change_override; [@ref "per_change_override"]
}
[@@deriving yojson, jsonschema]

type detail_config = {
  added : detail_level; [@ref "detail_level"]
  removed : detail_level; [@ref "detail_level"]
  modified : detail_level; [@ref "detail_level"]
  unchanged : detail_level; [@ref "detail_level"]

  (* Type-based overrides with per-change control.
     Each domain type can optionally override detail levels for specific change types.
     Use `uniform_override level` to set all changes to the same level (preserves legacy behavior).
     Use `override ~added:Full ~removed:Summary ()` for fine-grained control.
     None means use the base change_type default.
  *)
  type_overrides : type_override_entry list;

  max_collection_items : int option [@default None];

  (* Customizable prefixes for each change type *)
  prefix_added : string [@default "+"];
  prefix_removed : string [@default "-"];
  prefix_modified : string [@default "*"];
  prefix_unchanged : string [@default "="];

  (* Note name display style for MIDI notes *)
  note_name_style : note_display_style; [@ref "note_display_style"] [@default Sharp]

  (* Indentation width for rendered output (number of spaces) *)
  indent_width : int [@default 2];
}
[@@deriving yojson { strict = false }, jsonschema]

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
  match List.find_opt (fun entry -> entry.domain_type = dt) cfg.type_overrides with
  | Some entry ->
    (* Step 2: Check for change-specific override within this type *)
    let overrides = entry.override in
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
  | Ignore -> false
  | Summary | Compact | Inline | Full -> true

(* Helper to check if we should show fields for an element *)
let should_show_fields (cfg : detail_config) (elem : item) : bool =
  let level = get_effective_detail cfg elem.change elem.domain_type in
  (level = Full || level = Inline) && elem.children <> []

(* Helper to check if an item is element-like (all children are Field views) *)
let is_element_like_item (elem : item) : bool =
  elem.children <> [] &&
  List.for_all (fun (v : view) ->
      match v with
      | Field _ -> true
      | Item _ -> false
      | Collection _ -> false
    ) elem.children

(* Truncation info for collections when max_collection_items is applied *)
type truncation_info = {
  total: int;
  displayed: int;
  truncated_breakdown: change_breakdown;  (* breakdown of hidden items only *)
}

(* Helper to filter and limit collection elements, returning truncation info *)
let filter_collection_elements_with_info
    (cfg : detail_config)
    (col : collection)
  : item list * truncation_info option =
  let level : detail_level = get_effective_detail cfg col.change col.domain_type in
  if not (should_render_level level) then ([], None)
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
    let total = List.length filtered in
    match cfg.max_collection_items with
    | None -> (filtered, None)
    | Some n when total <= n -> (filtered, None)
    | Some n ->
      let displayed = List.take n filtered in
      let truncated = List.drop n filtered in
      let truncated_breakdown =
        List.fold_left (fun (acc : change_breakdown) (e : item) ->
            match e.change with
            | Added -> { acc with added = acc.added + 1 }
            | Removed -> { acc with removed = acc.removed + 1 }
            | Modified -> { acc with modified = acc.modified + 1 }
            | Unchanged -> acc
          ) { added = 0; removed = 0; modified = 0 } truncated
      in
      (displayed, Some { total; displayed = n; truncated_breakdown })

(* Helper to filter and limit collection elements (backward compatible) *)
let filter_collection_elements
    (cfg : detail_config)
    (col : collection)
  : item list =
  fst (filter_collection_elements_with_info cfg col)

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
  (* Filter views that will render based on detail levels *)
  let sub_views = List.filter (fun v ->
      match v with
      | Field f -> should_render_level (get_effective_detail cfg f.change f.domain_type)
      | Item e -> should_render_level (get_effective_detail cfg e.change e.domain_type)
      | Collection c ->
        let col_level = get_effective_detail cfg c.change c.domain_type in
        should_render_level col_level &&
        (filter_collection_elements cfg c) <> []
    ) section.children
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
  (* Filter views that will render based on detail levels *)
  let sub_views = List.filter (fun v ->
      match v with
      | Field f -> should_render_level (get_effective_detail cfg f.change f.domain_type)
      | Item e -> should_render_level (get_effective_detail cfg e.change e.domain_type)
      | Collection c ->
        let col_level = get_effective_detail cfg c.change c.domain_type in
        should_render_level col_level &&
        (filter_collection_elements cfg c) <> []
    ) section.children
  in
  (* Count by change type using helper *)
  List.fold_left (fun (acc : change_breakdown) v ->
      match v with
      | Field f -> increment_breakdown acc f.change
      | Item e -> increment_breakdown acc e.change
      | Collection c -> increment_breakdown acc c.change
    ) ({ added = 0; removed = 0; modified = 0 } : change_breakdown) sub_views

(* Preset configurations for common use cases *)

(* Compact preset: structure overview with change counts.
   Shows first-level children for key types without expanding field details.
   Uses per-change differentiation for tracks: new tracks show more detail than removed ones. *)
let compact = {
  (* Base: Summary level shows change counts without field details *)
  added = Summary;
  removed = Summary;
  modified = Summary;
  unchanged = Ignore;

  (* Type-specific overrides for structure visibility *)
  type_overrides = [
    (* Tracks: per-change differentiation
       - Added tracks: Inline to show key properties at a glance
       - Removed tracks: Summary (just name + change indicator)
       - Modified tracks: Compact to show sections (Clips, Mixer, Devices) *)
    { domain_type = DTTrack; override = override
                                 ~added:(Some Inline)
                                 ~removed:(Some Summary)
                                 ~modified:(Some Compact)
                                 ();
    };

    (* Clips: show clip structure (Loop, Notes, TimeSignature) *)
    { domain_type = DTClip; override = uniform_override Compact; };

    (* Devices: show device structure (Parameters, Preset) *)
    { domain_type = DTDevice; override = uniform_override Compact; };

    (* LiveSet: show top-level structure (Tracks, Locators) *)
    { domain_type = DTLiveset; override = uniform_override Compact; };

    (* Notes: Summary to avoid flooding output with individual note changes *)
    { domain_type = DTNote; override = uniform_override Summary; };

    (* Events: Summary for automation event collections *)
    { domain_type = DTEvent; override = uniform_override Summary; };

    (* Automation: Summary for envelope structure *)
    { domain_type = DTAutomation; override = uniform_override Summary; };
  ];

  (* Limit output - more than quiet (10) but bounded to prevent overwhelming diffs *)
  max_collection_items = Some 20;

  (* Standard prefixes - using defaults *)
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "=";

  (* Sharp note names - using default *)
  note_name_style = Sharp;

  (* Indentation width - using default *)
  indent_width = 2;
}

(* Full preset: show all details for every changed item.
   Useful for thorough code review or debugging diff output.
   Safety limit of 100 items prevents overwhelming output from large note/event collections. *)
let full = {
  (* Base: Full detail for all change types *)
  added = Full;
  removed = Full;
  modified = Full;
  unchanged = Ignore;

  (* No type-specific overrides - treat all types equally *)
  type_overrides = [];

  (* Safety limit: prevent overwhelming output from large MIDI/automation collections *)
  max_collection_items = Some 100;

  (* Standard prefixes - using defaults *)
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "=";

  (* Sharp note names - using default *)
  note_name_style = Sharp;

  (* Indentation width - using default *)
  indent_width = 2;
}

(* Inline preset: show all fields on a single line with item name.
   Compact yet informative - good for quick scanning of changes.
   Notes and parameters are naturally inline-friendly. *)
let inline = {
  (* Base: Inline shows name + all fields on one line *)
  added = Inline;
  removed = Inline;
  modified = Inline;
  unchanged = Ignore;

  (* Type-specific overrides for inline-friendly types *)
  type_overrides = [
    (* Notes: Inline is perfect for MIDI notes (pitch, velocity, duration) *)
    { domain_type = DTNote; override = uniform_override Inline; };

    (* Parameters: Inline shows name + value cleanly *)
    { domain_type = DTParam; override = uniform_override Inline; };

    (* Automation Events: Inline shows time + value *)
    { domain_type = DTEvent; override = uniform_override Inline; };
  ];

  (* Moderate limit - inline format is compact so we can show more *)
  max_collection_items = Some 30;

  (* Standard prefixes - using defaults *)
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "=";

  (* Sharp note names - using default *)
  note_name_style = Sharp;

  (* Indentation width - using default *)
  indent_width = 2;
}

(* Quiet preset: minimal output for quick overview.
   Shows only top-level change counts with basic structure.
   Good for CI/CD pipelines or quick "did anything change?" checks. *)
let quiet = {
  (* Base: Summary shows change counts only *)
  added = Summary;
  removed = Summary;
  modified = Summary;
  unchanged = Ignore;

  (* Only LiveSet gets Compact to show top-level sections *)
  type_overrides = [
    { domain_type = DTLiveset; override = uniform_override Compact; }
  ];

  (* Low limit for minimal output *)
  max_collection_items = Some 0;

  (* Standard prefixes - using defaults *)
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "=";

  (* Sharp note names - using default *)
  note_name_style = Sharp;

  (* Indentation width - using default *)
  indent_width = 2;
}

(* Verbose preset: show everything including unchanged items.
   Useful for debugging, auditing, or understanding complete file structure.
   No limits - displays all items and all fields. *)
let verbose = {
  (* Base: Full detail for everything, including unchanged *)
  added = Full;
  removed = Full;
  modified = Full;
  unchanged = Full;

  (* No type-specific overrides - treat all types equally *)
  type_overrides = [];

  (* No limit - show everything *)
  max_collection_items = None;

  (* Standard prefixes - using defaults *)
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "=";

  (* Sharp note names - using default *)
  note_name_style = Sharp;

  (* Indentation width - using default *)
  indent_width = 2;
}

(* Mixing preset: optimized for stem track mixing workflows.
   Focuses on mixer settings, automation, devices, and routing.
   Clips shown as summary only (stem tracks don't modify clip content).
   MIDI notes ignored (irrelevant for mixing rendered stems). *)
let mixing = {
  (* Base: Summary for overview, highlighting important changes *)
  added = Summary;
  removed = Summary;
  modified = Summary;
  unchanged = Ignore;

  (* Type-specific overrides for mixing workflow *)
  type_overrides = [
    (* === Core mixing types (Full detail) === *)

    (* Automation: full details - automation is critical for mixing adjustments *)
    { domain_type = DTAutomation; override = uniform_override Full; };

    (* Devices: full details - plugin/effect changes are important *)
    { domain_type = DTDevice; override = uniform_override Full; };

    (* Mixer: full details - volume/pan/mute/solo are core mixing concerns *)
    { domain_type = DTMixer; override = uniform_override Full; };

    (* Parameters: inline - device parameter name + value shown concisely *)
    { domain_type = DTParam; override = uniform_override Inline; };

    (* SampleRef: full details - audio file changes are critical for mixing *)
    { domain_type = DTSampleRef; override = uniform_override Full; };

    (* === Secondary mixing types (Summary/Inline) === *)

    (* Routing: Inline to show Type and Target concisely *)
    { domain_type = DTRouting; override = uniform_override Inline; };

    (* Sends: Inline to show send target and level concisely *)
    { domain_type = DTSend; override = uniform_override Inline; };

    (* Clips: Ignore - stem tracks don't modify clip content/position *)
    { domain_type = DTClip; override = uniform_override Ignore; };

    (* Loop: Ignore - loop settings not relevant for mixing *)
    { domain_type = DTLoop; override = uniform_override Ignore; };

    (* Events: Inline - show automation events concisely (time + value) *)
    { domain_type = DTEvent; override = uniform_override Inline; };

    (* === Structural types === *)

    (* Liveset: Compact structure overview *)
    { domain_type = DTLiveset; override = uniform_override Compact; };

    (* Tracks: Compact structure with sections *)
    { domain_type = DTTrack; override = uniform_override Compact; };

    (* === Ignored types (not relevant for mixing) === *)

    (* Notes: MIDI notes are irrelevant when mixing rendered stems *)
    { domain_type = DTNote; override = uniform_override Ignore; };
  ];

  (* Higher limit for automation-heavy workflows *)
  max_collection_items = Some 50;

  (* Standard prefixes - using defaults *)
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "=";

  (* Sharp note names (though notes are ignored in this preset) - using default *)
  note_name_style = Sharp;

  (* Indentation width - using default *)
  indent_width = 2;
}

(* Composer preset: focused on MIDI composition and sample arrangement.
   Shows clips, MIDI notes, loop settings, time signatures, and audio samples.
   Hides all mixing/engineering elements (devices, automation, routing, mixer). *)
let composer = {
  (* Base: show summary of structural changes *)
  added = Summary;
  removed = Summary;
  modified = Summary;
  unchanged = Ignore;

  (* Type-specific overrides for composition workflow *)
  type_overrides = [
    (* === Composition-critical types (Full detail) === *)

    (* Clips: full details - the core compositional element *)
    { domain_type = DTClip; override = uniform_override Full; };

    (* Notes: inline - MIDI note content displayed concisely (pitch, velocity, duration) *)
    { domain_type = DTNote; override = uniform_override Inline; };

    (* Loop: inline - loop boundaries shown concisely (start, end, enabled) *)
    { domain_type = DTLoop; override = uniform_override Inline; };

    (* Signature: inline - time signature shown concisely (numerator/denominator) *)
    { domain_type = DTSignature; override = uniform_override Inline; };

    (* SampleRef: full details - audio sample references matter for arrangement *)
    { domain_type = DTSampleRef; override = uniform_override Full; };

    (* === Structural types (Compact) === *)

    (* Tracks: show structure with clip sections visible *)
    { domain_type = DTTrack; override = uniform_override Compact; };

    (* Liveset: show top-level structure *)
    { domain_type = DTLiveset; override = uniform_override Compact; };

    (* === Mixing/Engineering types (Ignored) === *)

    (* Mixer: not relevant for composition *)
    { domain_type = DTMixer; override = uniform_override Ignore; };

    (* Device: effects/instruments config not relevant *)
    { domain_type = DTDevice; override = uniform_override Ignore; };

    (* Automation: mixing automation not relevant *)
    { domain_type = DTAutomation; override = uniform_override Ignore; };

    (* Event: automation events not relevant for composition *)
    { domain_type = DTEvent; override = uniform_override Ignore; };

    (* Routing: I/O routing not relevant *)
    { domain_type = DTRouting; override = uniform_override Ignore; };

    (* === Other hidden types === *)

    { domain_type = DTLocator; override = uniform_override Ignore; };
    { domain_type = DTParam; override = uniform_override Ignore; };
    { domain_type = DTSend; override = uniform_override Ignore; };
    { domain_type = DTPreset; override = uniform_override Ignore; };
    { domain_type = DTMacro; override = uniform_override Ignore; };
    { domain_type = DTSnapshot; override = uniform_override Ignore; };
    { domain_type = DTVersion; override = uniform_override Ignore; };
    { domain_type = DTOther; override = uniform_override Ignore; };
  ];

  (* No limit - composers need to see all notes/clips in their arrangements *)
  max_collection_items = None;

  (* Standard prefixes - using defaults *)
  prefix_added = "+";
  prefix_removed = "-";
  prefix_modified = "*";
  prefix_unchanged = "=";

  (* Sharp note names (standard for DAW MIDI editing) - using default *)
  note_name_style = Sharp;

  (* Indentation width - using default *)
  indent_width = 2;
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
  let existing = match List.find_opt (fun e -> e.domain_type = dt) cfg.type_overrides with
    | Some entry -> entry.override
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
  let filtered = List.filter (fun e -> e.domain_type <> dt) cfg.type_overrides in
  { cfg with type_overrides = { domain_type = dt; override = new_override } :: filtered }

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
  let override_warnings = cfg.type_overrides
    |> List.filter_map (fun (entry : type_override_entry) ->
        if entry.override.added = None && entry.override.removed = None && entry.override.modified = None && entry.override.unchanged = None then
          Some (Printf.sprintf "Type override for %s has all None values (no effect)"
                  (domain_type_to_string entry.domain_type))
        else
          None) in
  let indent_warnings =
    if cfg.indent_width < 0 then
      [Printf.sprintf "indent_width must be >= 0, got %d" cfg.indent_width]
    else []
  in
  let max_collection_warnings =
    match cfg.max_collection_items with
    | Some n when n < 0 ->  [Printf.sprintf "max_collection_items must be >= 0, got %d" n]
    | _ -> []
  in
  override_warnings @ indent_warnings @ max_collection_warnings

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

(* ==================== JSON Schema Generation ==================== *)

(** Generate JSON schema for detail_config as Yojson.Basic.t
    Uses $defs for domain_type, detail_level, per_change_override,
    and note_display_style for better readability *)
let detail_config_json_schema () : Yojson.Basic.t =
  let base_schema = Ppx_deriving_jsonschema_runtime.json_schema
      ~title:"alsdiff configuration schema"
      ~description:"Configuration schema for alsdiff output rendering"
      ~definitions:[
        ("domain_type", View_model.domain_type_jsonschema);
        ("detail_level", detail_level_jsonschema);
        ("per_change_override", per_change_override_jsonschema);
        ("type_override_entry", type_override_entry_jsonschema);
        ("note_display_style", View_model.note_display_style_jsonschema);
      ]
      detail_config_jsonschema
  in
  (* Fields with defaults that should be optional in the JSON schema *)
  let optional_fields_with_defaults = [
    "prefix_added";
    "prefix_removed";
    "prefix_modified";
    "prefix_unchanged";
    "note_name_style";
    "indent_width";
  ] in
  (* Add $id to schema root and $schema property for config files *)
  let schema_uri = "https://raw.githubusercontent.com/krfantasy/alsdiff/master/docs/config.schema.json" in
  match base_schema with
  | `Assoc fields ->
    let fields_with_id = ("$id", `String schema_uri) :: fields in
    (match List.assoc_opt "properties" fields_with_id with
     | Some (`Assoc props) ->
       (* Add $schema as an optional property for config files *)
       let with_schema_prop = ("$schema", `Assoc [
           ("type", `String "string");
           ("description", `String "JSON Schema URI pointing to the schema for this config file.");
         ]) :: props in
       (* Filter out defaulted fields from required array *)
       let updated_fields = List.map (fun (k, v) ->
           if k = "properties" then ("properties", `Assoc with_schema_prop)
           else if k = "required" then (
             match v with
             | `List required_fields ->
               (* Remove fields with defaults from required array *)
               let filtered = List.filter (fun (field : Yojson.Basic.t) ->
                   match field with
                   | `String s -> not (List.mem s optional_fields_with_defaults)
                   | _ -> true
                 ) required_fields in
               ("required", `List filtered)
             | _ -> (k, v)
           )
           else (k, v)
         ) fields_with_id in
       `Assoc updated_fields
     | _ -> `Assoc fields_with_id)
  | _ -> base_schema

(** Generate JSON schema as a formatted string *)
let detail_config_schema_to_string () : string =
  (* Use base schema directly without nullable post-processing.
     The ppx_deriving_jsonschema library handles option types by omitting them
     from the required array. Optional fields use [@default None] which causes
     PPX to omit them when encoding (not emit null) and accept both missing and
     explicit null when decoding. *)
  Yojson.Basic.pretty_to_string (detail_config_json_schema ())

(** Write JSON schema to a file *)
let write_schema_to_file (path : string) : unit =
  let schema = detail_config_schema_to_string () in
  Out_channel.with_open_text path (fun oc ->
      output_string oc schema;
      output_string oc "\n"
    )

(* ==================== JSON Schema Validation ==================== *)

(** Cached compiled validator for performance.
    Compiled once on first use, then reused. *)
let cached_validator : Jsonschema.validator option ref = ref None

(** Get or create the JSON schema validator.
    The validator is compiled once and cached for subsequent calls.
    @raise Failure if schema compilation fails (should never happen with our generated schema) *)
let get_config_validator () : Jsonschema.validator =
  match !cached_validator with
  | Some v -> v
  | None ->
    let schema = detail_config_json_schema () in
    match Jsonschema.create_validator_from_json ~schema () with
    | Ok v ->
      cached_validator := Some v;
      v
    | Error err ->
      (* This should never happen since our schema is generated correctly *)
      let msg = Format.asprintf "Internal error: invalid schema: %a"
          Jsonschema.pp_compile_error err in
      failwith msg

(** Validation error type with detailed information *)
type validation_error = {
  message: string;
  path: string option;
  details: string;
}

(** Convert Jsonschema validation error to our error type *)
let validation_error_of_jsonschema (err : Jsonschema.validation_error) : validation_error =
  let details = Jsonschema.Validation_error.to_string err in
  { message = "JSON schema validation failed";
    path = None;  (* Could extract from details if needed *)
    details }

(** Validate a JSON value against the detail_config schema.
    @param json The JSON value to validate (Yojson.Basic.t)
    @return Ok () if valid, Error with detailed message if invalid *)
let validate_config_json (json : Yojson.Basic.t) : (unit, validation_error) result =
  let validator = get_config_validator () in
  match Jsonschema.validate validator json with
  | Ok () -> Ok ()
  | Error err -> Error (validation_error_of_jsonschema err)

(** Validate a JSON string against the detail_config schema.
    @param json_str The JSON string to validate
    @return Ok () if valid, Error with message if invalid (including parse errors) *)
let validate_config_string (json_str : string) : (unit, string) result =
  try
    let json = Yojson.Basic.from_string json_str in
    match validate_config_json json with
    | Ok () -> Ok ()
    | Error err -> Error err.details
  with
  | Yojson.Json_error msg -> Error ("JSON parse error: " ^ msg)

(** Filter out JSON schema metadata fields that are not part of detail_config.
    These fields ($schema, $id, $defs) are valid in config files for schema
    validation but should be removed before PPX parsing. *)
let filter_schema_metadata_fields (json : Yojson.Basic.t) : Yojson.Basic.t =
  match json with
  | `Assoc fields ->
    let known_fields = List.filter (fun (key, _) ->
        key <> "$schema" && key <> "$id" && key <> "$defs"
      ) fields in
    `Assoc known_fields
  | other -> other

(** Validate a config file against the detail_config schema.
    @param file_path Path to the JSON config file
    @return Ok () if valid, Error with detailed message if invalid *)
let validate_config_file (file_path : string) : (unit, string) result =
  try
    (* Step 1: Parse JSON for schema validation *)
    let json = Yojson.Basic.from_file file_path in

    (* Step 2: Validate against JSON schema *)
    match validate_config_json json with
    | Error err -> Error (Printf.sprintf "Validation failed for %s:\n%s" file_path err.details)
    | Ok () ->
      (* Step 3: Parse config for business logic validation *)
      let json_str = Yojson.Basic.to_string (filter_schema_metadata_fields json) in
      match detail_config_of_yojson (Yojson.Safe.from_string json_str) with
      | Error msg -> Error (Printf.sprintf "Step 3. Config parsing failed in %s: %s" file_path msg)
      | Ok cfg ->
        (* Step 4: Run business logic validation *)
        let warnings = validate_config cfg in
        if warnings = [] then Ok ()
        else Error (Printf.sprintf "Step 4. Config validation warnings for %s:\n%s"
                      file_path (String.concat "\n" warnings))
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error in %s: %s" file_path msg)
  | Sys_error msg -> Error (Printf.sprintf "File error: %s" msg)

(** Load and validate a config file, returning the parsed config or an error.
    This combines validation and parsing in one step.
    @param file_path Path to the JSON config file
    @return Ok detail_config if valid and parsed, Error with message otherwise *)
let load_and_validate_config (file_path : string) : (detail_config, string) result =
  try
    (* Read file content once *)
    let json_str = In_channel.with_open_text file_path In_channel.input_all in

    (* Parse as Basic for validation *)
    let json_basic = Yojson.Basic.from_string json_str in

    (* Validate against schema *)
    match validate_config_json json_basic with
    | Error err ->
      Error (Printf.sprintf "Config validation failed in %s:\n%s" file_path err.details)
    | Ok () ->
      (* Filter out schema metadata fields before PPX parsing *)
      let filtered_str = Yojson.Basic.to_string (filter_schema_metadata_fields json_basic) in
      (* Parse as Safe for yojson deserialization *)
      let json_safe = Yojson.Safe.from_string filtered_str in
      match detail_config_of_yojson json_safe with
      | Ok cfg -> Ok cfg
      | Error msg -> Error (Printf.sprintf "Config parsing failed in %s: %s" file_path msg)
  with
  | Yojson.Json_error msg ->
    Error (Printf.sprintf "JSON error in %s: %s" file_path msg)
  | Sys_error msg ->
    Error (Printf.sprintf "File error: %s" msg)

(** Helper function for backward compatibility with configs that don't have indent_width *)
let detail_config_of_yojson_with_default json =
  (* Try normal parsing first *)
  match detail_config_of_yojson json with
  | Ok cfg -> Ok cfg
  | Error _ -> (
      (* If that fails, check if it's a config with only indent_width and merge with preset *)
      let json_basic = Yojson.Safe.to_basic json in
      match json_basic with
      | `Assoc fields ->
        (* Check if this is a config with only indent_width field *)
        if List.length fields = 1 && List.mem_assoc "indent_width" fields then begin
          (* Get the indent_width value and merge with full preset *)
          match List.assoc "indent_width" fields with
          | `Int indent ->
            (* Create a config from full preset with custom indent_width *)
            Ok { full with indent_width = indent }
          | _ -> Error "indent_width must be an integer"
        end else if not (List.mem_assoc "indent_width" fields) then
          (* Add default indent_width and retry *)
          let json_with_default = `Assoc (("indent_width", `Int 2) :: fields) in
          (* Convert back to Safe via string *)
          let json_str = Yojson.Basic.pretty_to_string json_with_default in
          let json_safe = Yojson.Safe.from_string json_str in
          detail_config_of_yojson json_safe
        else
          Error "Failed to parse detail_config"
      | _ -> Error "Failed to parse detail_config"
    )

(* ==================== JSON Export with Schema Metadata ==================== *)

(** Convert detail_config to Yojson with $schema field.
    This is the preferred function for exporting config JSON.
    Optional fields with None values are omitted by PPX via [@default None]. *)
let detail_config_to_yojson_with_schema (cfg : detail_config) : Yojson.Safe.t =
  let base_json = detail_config_to_yojson cfg in
  match base_json with
  | `Assoc fields ->
    let schema_uri = "https://raw.githubusercontent.com/krfantasy/alsdiff/master/docs/config.schema.json" in
    let with_schema = ("$schema", `String schema_uri) :: fields in
    `Assoc with_schema
  | _ -> base_json

(** Convert detail_config to JSON string with $schema and version fields. *)
let detail_config_to_string_with_schema (cfg : detail_config) : string =
  detail_config_to_yojson_with_schema cfg
  |> Yojson.Safe.pretty_to_string
