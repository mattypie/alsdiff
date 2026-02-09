open View_model
open Config
(* Config types and functions are re-exported at the end of this file for backward compatibility *)

(* Field value formatting *)
let pp_field_value fmt = function
  | Fint i -> Fmt.pf fmt "%d" i
  | Ffloat f -> Fmt.pf fmt "%.2f" f
  | Fbool b -> Fmt.pf fmt "%b" b
  | Fstring s -> Fmt.pf fmt "%s" s

(* Field view rendering *)
let pp_field cfg fmt (field : field) =
  (* Always render if called - filtering happens at parent level *)
  Fmt.pf fmt "%a %s: " (pp_change_type cfg) field.change field.name;
  match field.oldval, field.newval with
  | Some old_v, Some new_v ->
    Fmt.pf fmt "%a -> %a" pp_field_value old_v pp_field_value new_v
  | Some old_v, None ->
    Fmt.pf fmt "%a" pp_field_value old_v
  | None, Some new_v ->
    Fmt.pf fmt "%a" pp_field_value new_v
  | None, None -> ()

(* Inline field formatting: "Name: old -> new" without change prefix *)
let pp_field_inline _cfg fmt (field : field) =
  Fmt.pf fmt "%s: " field.name;
  match field.oldval, field.newval with
  | Some old_v, Some new_v ->
    Fmt.pf fmt "%a -> %a" pp_field_value old_v pp_field_value new_v
  | Some old_v, None ->
    Fmt.pf fmt "%a" pp_field_value old_v
  | None, Some new_v ->
    Fmt.pf fmt "%a" pp_field_value new_v
  | None, None -> ()

(* Format truncation message: "... and N more (breakdown)" *)
let pp_truncation_info _cfg fmt (info : truncation_info) =
  let truncated_count = info.total - info.displayed in
  let breakdown_str = format_breakdown info.truncated_breakdown in
  if breakdown_str = "" then
    Fmt.pf fmt "... and %d more" truncated_count
  else
    Fmt.pf fmt "... and %d more %s" truncated_count breakdown_str

(* Element view rendering *)
let rec pp_item cfg fmt (elem : item) =
  let level = get_effective_detail cfg elem.change elem.domain_type in
  if not (should_render_level level) then ()
  else if level = Summary then
    (* Summary mode: name + change symbol + counts *)
    render_summary_breakdown cfg fmt (count_fields_breakdown elem) elem.name elem.change
  else if level = Compact then
    (* Compact mode: name + change symbol only *)
    Fmt.(box (fun fmt () -> pf fmt "%a %s" (pp_change_type cfg) elem.change elem.name)) fmt ()
  else if level = Inline then begin
    (* Inline mode: name + fields in brackets, nested items on new lines *)
    let fields = List.filter_map (fun (v : view) ->
        match v with Field f -> Some f | _ -> None
      ) elem.children in
    let nested = List.filter (fun (v : view) ->
        match v with Item _ | Collection _ -> true | _ -> false
      ) elem.children in
    Fmt.(vbox ~indent:cfg.indent_width (fun fmt () ->
        pf fmt "%a %s" (pp_change_type cfg) elem.change elem.name;
        (* Only show brackets if there are fields *)
        if fields <> [] then begin
          pf fmt " [";
          list ~sep:(any ", ") (pp_field_inline cfg) fmt fields;
          pf fmt "]"
        end;
        (* Render nested items on new lines *)
        List.iter (fun v ->
            Fmt.cut fmt ();
            match v with
            | Item e -> pp_item cfg fmt e
            | Collection c -> pp_collection cfg fmt c
            | _ -> ()
          ) nested
      )) fmt ()
  end
  else begin
    (* Full mode: name + symbol + fields (multiline) *)
    Fmt.(vbox ~indent:cfg.indent_width (fun fmt () ->
        pf fmt "%a %s" (pp_change_type cfg) elem.change elem.name;
        if should_show_fields cfg elem then (
          Fmt.cut fmt ();
          (* Render Field children *)
          let fields = List.filter_map (fun (v : view) ->
              match v with
              | Field f -> Some f
              | _ -> None
            ) elem.children in
          list ~sep:(fun fmt () -> Fmt.cut fmt ()) (pp_field cfg) fmt fields;
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
        )
      )) fmt ()
  end

(* Collection view rendering *)
and pp_collection cfg fmt (col : collection) =
  let level = get_effective_detail cfg col.change col.domain_type in
  if not (should_render_level level) then ()
  else if level = Summary then
    (* Summary mode: always show counts, even if elements are filtered out *)
    render_summary_breakdown cfg fmt (count_elements_breakdown cfg col) col.name col.change
  else
    let elements, truncation = filter_collection_elements_with_info cfg col in
    if elements = [] then ()
    else if level = Compact then
      (* Compact mode: name + symbol only *)
      Fmt.(box (fun fmt () -> pf fmt "%a %s" (pp_change_type cfg) col.change col.name)) fmt ()
    else begin
      (* Inline and Full: show collection name, elements on new lines *)
      Fmt.(vbox ~indent:cfg.indent_width (fun fmt () ->
          pf fmt "%a %s" (pp_change_type cfg) col.change col.name;
          List.iter (fun e ->
              Fmt.cut fmt ();
              pp_view cfg fmt (Item e)
            ) elements;
          (* Show truncation message if items were hidden *)
          match truncation with
          | Some info -> Fmt.cut fmt (); pp_truncation_info cfg fmt info
          | None -> ()
        )) fmt ()
    end

(* Section view rendering *)
and pp_section cfg fmt (section : item) =
  let level = get_effective_detail cfg section.change section.domain_type in
  if not (should_render_level level) then ()
  else
    (* Filter sub_views based on detail levels *)
    let sub_views = List.filter (fun v ->
        match v with
        | Field f -> should_render_level (get_effective_detail cfg f.change f.domain_type)
        | Item e -> should_render_level (get_effective_detail cfg e.change e.domain_type)
        | Collection c ->
          let col_level = get_effective_detail cfg c.change c.domain_type in
          if not (should_render_level col_level) then false
          else if col_level = Summary then
            (* Summary mode: always include collections, they show counts even without elements *)
            true
          else
            (* Other modes: check if any elements would render *)
            (filter_collection_elements cfg c) <> []
      ) section.children in
    (* Render section header *)
    if level = Summary then begin
      (* Don't show count for LiveSet - it shows sub-views in Summary mode *)
      if section.domain_type <> DTLiveset then
        render_summary_breakdown cfg fmt (count_sub_views_breakdown cfg section) section.name section.change
      else begin
        Fmt.(vbox ~indent:cfg.indent_width (fun fmt () ->
            pf fmt "%a %s" (pp_change_type cfg) section.change section.name;
            List.iter (fun view ->
                Fmt.cut fmt ();
                pp_view cfg fmt view
              ) sub_views
          )) fmt ()
      end
    end
    else if level = Inline then begin
      (* Inline mode: fields inline with header, nested items on new lines *)
      let fields = List.filter_map (fun v -> match v with Field f -> Some f | _ -> None) sub_views in
      let nested = List.filter (fun v -> match v with Item _ | Collection _ -> true | _ -> false) sub_views in
      Fmt.(vbox ~indent:cfg.indent_width (fun fmt () ->
          pf fmt "%a %s" (pp_change_type cfg) section.change section.name;
          (* Only show brackets if there are fields *)
          if fields <> [] then begin
            pf fmt " [";
            list ~sep:(any ", ") (pp_field_inline cfg) fmt fields;
            pf fmt "]"
          end;
          (* Render nested items on new lines *)
          List.iter (fun view ->
              Fmt.cut fmt ();
              pp_view cfg fmt view
            ) nested
        )) fmt ()
    end
    else begin
      (* Compact and Full modes *)
      Fmt.(vbox ~indent:cfg.indent_width (fun fmt () ->
          pf fmt "%a %s" (pp_change_type cfg) section.change section.name;
          let views_to_render = match level with
            | Compact ->
              (* Compact mode: only show Item and Collection subviews, no Fields *)
              List.filter (fun v ->
                  match v with
                  | Item _ | Collection _ -> true
                  | Field _ -> false
                ) sub_views
            | Full | Summary | Inline | Ignore -> sub_views
          in
          List.iter (fun view ->
              Fmt.cut fmt ();
              pp_view cfg fmt view
            ) views_to_render
        )) fmt ()
    end

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

(* ==================== Backward Compatibility Re-exports ==================== *)
(* Include all Config types and functions for backward compatibility.
   This ensures that code referencing Text_renderer.Ignore, Text_renderer.detail_config_of_yojson,
   etc. continues to work. *)
include Config
