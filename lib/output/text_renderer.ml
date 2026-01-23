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
  let field_indent = String.make cfg.indent_width ' ' in
  Fmt.pf fmt "%s%a %s: " field_indent (pp_change_type cfg) field.change field.name;
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

(* Element view rendering *)
let rec pp_item cfg fmt (elem : item) =
  let level = get_effective_detail cfg elem.change elem.domain_type in
  if not (should_render_level level) then ()
  else if level = Summary then
    (* Summary mode: name + change symbol + counts *)
    render_summary_breakdown cfg fmt (count_fields_breakdown elem) elem.name elem.change
  else if level = Compact then
    (* Compact mode: name + change symbol only *)
    Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) elem.change elem.name
  else if level = Inline then begin
    (* Inline mode: name + fields in brackets, nested items on new lines *)
    let fields = List.filter_map (fun (v : view) ->
        match v with Field f -> Some f | _ -> None
      ) elem.children in
    let nested = List.filter (fun (v : view) ->
        match v with Item _ | Collection _ -> true | _ -> false
      ) elem.children in
    Fmt.(vbox ~indent:0 (fun fmt () ->
        pf fmt "%a %s" (pp_change_type cfg) elem.change elem.name;
        (* Only show brackets if there are fields *)
        if fields <> [] then begin
          pf fmt " [";
          list ~sep:(any ", ") (pp_field_inline cfg) fmt fields;
          pf fmt "]"
        end;
        (* Render nested items on new lines *)
        List.iter (fun v ->
            pf fmt "@\n";
            match v with
            | Item e -> pp_item cfg fmt e
            | Collection c -> pp_collection cfg fmt c
            | _ -> ()
          ) nested
      )) fmt ()
  end
  else begin
    (* Full mode: name + symbol + fields (multiline) *)
    Fmt.(vbox ~indent:0 (fun fmt () ->
        pf fmt "%a %s" (pp_change_type cfg) elem.change elem.name;
        if should_show_fields cfg elem then (
          pf fmt "@\n";
          (* Render Field children *)
          let fields = List.filter_map (fun (v : view) ->
              match v with
              | Field f -> Some f
              | _ -> None
            ) elem.children in
          list ~sep:(fun fmt () -> pf fmt "@\n") (pp_field cfg) fmt fields;
          (* Render Item and Collection children *)
          let nested = List.filter (fun (v : view) ->
              match v with
              | Item _ | Collection _ -> true
              | _ -> false
            ) elem.children in
          List.iter (fun v ->
              pf fmt "@\n";
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
  else
    let elements = filter_collection_elements cfg col in
    if elements = [] then ()
    else if level = Summary then
      (* Summary mode: name + change symbol + counts *)
      render_summary_breakdown cfg fmt (count_elements_breakdown cfg col) col.name col.change
    else if level = Compact then
      (* Compact mode: name + symbol only *)
      Fmt.pf fmt "@[%a %s@]" (pp_change_type cfg) col.change col.name
    else begin
      (* Inline and Full: show collection name, elements on new lines *)
      Fmt.(vbox ~indent:cfg.indent_width (fun fmt () ->
          pf fmt "%a %s" (pp_change_type cfg) col.change col.name;
          List.iter (fun e ->
              pf fmt "@\n";
              pp_item cfg fmt e
            ) elements
        )) fmt ()
    end

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
    if level = Summary then begin
      (* Don't show count for LiveSet - it shows sub-views in Summary mode *)
      if section.domain_type <> DTLiveset then
        render_summary_breakdown cfg fmt (count_sub_views_breakdown cfg section) section.name section.change
      else begin
        Fmt.(vbox ~indent:cfg.indent_width (fun fmt () ->
            pf fmt "%a %s" (pp_change_type cfg) section.change section.name;
            List.iter (fun view ->
                pf fmt "@\n";
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
              pf fmt "@\n";
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
            | Full | Summary | Inline | DLNone -> sub_views
          in
          List.iter (fun view ->
              pf fmt "@\n";
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
   This ensures that code referencing Text_renderer.DLNone, Text_renderer.detail_config_of_yojson,
   etc. continues to work. *)
include Config
