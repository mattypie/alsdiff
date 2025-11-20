(** μpath: A tiny expression language for querying in XML documents, with a XPath-like syntax.

    Supports:
    - Tag names with optional attributes (e.g., `tag`, `tag@attr="value"`, `tag@attr=*`)
    - Regex tag name matching with single quotes (e.g., `'MacroControls\.[0-9]+'`, `'prefix.*suffix$'`)
    - Indexing with optional Tag name (e.g., `[0]`, `[1]`, `tag[3]`)
    - Single wildcard (`*`) for single level matching
    - Multi wildcard (`**`) for multiple levels matching, like XPath's '//'
    - Wildcards with optional attributes (e.g., `*@attr="value"`, `**@attr`)
    - Current node navigation (`.`) for selecting the current node
    - Parent node navigation (`..`) for selecting the parent node
    - Paths separated by `/` (e.g., `/tag1/tag2@attr="value"/*/tag3/**/tag4`)
    - Combined navigation (e.g., `/a/b/c/.`, `/a/b/c/..`, `/a/b/c/../..`)

    Does not support:
    - Functions
    - Complex predicates
    - Namespaces
    - Other XPath features

    Regex Syntax:
    - Regex patterns are enclosed in single quotes to distinguish from raw string matching
    - Supports full PCRE regex syntax including quantifiers ( *, +, ? ), character classes ([0-9]), etc.
    - Examples:
      - `'MacroControls\.[0-3]+'` - matches MacroControls.0, MacroControls.1, MacroControls.2, MacroControls.3
      - `'prefix.*suffix$'` - matches tags starting with "prefix" and ending with "suffix"
      - `'element-[0-9]{2}'` - matches element-01, element-02, etc.
    - Use `$` anchor to match complete tag names and avoid partial matches

    Example usage:
    let result = parse_path "/bookstore/book@category=\"cooking\"/title"
    let wildcard_with_attr = parse_path "/*@type=\"magic\""
    let regex_match = parse_path "/**/'MacroControls\.[0-9]'"  (* matches all MacroControls with single digits *)
    let regex_range = parse_path "/**/'MacroControls\.[1-5]'"  (* matches MacroControls.1 through MacroControls.5 *)
    let current_node = parse_path "/a/b/c/."  (* returns 'c' node *)
    let parent_node = parse_path "/a/b/c/.."  (* returns 'b' node *)
*)

type attribute_value =
  | Exact of string
  | Any

type attribute = {
  name : string;
  value : attribute_value;
} [@@deriving eq]

type name_component =
  | Raw of string               (* for raw string matching *)
  | Regex of string * Re.Pcre.regexp (* for regex matching, the first element in the tuple is the raw regex string, the second element is the compiled PCRE regex *)

type path_component =
  | Tag of name_component * attribute list
  | Index of int * name_component option
  | SingleWildcard of attribute list
  | MultiWildcard of attribute list
  | CurrentNode
  | ParentNode

type path = path_component list

(** Custom exceptions for better error reporting *)
exception Path_not_found of {
  full_path: string;
  matched_prefix: string list;
  failed_component: path_component;
  cause: string;
}

exception Index_out_of_bounds of {
  full_path: string;
  index: int;
  collection_length: int;
  component: path_component;
}

exception Invalid_path_component of {
  full_path: string;
  component: path_component;
  reason: string;
}


module Parser =
struct

  open Angstrom

  let is_identifier_char = function
    | '/' | '[' | ']' | '@' | '=' | '*' | '\'' -> false
    | _ -> true

  let identifier = take_while1 is_identifier_char <?> "identifier"

  (* Compile regex pattern, raising error if invalid *)
  let compile_regex pattern =
    try
      Re.Pcre.regexp pattern
    with
    | Re.Pcre.Parse_error | Re.Pcre.Not_supported -> failwith ("Invalid PCRE regex pattern '" ^ pattern ^ "'")

  (* Parse a single-quoted regex pattern *)
  let p_quoted_regex =
    let p_regex_content =
      let p_escaped = char '\\' *> any_char >>| String.make 1 in
      let p_unescaped = take_while1 (fun c -> c <> '\'' && c <> '\\') in
      many (p_escaped <|> p_unescaped) >>| String.concat ""
    in
    char '\'' *> p_regex_content <* char '\'' >>| fun pattern ->
      Regex (pattern, compile_regex pattern)

  (* Parse either a raw identifier or a quoted regex *)
  let p_name_component =
    choice [
      p_quoted_regex;
      identifier >>| fun name -> Raw name;
    ]

  let integer =
    take_while1 (function '0'..'9' -> true | _ -> false)
    >>| int_of_string
        <?> "integer"

  let p_quoted_string =
    let p_escaped = char '\\' *> any_char >>| String.make 1 in
    let p_unescaped = take_while1 (fun c -> c <> '"' && c <> '\\') in
    let p_content = many (p_escaped <|> p_unescaped) >>| String.concat "" in
    char '"' *> p_content <* char '"' <?> "quoted string"

  let p_unquoted_string =
    let p_escaped = char '\\' *> any_char >>| String.make 1 in
    let is_value_terminator = function
      | '@' | '/' | '[' | ']' -> true
      | _ -> false
    in
    let p_unescaped = take_while1 (fun c -> c <> '\\' && not (is_value_terminator c))
    in
    many1 (p_escaped <|> p_unescaped) >>| String.concat "" <?> "unquoted string"

  let p_attr_value =
    let p_wildcard_val = char '*' *> return Any in
    let p_quoted_val = p_quoted_string >>| fun s -> Exact s in
    let p_unquoted_val = p_unquoted_string >>| fun s -> Exact s in
    choice [
      p_wildcard_val;
      p_quoted_val;
      p_unquoted_val;
    ]

  let p_attribute =
    let p_key_value =
      lift2 (fun name value -> { name; value })
        (char '@' *> identifier <* char '=')
        p_attr_value
    in
    let p_key_only =
      char '@' *> identifier >>| fun name -> { name; value = Any }
    in
    p_key_value <|> p_key_only <?> "attribute"

  let p_component =
    let p_current_node = char '.' *> return CurrentNode in
    let p_parent_node = string ".." *> return ParentNode in
    let p_index =
      lift2 (fun tag index -> Index (index, tag))
        (option None (p_name_component >>| Option.some))
        (char '[' *> integer <* char ']')
    in
    let p_single_wildcard =
      lift2 (fun _ attrs -> SingleWildcard attrs)
        (char '*')
        (many p_attribute)
    in
    let p_multi_wildcard =
      lift2 (fun _ attrs -> MultiWildcard attrs)
        (string "**")
        (many p_attribute)
    in
    let p_tag =
      lift2 (fun name attrs -> Tag (name, attrs))
        p_name_component
        (many p_attribute)
    in
    choice [ p_parent_node;
             p_current_node;
             p_index;
             p_multi_wildcard;
             p_single_wildcard;
             p_tag ] <?> "path component"

  let path_parser =
    let optional_slash = option None (char '/' >>| fun _ -> Some ()) in
    optional_slash *> sep_by1 (char '/') p_component <?> "path"

  let parse_path s =
    parse_string ~consume:All path_parser s

end


(** the search engine *)
let match_attributes (tree : Xml.t) (pattrs : attribute list) : bool =
  match tree with
  | Xml.Element { attrs; _ } ->
    let match_attribute attrs k v =
      try
        let x = List.assoc k attrs in
        match v with
        | Exact v -> x = v
        | Any -> true
      with
      | Not_found -> false
    in
    List.for_all (fun { name; value } -> match_attribute attrs name value) pattrs
  | _ -> assert false


let match_name_component tag_name = function
  | Raw name -> tag_name = name
  | Regex (_, compiled_regex) -> Re.execp compiled_regex tag_name


let match_component tree = function
  | Tag (name_comp, attrs) ->
    (match tree with
     | Xml.Element { name=tag; _ } when match_name_component tag name_comp -> match_attributes tree attrs
     | _ -> false)
  | SingleWildcard attrs ->
    (match tree with
     | Xml.Element _ -> match_attributes tree attrs
     | Xml.Data _ -> false)
  | Index _ -> false
  | MultiWildcard attrs ->
    (match tree with
     | Xml.Element _ -> match_attributes tree attrs
     | Xml.Data _ -> false)
  | CurrentNode -> true  (* Current node always matches *)
  | ParentNode -> true   (* Parent node matching is handled in search logic *)


let path_to_string path =
  List.fold_right (fun x acc -> "/" ^ x ^ acc) path ""

(* Shared pretty printing functions for components *)
let pp_name_component fmt = function
  | Raw name -> Format.fprintf fmt "%s" name
  | Regex (pattern, _) -> Format.fprintf fmt "'%s'" pattern

let pp_component fmt = function
  | Tag (name_comp, attrs) ->
    Format.fprintf fmt "%a" pp_name_component name_comp;
    List.iter (fun {name; value} ->
      match value with
      | Any -> Format.fprintf fmt "@%s" name
      | Exact v -> Format.fprintf fmt "@%s=\"%s\"" name v
    ) attrs
  | Index (i, name_comp_opt) ->
    (match name_comp_opt with
     | Some name_comp -> Format.fprintf fmt "%a" pp_name_component name_comp
     | None -> ());
    Format.fprintf fmt "[%d]" i
  | SingleWildcard attrs ->
    Format.fprintf fmt "*";
    List.iter (fun {name; value} ->
      match value with
      | Any -> Format.fprintf fmt "@%s" name
      | Exact v -> Format.fprintf fmt "@%s=\"%s\"" name v
    ) attrs
  | MultiWildcard attrs ->
    Format.fprintf fmt "**";
    List.iter (fun {name; value} ->
      match value with
      | Any -> Format.fprintf fmt "@%s" name
      | Exact v -> Format.fprintf fmt "@%s=\"%s\"" name v
    ) attrs
  | CurrentNode ->
    Format.fprintf fmt "."
  | ParentNode ->
    Format.fprintf fmt ".."

let component_to_string component =
  Format.asprintf "%a" pp_component component

let format_path_error full_path matched_prefix failed_component cause =
  let matched_str = if matched_prefix = [] then "" else "/" ^ (String.concat "/" (List.rev matched_prefix)) in
  let failed_str = component_to_string failed_component in
  Printf.sprintf "Path '%s' failed at component '%s' after matching '%s': %s"
    full_path failed_str matched_str cause

let parse_path path =
  match Parser.parse_path path with
  | Ok p -> p
  | Error msg -> failwith ("Failed to parse path: " ^ path ^ " with error: " ^ msg)


(** Optimized internal traversal state.
    Replaces 'search_node'. 'path_stack' stores ancestors in reverse order
    (e.g. ["parent"; "grandparent"; "root"]), avoiding expensive string concatenation
    during the search. *)
type traverse_state = {
  path_stack : string list;
  node : Xml.t;
  parent : traverse_state option;
}


(** Find all XML elements in [tree] that match the [path]. *)
let find_all_seq_0 (path : path_component list) (tree : Xml.t) : (string * Xml.t) Seq.t =
  (* Helper: Efficiently iterate children while maintaining the path stack.
     Using a list stack is O(1) per level, unlike string concat which is O(Length). *)
  let children_of_state s =
    match s.node with
    | Xml.Element {childs; name; _} ->
      let new_stack = name :: s.path_stack in
      List.to_seq childs
      |> Seq.map (fun child -> { path_stack = new_stack; node = child; parent = Some s; })
    | Xml.Data _ -> Seq.empty
  in

  (* Helper: DFS generator for MultiWildcard (**)
     This avoids the O(N^2) pitfall of BFS with Seq.append. *)
  let rec get_descendants state =
    Seq.cons state (
      match state.node with
      | Xml.Element {childs; name; _} ->
        let new_stack = name :: state.path_stack in
        let child_states =
          List.to_seq childs
          |> Seq.map (fun child -> { path_stack = new_stack; node = child; parent = Some state })
        in
        Seq.flat_map get_descendants child_states
      | Xml.Data _ -> Seq.empty
    )
  in

  (* Core recursive search function *)
  let rec find_path_in_children (p: path_component list) (states: traverse_state Seq.t) : traverse_state Seq.t =
    match p with
    | [] -> states
    | c :: rest ->
      match c with
      | Tag (name_comp, attrs) ->
        (* OPTIMIZATION: Fuse generation and filtering.
           Instead of flat_map children -> filter match, we filter strictly inside the loop. *)
        let matched =
          states |> Seq.flat_map (fun state ->
              match state.node with
              | Xml.Element {childs; name; _} ->
                (* Push current node's name to stack for the children *)
                let new_stack = name :: state.path_stack in
                List.to_seq childs
                |> Seq.filter_map (fun child ->
                    (* Check match immediately before allocating a state wrapper *)
                    match child with
                    | Xml.Element {name=child_name; _} ->
                      if match_name_component child_name name_comp && match_attributes child attrs then
                        Some { path_stack = new_stack; node = child; parent = Some state }
                      else None
                    | Xml.Data _ -> None
                  )
              | Xml.Data _ -> Seq.empty
            )
        in
        find_path_in_children rest matched

      | SingleWildcard attrs ->
        let matched =
          states |> Seq.flat_map children_of_state
          |> Seq.filter (fun s -> match_component s.node (SingleWildcard attrs))
        in
        find_path_in_children rest matched

      | MultiWildcard attrs ->
        (* OPTIMIZATION: DFS traversal.
           We generate all descendants efficiently and then filter. *)
        let all_descendants = Seq.flat_map get_descendants states in
        let matched_descendants =
          Seq.filter (fun s -> match_component s.node (MultiWildcard attrs)) all_descendants
        in
        find_path_in_children rest matched_descendants

      | Index (i, name_comp_opt) ->
        let children = Seq.flat_map children_of_state states in
        let filtered =
          match name_comp_opt with
          | None -> children
          | Some nc ->
            children |> Seq.filter (fun s ->
                match s.node with
                | Xml.Element {name; _} -> match_name_component name nc
                | _ -> false
              )
        in
        (* Efficiently skip 'i' items and take the next one *)
        let target = filtered |> Seq.drop i |> Seq.take 1 in
        find_path_in_children rest target

      | CurrentNode ->
        find_path_in_children rest states

      | ParentNode ->
        (* OPTIMIZATION: Use XML parent pointer + Stack Pop.
           No string manipulation required. *)
        let parents = Seq.filter_map (fun s -> s.parent) states in
        find_path_in_children rest parents
  in

  (* Initial logic to handle the root match case *)
  let initial_state = { path_stack = []; node = tree; parent = None } in

  let result_seq =
    match path with
    | (Tag(name_comp, _) as first) :: rest ->
      let root_name = match tree with Xml.Element {name; _} -> name | _ -> "" in
      if match_name_component root_name name_comp && match_component tree first then
        (* Match root, search rest in children of root *)
        find_path_in_children rest (Seq.return initial_state)
      else
        (* No match, search entire path in children of root *)
        find_path_in_children path (Seq.return initial_state)
    | CurrentNode :: rest ->
      find_path_in_children rest (Seq.return initial_state)
    | ParentNode :: _ ->
      Seq.empty
    | _ ->
      find_path_in_children path (Seq.return initial_state)
  in

  (* Final Step: Reconstruct string paths only for the matching results *)
  result_seq |> Seq.map (fun s ->
      (* Reconstruct path from stack: ["child"; "root"] -> "/root/child" *)
      let path_prefix =
        if s.path_stack = [] then ""
        else "/" ^ (String.concat "/" (List.rev s.path_stack))
      in
      let final_path = match s.node with
        | Xml.Element {name; _} -> path_prefix ^ "/" ^ name
        | Xml.Data _ -> path_prefix
      in
      (final_path, s.node)
    )


(** Find all XML elements in [tree] that match the [path] with enhanced error reporting.
    When [strict] is true, raises detailed exceptions for path failures.
    When [strict] is false, behaves like find_all_seq_0 (returns empty sequence on failures). *)
let find_all_seq_strict (strict : bool) (path_str : string) (tree : Xml.t) : (string * Xml.t) Seq.t =
  let path = parse_path path_str in

  (* Enhanced version of children_of_state with error tracking *)
  let children_of_state_with_error s current_component current_path =
    match s.node with
    | Xml.Element {childs; name; _} ->
      let new_stack = name :: s.path_stack in
      List.to_seq childs
      |> Seq.map (fun child -> { path_stack = new_stack; node = child; parent = Some s; })
    | Xml.Data _ ->
        if strict then
          raise (Path_not_found {
            full_path = path_str;
            matched_prefix = current_path;
            failed_component = current_component;
            cause = "Cannot navigate into text node";
          })
        else Seq.empty
  in

  (* Enhanced DFS generator with error tracking *)
  let rec get_descendants_with_error state current_component current_path =
    Seq.cons state (
      match state.node with
      | Xml.Element {childs; name; _} ->
        let new_stack = name :: state.path_stack in
        let child_states =
          List.to_seq childs
          |> Seq.map (fun child -> { path_stack = new_stack; node = child; parent = Some state })
        in
        Seq.flat_map (fun child_state -> get_descendants_with_error child_state current_component current_path) child_states
      | Xml.Data _ ->
          if strict then
            raise (Path_not_found {
              full_path = path_str;
              matched_prefix = current_path;
              failed_component = current_component;
              cause = "Cannot traverse descendants of text node";
            })
          else Seq.empty
    )
  in

  (* Enhanced recursive search function with error tracking *)
  let rec find_path_in_children_with_error (p: path_component list) (states: traverse_state Seq.t) (current_path : string list) : traverse_state Seq.t =
    match p with
    | [] -> states
    | c :: rest ->
      let new_path = (component_to_string c) :: current_path in

      match c with
      | Tag (name_comp, attrs) ->
        let matched =
          states |> Seq.flat_map (fun state ->
              match state.node with
              | Xml.Element {childs; name; _} ->
                let new_stack = name :: state.path_stack in
                List.to_seq childs
                |> Seq.filter_map (fun child ->
                    match child with
                    | Xml.Element {name=child_name; _} ->
                      if match_name_component child_name name_comp && match_attributes child attrs then
                        Some { path_stack = new_stack; node = child; parent = Some state }
                      else None
                    | Xml.Data _ -> None
                  )
              | Xml.Data _ ->
                  if strict then
                    raise (Path_not_found {
                      full_path = path_str;
                      matched_prefix = current_path;
                      failed_component = c;
                      cause = "Cannot search children of text node";
                    })
                  else Seq.empty
            )
        in
        (if strict && Seq.is_empty matched && not (Seq.is_empty states) then
           raise (Path_not_found {
             full_path = path_str;
             matched_prefix = current_path;
             failed_component = c;
             cause = "No matching child elements found";
           });
         find_path_in_children_with_error rest matched new_path)

      | SingleWildcard attrs ->
        let matched =
          states |> Seq.flat_map (fun s -> children_of_state_with_error s c current_path)
          |> Seq.filter (fun s -> match_component s.node (SingleWildcard attrs))
        in
        (if strict && Seq.is_empty matched && not (Seq.is_empty states) then
           raise (Path_not_found {
             full_path = path_str;
             matched_prefix = current_path;
             failed_component = c;
             cause = "No elements matched wildcard pattern";
           });
         find_path_in_children_with_error rest matched new_path)

      | MultiWildcard attrs ->
        let all_descendants = Seq.flat_map (fun s -> get_descendants_with_error s c current_path) states in
        let matched_descendants =
          Seq.filter (fun s -> match_component s.node (MultiWildcard attrs)) all_descendants
        in
        (if strict && Seq.is_empty matched_descendants && not (Seq.is_empty states) then
           raise (Path_not_found {
             full_path = path_str;
             matched_prefix = current_path;
             failed_component = c;
             cause = "No elements matched multi-wildcard pattern";
           });
         find_path_in_children_with_error rest matched_descendants new_path)

      | Index (i, name_comp_opt) ->
        let children = Seq.flat_map (fun s -> children_of_state_with_error s c current_path) states in
        let filtered =
          match name_comp_opt with
          | None -> children
          | Some nc ->
            children |> Seq.filter (fun s ->
                match s.node with
                | Xml.Element {name; _} -> match_name_component name nc
                | _ -> false
              )
        in
        let children_list = List.of_seq filtered in
        (if strict && i >= List.length children_list then
           raise (Index_out_of_bounds {
             full_path = path_str;
             index = i;
             collection_length = List.length children_list;
             component = c;
           });
         let target =
           if i < List.length children_list then
             Seq.return (List.nth children_list i)
           else
             Seq.empty
         in
         find_path_in_children_with_error rest target new_path)

      | CurrentNode ->
        find_path_in_children_with_error rest states new_path

      | ParentNode ->
        let parents = Seq.filter_map (fun s -> s.parent) states in
        (if strict && Seq.is_empty parents && not (Seq.is_empty states) then
           raise (Path_not_found {
             full_path = path_str;
             matched_prefix = current_path;
             failed_component = c;
             cause = "Cannot navigate to parent (already at root)";
           });
         find_path_in_children_with_error rest parents new_path)
  in

  (* Initial state setup with error handling *)
  let initial_state = { path_stack = []; node = tree; parent = None } in

  let result_seq =
    match path with
    | (Tag(name_comp, _) as first) :: rest ->
      let root_name = match tree with Xml.Element {name; _} -> name | _ -> "" in
      if match_name_component root_name name_comp && match_component tree first then
        find_path_in_children_with_error rest (Seq.return initial_state) [root_name]
      else
        find_path_in_children_with_error path (Seq.return initial_state) []
    | CurrentNode :: rest ->
      find_path_in_children_with_error rest (Seq.return initial_state) []
    | ParentNode :: _ ->
        if strict then
          raise (Path_not_found {
            full_path = path_str;
            matched_prefix = [];
            failed_component = ParentNode;
            cause = "Cannot navigate to parent from root";
          })
        else Seq.empty
    | _ ->
      find_path_in_children_with_error path (Seq.return initial_state) []
  in

  (* Final Step: Reconstruct string paths only for the matching results *)
  result_seq |> Seq.map (fun s ->
      let path_prefix =
        if s.path_stack = [] then ""
        else "/" ^ (String.concat "/" (List.rev s.path_stack))
      in
      let final_path = match s.node with
        | Xml.Element {name; _} -> path_prefix ^ "/" ^ name
        | Xml.Data _ -> path_prefix
      in
      (final_path, s.node)
    )

(** Find all XML elements in [tree] that match the [path]. *)
let find_all_seq (path : string) (tree : Xml.t) : (string * Xml.t) Seq.t =
  find_all_seq_strict false path tree


(** Find all XML elements in [tree] that match the [path] with enhanced error reporting.
    When [strict] is true, raises detailed exceptions for path failures.
    When [strict] is false, returns empty list for backward compatibility. *)
let find_all_strict (strict : bool) (path : string) (tree : Xml.t) : (string * Xml.t) list =
  find_all_seq_strict strict path tree |> List.of_seq

(** Find all XML elements in [tree] that match the [path]. *)
let find_all (path : string) (tree : Xml.t) : (string * Xml.t) list =
  find_all_strict false path tree


(** Find the first XML element in [tree] that matches the [path]. *)
let find_opt (path : string) (tree : Xml.t) : (string * Xml.t) option =
  find_all_seq path tree |> Seq.uncons |> Option.map fst


(** Find the first XML element in [tree] that matches the [path] with enhanced error reporting.
    When [strict] is true, raises detailed exceptions for path failures.
    When [strict] is false, raises generic Not_found for backward compatibility. *)
let find_strict (strict : bool) (path : string) (tree : Xml.t) : string * Xml.t =
  if strict then (
    (* In strict mode, we call the strict version directly to get detailed errors *)
    match find_all_seq_strict true path tree |> Seq.uncons with
    | Some (result, _) -> result
    | None -> raise (Path_not_found {
        full_path = path;
        matched_prefix = [];
        failed_component = CurrentNode; (* placeholder *)
        cause = "No elements found matching the complete path";
      })
  ) else (
    (* In non-strict mode, use the original function for backward compatibility *)
    match find_all_seq_strict false path tree |> Seq.uncons with
    | Some (result, _) -> result
    | None -> raise Not_found
  )

(** Find the first XML element in [tree] that matches the [path].
    @raise [Not_found] when no XML element found *)
let find (path : string) (tree : Xml.t) : string * Xml.t =
  find_strict false path tree


(** Find a XML element that path matches [path], and return the attribute [attr] value of it.
    @raise [Invalid_argument] if [path] is invalid, like wildcards path or indexes path. *)
let find_attr_opt (path : string) (attr : string) (tree : Xml.t) : (string * string) option =
  let parsed_path = parse_path path in
  let last_component = List.hd @@ List.rev parsed_path in
  match last_component with
  | Tag (name_comp, existing_attrs) ->
    (* Check if the required attribute is already in the path constraints *)
    let attr_already_exists = List.exists (fun a -> a.name = attr) existing_attrs in

    (* Add the attribute constraint if it's not already there *)
    let final_attrs =
      if attr_already_exists then
        existing_attrs
      else
        { name = attr; value = Any } :: existing_attrs
    in

    (* Create the final path with the attribute constraint added *)
    let final_last_component = Tag (name_comp, final_attrs) in
    let final_path =
      if List.length parsed_path = 1 then
        [final_last_component]
      else
        let all_but_last = List.rev (List.tl (List.rev parsed_path)) in
        all_but_last @ [final_last_component]
    in

    (* Find the element that matches the updated path (which now includes the attribute requirement) *)
    let matched_element = find_all_seq_0 final_path tree |> Seq.uncons |> Option.map fst in
    Option.map (fun (element_path, xml) ->
        let attr_val = Xml.get_attr attr xml in
        (element_path, attr_val)
      ) matched_element
  | _ -> raise (Invalid_argument "Invalid path for find_attr, the last component must matches a tag")


let find_attr (path : string) (attr : string) (tree : Xml.t) : string * string =
  match find_attr_opt path attr tree with
  | Some result -> result
  | _ -> raise Not_found


let get_attr_opt path attr tree = find_attr_opt path attr tree |> Option.map snd
let get_int_attr_opt path attr tree = Option.bind (get_attr_opt path attr tree) int_of_string_opt
let get_float_attr_opt path attr tree = Option.bind (get_attr_opt path attr tree) float_of_string_opt
let get_bool_attr_opt path attr tree = Option.bind (get_attr_opt path attr tree) (fun x -> x |> String.lowercase_ascii |> bool_of_string_opt)
let get_int64_attr_opt path attr tree = Option.bind (get_attr_opt path attr tree) Int64.of_string_opt

let get_attr path attr tree = find_attr path attr tree |> snd
let get_int_attr path attr tree = get_attr path attr tree |> int_of_string
let get_float_attr path attr tree = get_attr path attr tree |> float_of_string
let get_bool_attr path attr tree = get_attr path attr tree |> String.lowercase_ascii |> bool_of_string
let get_int64_attr path attr tree = get_attr path attr tree |> Int64.of_string


(* Equality and pretty printing functions for testing *)
let equal_path path1 path2 =
  path1 = path2


let pp_path fmt path =
  match path with
  | [] -> Format.fprintf fmt "/"
  | components ->
      Format.fprintf fmt "/%s"
        (String.concat "/" (List.map (fun c ->
           Format.asprintf "%a" pp_component c
         ) components))
