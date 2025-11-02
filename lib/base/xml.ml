type t =
  | Element of {
      name: string;
      attrs: (string * string) list;
      mutable childs: t list;
      mutable parent : t option;
    }
  | Data of {
      value : string;
      mutable parent : t option;
    }


let get_childs = function
  | Element { childs; _ } -> childs
  | Data _ -> invalid_arg "Cannot get childs from Data"


let get_value = function
  | Data { value; _ } -> value
  | Element _ -> invalid_arg "Cannot get value from Element"


let get_parent = function
  | Element { parent; _ } -> parent
  | Data { parent; _ } -> parent


let get_attr name xml =
  match xml with
  | Element { attrs; _ } -> List.assoc name attrs
  | Data _ -> raise (Invalid_argument "Can not found attribute in a XML Data")

let get_int_attr name xml =
  get_attr name xml |> int_of_string

let get_float_attr name xml =
  get_attr name xml |> float_of_string

let get_int64_attr name xml =
  get_attr name xml |> Int64.of_string

let get_attr_opt name xml =
  match xml with
  | Element { attrs; _ } ->
    List.assoc_opt name attrs
  | Data _ -> None

let get_int_attr_opt name xml =
  Option.bind (get_attr_opt name xml) int_of_string_opt

let get_float_attr_opt name xml =
  Option.bind (get_attr_opt name xml) float_of_string_opt

let get_bool_attr_opt name xml =
  Option.bind (get_attr_opt name xml)
    (fun x -> x |> String.lowercase_ascii |> bool_of_string_opt)

let get_int64_attr_opt name xml =
  Option.bind (get_attr_opt name xml) Int64.of_string_opt

module Parser = struct
  (* Helper functions for parsing XML *)
  let get_name (tag : Xmlm.tag)  =
    let ((_, name), _) = tag in name

  let get_attrs (tag : Xmlm.tag) =
    let ((_, _), attrs) = tag in
    List.map (fun a -> let ((_, k), v) = a in (k, v)) attrs

  let make_element tag childs = Element {
      name = get_name tag;
      attrs = get_attrs tag;
      childs = childs;
      parent = None;
    }

  let make_data d = Data { value = d; parent = None }

  let rec setup_parent_links (node: t) (parent: t option) =
    match node with
    | Element element ->
      element.parent <- parent;
      List.iter (fun child -> setup_parent_links child (Some node)) element.childs
    | Data data -> data.parent <- parent

end

let parse_with_parent_links input =
  let parent_stack : t Stack.t = Stack.create() in

  let rec parse_tree () =
    match Xmlm.input input with
    | `El_start tag ->
        let current_parent = try Some (Stack.top parent_stack) with Stack.Empty -> None in
        let element = Element {
          name = Parser.get_name tag;
          attrs = Parser.get_attrs tag;
          childs = [];  (* temporary empty list *)
          parent = current_parent;
        } in
        (* Set as parent for subsequent children *)
        Stack.push element parent_stack;
        (* Parse children *)
        let rec parse_children acc =
          match Xmlm.peek input with
          | `El_end ->
              ignore (Xmlm.input input); (* consume El_end *)
              ignore (Stack.pop parent_stack);
              List.rev acc
          | `Data d ->
              ignore (Xmlm.input input); (* consume data signal *)
              let data_node = Data { value = d; parent = Some element } in
              parse_children (data_node :: acc)
          | `El_start _ ->
              let child = parse_tree () in
              parse_children (child :: acc)
          | _ -> failwith "Unexpected signal during parsing"
        in
        let childs = parse_children [] in
        (* Update the childs field using mutable update *)
        (match element with
         | Element el -> el.childs <- childs
         | _ -> ());
        element
    | `Data d ->
        let parent = try Some (Stack.top parent_stack) with Stack.Empty -> None in
        Data { value = d; parent = parent }
    | `El_end ->
        (* Handle end tag at root level - this can happen with empty elements *)
        failwith "Unexpected end tag at root level"
    | `Dtd _ ->
        (* Skip DTD declarations and continue parsing *)
        parse_tree ()
  in

  parse_tree ()

let read_file filename =
  let i = Xmlm.make_input ~strip:true (`Channel (In_channel.open_text filename)) in
  parse_with_parent_links i

let read_string s =
  let i = Xmlm.make_input ~strip:true (`String (0, s)) in
  parse_with_parent_links i
