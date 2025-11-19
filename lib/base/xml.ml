type t =
  | Element of { name: string; attrs: (string * string) list; childs: t list }
  | Data of string


let get_name = function
  | Element { name; _ } -> name
  | Data _ -> invalid_arg "Cannot get childs from Data"

let get_childs = function
  | Element { childs; _ } -> childs
  | Data _ -> invalid_arg "Cannot get childs from Data"

let get_value = function
  | Data value -> value
  | Element _ -> invalid_arg "Cannot get value from Element"


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
    }

  let make_data d = Data d

end

let read_file filename =
  let ic = In_channel.open_text filename in
  try
    let i = Xmlm.make_input ~strip:true (`Channel ic) in
    let result = Xmlm.input_doc_tree ~el:Parser.make_element ~data:Parser.make_data i |> snd in
    In_channel.close ic;
    result
  with e ->
    In_channel.close ic;
    raise e

let read_string s =
  let i = Xmlm.make_input ~strip:true (`String (0, s)) in
  Xmlm.input_doc_tree ~el:Parser.make_element ~data:Parser.make_data i |> snd
