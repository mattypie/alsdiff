type t =
  | Element of { name: string; attrs: (string * string) list; childs: t list }
  | Data of string

exception Invalid_Xml of t * string

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

(* Escape XML special characters to entities *)
let escape_xml s =
  let len = String.length s in
  let buf = Buffer.create (len * 2) in
  String.iter (function
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&apos;"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(* Compact pretty printer without indentation or newlines *)
let rec pp fmt = function
  | Element {name; attrs; childs} ->
    Format.fprintf fmt "<%s" name;
    List.iter (fun (k, v) ->
      Format.fprintf fmt " %s=\"%s\"" k (escape_xml v)
    ) attrs;
    Format.fprintf fmt ">";

    List.iter (fun child -> pp fmt child) childs;

    Format.fprintf fmt "</%s>" name
  | Data s ->
    Format.fprintf fmt "%s" (escape_xml s)

(* Pretty printer with indentation and newlines *)
let rec pp_pretty fmt = function
  | Element {name; attrs; childs} ->
    (* @[<v 2> opens a vertical box with 2-space indentation *)
    Format.fprintf fmt "@[<v 2><%s" name;
    List.iter (fun (k, v) ->
      Format.fprintf fmt " %s=\"%s\"" k (escape_xml v)
    ) attrs;
    Format.fprintf fmt ">";

    (* @, adds a cut hint - newline if needed before each child *)
    List.iter (fun child -> Format.fprintf fmt "@,%a" pp_pretty child) childs;

    Format.fprintf fmt "@]</%s>" name
  | Data s ->
    if s <> "" then Format.fprintf fmt "%s" (escape_xml s)

let to_string xml =
  Fmt.str "%a" pp_pretty xml

let pp_invalid_xml fmt (xml, msg) =
  Fmt.pf fmt "%s@\nProblematic XML:\n%a@\n" msg pp_pretty xml
