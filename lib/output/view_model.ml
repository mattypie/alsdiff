open Alsdiff_live
open Alsdiff_base.Diff

type field_value =
  | Fint of int
  | Ffloat of float
  | Fbool of bool
  | Fstring of string

let int_value x = Fint x
let float_value x = Ffloat x
let bool_value x = Fbool x
let string_value x = Fstring x


type change_type =
  | Unchanged
  | Added
  | Removed
  | Modified


type view =
  | Field of field_view
  | Element of element_view
  | Collection of collection_view
  | Section of section_view

and field_view = {
  name : string;
  change : change_type;
  oldval : field_value option;
  newval : field_value option;
}

and element_view = {
  name : string;
  change : change_type;
  fields : field_view list;
}

and collection_view = {
  name : string;
  change : change_type;
  elements : element_view list;
}

and section_view = {
  name : string;
  change : change_type;
  sub_views : view list;
}

type ('a, 'p) field_descriptor =
  | FieldDesc : {
      name : string;
      of_parent_value : 'a -> 'b;  (* Get the field value when parent is Added/Removed *)
      of_parent_patch : 'p -> 'b atomic_change;  (* Get the field change when parent is Modified *)
      wrapper : 'b -> field_value;
    } -> ('a, 'p) field_descriptor


module ViewBuilder = struct

  let map_atomic_change (f : 'a -> 'b) (c : 'a atomic_change) : 'b atomic_change =
    match c with
    | `Added x -> `Added (f x)
    | `Removed x -> `Removed (f x)
    | `Modified { oldval; newval } -> `Modified { oldval = f oldval; newval = f newval }
    | `Unchanged -> `Unchanged

  let lift_atomic_change (c : 'a atomic_change) : ('a option * 'a option) =
    match c with
    | `Added a -> (None, Some a)
    | `Removed r -> (Some r, None)
    | `Modified { oldval; newval } -> (Some oldval, Some newval)
    | `Unchanged -> (None, None)


  (** [build_field_view c fd] build a [field_view] from the element change [c] with given field descriptor [fd].
      @param c the element change [('a, 'p) change]
      @param fd the field descriptor
  *)
  let build_field_view
      (c : ('a, 'p) structured_change)
      (FieldDesc fd : ('a, 'p) field_descriptor)
      : field_view =

    let name = fd.name in
    let (change_type, oldval, newval) =
      match c with
      | `Added a -> (Added, None, Some (fd.wrapper @@ fd.of_parent_value a))
      | `Removed r -> (Removed, Some (fd.wrapper @@ fd.of_parent_value r), None)
      | `Modified p ->
        let field_change = fd.of_parent_patch p in
        (match field_change with
         | `Added a -> (Added, None, Some (fd.wrapper a))
         | `Removed r -> (Removed, Some (fd.wrapper r), None)
         | `Modified { oldval; newval } -> (Modified, Some (fd.wrapper oldval), Some (fd.wrapper newval))
         | `Unchanged -> (Unchanged, None, None))
      | `Unchanged -> (Unchanged, None, None)
    in
    { name; change = change_type; oldval; newval }


  let build_element_view
    (c : ('a, 'p) structured_change)
    ~(name : string)
    ~(field_descs : ('a, 'p) field_descriptor list)
    : element_view =

    let change_type =
      match c with
      | `Added _ -> Added
      | `Removed _ -> Removed
      | `Modified _ -> Modified
      | `Unchanged -> Unchanged
    in
    let fields = field_descs |> List.map (build_field_view c) in
    { name; change = change_type; fields }

end


let create_events_element_view
  (c : (Automation.EnvelopeEvent.t, Automation.EnvelopeEvent.Patch.t) structured_change)
  : element_view =
  let open Automation in
  let field_descs = [
    FieldDesc {
      name = "Time";
      of_parent_value = (fun x -> x.EnvelopeEvent.time);
      of_parent_patch = (fun x -> (x.EnvelopeEvent.Patch.time :> float atomic_change));
      wrapper = float_value;
    };
    FieldDesc {
      name = "Value";
      of_parent_value = (fun x -> x.EnvelopeEvent.value);
      of_parent_patch = (fun x -> (x.EnvelopeEvent.Patch.time :> float atomic_change));
      wrapper = float_value;
    }
  ]
  in
  let fields =
    field_descs
    |> List.map @@ ViewBuilder.build_field_view c
  in
  {
    name = "EnvelopeEvent";
    change = Modified;
    fields
  }

(* let create_midi_note_element_view *)
(*   (c : (Clip.MidiNote.t, Clip.MidiNote.Patch.t) structured_change) *)
(*   : element_view = *)
(*   let open Clip in *)
(*   let field_descs = [ *)
(*     FieldDesc { *)
(*       name = "Pitch"; *)
(*       of_parent_value = (fun x -> x.MidiNote.note); *)
(*       of_parent_patch = (fun x -> (x.MidiNote.Patch.note :> int atomic_change)); *)
(*       wrapper = int_value; *)
(*     }; *)
(*   ] *)
(*   in *)
