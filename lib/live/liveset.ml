open Alsdiff_base
open Alsdiff_base.Diff

module IntHashtbl = Hashtbl.Make(Int)

let () =
  let h = IntHashtbl.create 17 in
  IntHashtbl.add h 10 "hello";

module Locator = struct
  type t = {
    id : int;
    name : string;
    time : float;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "Locator"; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr "/Name" "Value" xml in
      let time = Upath.get_float_attr "/Time" "Value" xml in
      { id; name; time }
    | _ -> failwith "Invalid XML element for creating Locator"

  let has_same_id a b = a.id = b.id
  let id_hash t = Hashtbl.hash t.id

  module Patch = struct
    type t = {
      name : string atomic_update;
      time : float atomic_update;
    }

    let is_empty = function
      | { name = `Unchanged; time = `Unchanged } -> true
      | _ -> false
  end

  let diff (old_locator : t) (new_locator : t) : Patch.t =
    let { id = old_id; name = old_name; time = old_time } = old_locator in
    let { id = new_id; name = new_name; time = new_time } = new_locator in

    (* Only compare locators with the same id *)
    if old_id <> new_id then
      failwith "cannot diff two locators with different Id"
    else
      let name_change = diff_atomic_value (module Equality.StringEq) old_name new_name in
      let time_change = diff_atomic_value (module Equality.FloatEq) old_time new_time in
      { name = name_change; time = time_change }
end

type version = {
  major : string;
  minor : string;
  reversion : string;
}

type t = {
  name : string;
  version : version;
  creator : string;
  tracks : Track.t list;
  pointees : int IntHashtbl.t;
}
