
open Alsdiff_output.Layout_engine
open Alsdiff_output.View_model

let test_compact () =
  let view =
    Element {
      name = "MidiClip";
      change = Modified;
      domain_type = DTOther;
      fields = [
        { name = "Name"; change = Modified; domain_type = DTOther; oldval = Some (Fstring "Old"); newval = Some (Fstring "New") }
      ]
    }
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp compact ppf view;  (* Use compact preset *)
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  (* Compact mode should show the element name and change, but NOT the fields *)
  Alcotest.(check string) "compact output" "* MidiClip" (String.trim output)

let test_full () =
  let view =
    Element {
      name = "MidiClip";
      change = Modified;
      domain_type = DTOther;
      fields = [
        { name = "Name"; change = Modified; domain_type = DTOther; oldval = Some (Fstring "Old"); newval = Some (Fstring "New") }
      ]
    }
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp full ppf view;  (* Use full preset *)
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  (* Full mode should show the element name, change, AND the fields *)
  let expected = "* MidiClip\n  * Name: Old -> New" in
  Alcotest.(check string) "full output" expected (String.trim output)

let test_collection () =
  let view =
    Collection {
      name = "Notes";
      change = Modified;
      domain_type = DTOther;
      elements = [
        {
          name = "Note";
          change = Added;
          domain_type = DTOther;
          fields = [
             { name = "Pitch"; change = Added; domain_type = DTOther; oldval = None; newval = Some (Fint 60) }
          ]
        }
      ]
    }
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp full ppf view;  (* Use full preset *)
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  let expected = "* Notes\n  + Note\n    + Pitch: 60" in
  Alcotest.(check string) "collection output" expected (String.trim output)

(* New test: Removed items show summary only *)
let test_removed_summary () =
  let view =
    Element {
      name = "MidiClip";
      change = Removed;
      domain_type = DTOther;
      fields = [
        { name = "Name"; change = Removed; domain_type = DTOther; oldval = Some (Fstring "Test"); newval = None }
      ]
    }
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp midi_friendly ppf view;  (* midi_friendly has removed = Summary *)
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  (* Summary mode shows name only, no change symbol *)
  Alcotest.(check string) "removed summary" "MidiClip" (String.trim output)

(* New test: Collection item limiting *)
let test_collection_limit () =
  let elements = List.init 100 (fun i ->
    { name = "Note"; change = Added; domain_type = DTOther; fields = [{ name = "Pitch"; change = Added; domain_type = DTOther; oldval = None; newval = Some (Fint i) }] }
  ) in
  let view = Collection { name = "Notes"; change = Added; domain_type = DTOther; elements } in
  let cfg = { full with max_collection_items = Some 10 } in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp cfg ppf view;
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  (* Should only show 10 items, which is less than the 100 original items *)
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  (* 10 items * 2 lines per item (element + field) + 1 line for collection header = 21 lines max *)
  Alcotest.(check bool) "collection limited" true (List.length lines < 100)

(* New test: None level hides items *)
let test_none_level () =
  let cfg = { full with unchanged = None } in
  let view =
    Element {
      name = "MidiClip";
      change = Unchanged;
      domain_type = DTOther;
      fields = []
    }
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp cfg ppf view;
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  Alcotest.(check string) "none level" "" (String.trim output)

let tests = [
  "compact", `Quick, test_compact;
  "full", `Quick, test_full;
  "collection", `Quick, test_collection;
  "removed summary", `Quick, test_removed_summary;
  "collection limit", `Quick, test_collection_limit;
  "none level", `Quick, test_none_level;
]

let () =
  Alcotest.run "Layout_engine" [
    "layout", tests
  ]
