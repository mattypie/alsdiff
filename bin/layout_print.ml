open Alsdiff_base
open Alsdiff_live
open Alsdiff_output
open Eio.Std

(** [get_config ()] parses the command line and returns the appropriate detail_config preset. *)
let get_config () =
  match Sys.argv with
  | [| _; _; _; mode |] ->
    (match mode with
     | "compact" -> Layout_engine.compact
     | "full" -> Layout_engine.full
     | "midi" -> Layout_engine.midi_friendly
     | "quiet" -> Layout_engine.quiet
     | "verbose" -> Layout_engine.verbose
     | "track_only" -> Layout_engine.track_only
     | _ -> Fmt.epr "Unknown mode: %s@." mode; exit 1)
  | _ -> Layout_engine.midi_friendly  (* Default *)

(** [load_liveset ~domain_mgr file] loads an .als file and creates a Liveset. *)
let load_liveset ~domain_mgr file =
  Eio.Domain_manager.run domain_mgr @@ fun () ->
  let xml = File.open_als file in
  Liveset.create xml file

(** [create_views change] converts a Liveset structured change into view objects. *)
let create_views (change : (Liveset.t, Liveset.Patch.t) Diff.structured_change)
  : View_model.view list =
  let section_view = View_model.create_liveset_view change in
  [View_model.Section section_view]

(** [render_views config views] renders a list of views to a string using the layout engine. *)
let render_views config (views : View_model.view list) : string =
  let buffer = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;

  List.iter (fun view ->
    Layout_engine.pp config ppf view;
    Fmt.pf ppf "@.";
  ) views;

  Format.pp_print_flush ppf ();
  Buffer.contents buffer

(** [main ~domain_mgr] is the main entry point. *)
let main ~domain_mgr =
  if Array.length Sys.argv < 3 then (
    Fmt.epr "Usage: %s <file1.als> <file2.als> [mode]@." Sys.argv.(0);
    Fmt.epr "Modes: compact, full, midi (default), quiet, verbose, track_only@.";
    exit 1
  );

  let config = get_config () in
  let file1 = Sys.argv.(1) in
  let file2 = Sys.argv.(2) in

  (* Load livesets in parallel *)
  let liveset1, liveset2 = Fiber.pair
    (fun () -> load_liveset ~domain_mgr file1)
    (fun () -> load_liveset ~domain_mgr file2)
  in

  (* Diff livesets - returns Liveset.Patch.t *)
  let liveset_patch = Liveset.diff liveset1 liveset2 in

  (* Wrap in `Modified to get (Liveset.t, Liveset.Patch.t) structured_change *)
  let liveset_change = `Modified liveset_patch in

  (* Create view using the full hierarchy *)
  let views = create_views liveset_change in

  (* Render and print *)
  let output = render_views config views in
  Fmt.pr "%s@." output

(** Entry point with error handling *)
let () =
  Printexc.record_backtrace true;
  try
    Eio_main.run @@ fun env ->
    main ~domain_mgr:(Eio.Stdenv.domain_mgr env)
  with
  | Xml.Invalid_Xml (xml, msg) ->
      Fmt.epr "Invalid XML: %a@." Xml.pp_invalid_xml (xml, msg)
  | e ->
      Fmt.epr "Error: %s@." (Printexc.to_string e);
      raise e
