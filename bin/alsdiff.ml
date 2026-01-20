open Alsdiff_base
open Alsdiff_live
open Alsdiff_output
open Eio.Std
open View_model
open Cmdliner
open Cmdliner.Term.Syntax
open Build_info.V1

let load_liveset ~domain_mgr file =
  Eio.Domain_manager.run domain_mgr @@ fun () ->
  let xml = File.open_als file in
  Liveset.create xml file

let create_views (change : (Liveset.t, Liveset.Patch.t) Diff.structured_change)
  : View_model.view list =
  let item = View_model.create_liveset_item change in
  [View_model.Item item]

let render_views config (views : View_model.view list) : string =
  let buffer = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;

  List.iter (fun view ->
      Text_renderer.pp config ppf view;
      Fmt.pf ppf "@.";
    ) views;

  Format.pp_print_flush ppf ();
  Buffer.contents buffer

type config = {
  file1: string option;
  file2: string option;
  config_file: string option;
  preset: [ `Compact | `Composer | `Full | `Mixing | `Quiet | `Verbose ] option;
  prefix_added: string option;
  prefix_removed: string option;
  prefix_modified: string option;
  prefix_unchanged: string option;
  note_name_style: note_display_style option;
  max_collection_items: int option;
  dump_schema: string;
  validate_config: string option;
}

let load_config_from_json file_path =
  try
    let json_value = Yojson.Safe.from_file file_path in
    match Text_renderer.detail_config_of_yojson json_value with
    | Ok cfg -> cfg
    | Error msg -> failwith ("Failed to parse config file: " ^ msg)
  with
  | Yojson.Json_error msg ->
    failwith ("JSON error in " ^ file_path ^ ": " ^ msg)
  | Sys_error msg ->
    failwith ("I/O error reading " ^ file_path ^ ": " ^ msg)

let find_git_root () =
  let rec search path =
    if Sys.file_exists (Filename.concat path ".git") then
      Some path
    else
      let parent = Filename.dirname path in
      if parent = path then None
      else search parent
  in
  search (Sys.getcwd ())

let get_home_dir () =
  match Sys.getenv_opt "HOME" with
  | Some home -> Some home
  | None ->
    (* Fallback for Windows systems *)
    match Sys.getenv_opt "USERPROFILE" with
    | Some userprofile -> Some userprofile
    | None -> None

let discover_config_file () =
  (* Try git root config first *)
  let check_git_config () =
    match find_git_root () with
    | Some git_root ->
      let git_config = Filename.concat git_root ".alsdiff.json" in
      if Sys.file_exists git_config then Some git_config else None
    | None -> None
  in
  (* Try home directory config *)
  let check_home_config () =
    match get_home_dir () with
    | Some home ->
      let home_config = Filename.concat home ".alsdiff.json" in
      if Sys.file_exists home_config then Some home_config else None
    | None -> None
  in
  (* Priority: git config > home config *)
  match check_git_config () with
  | Some _ as result -> result
  | None -> check_home_config ()

let load_and_report_config config_path =
  Fmt.pr "Loading configuration from %s@." config_path;
  load_config_from_json config_path

let diff_cmd ~config ~domain_mgr =
  let file1 = match config.file1 with
    | Some f -> f
    | None -> failwith "FILE1.als is required for diff"
  in
  let file2 = match config.file2 with
    | Some f -> f
    | None -> failwith "FILE2.als is required for diff"
  in

  let base_renderer_config =
    match config.config_file with
    | Some config_path ->
      load_config_from_json config_path
    | None ->
      match config.preset with
      | Some preset ->
        let base = match preset with
          | `Compact -> Text_renderer.compact
          | `Composer -> Text_renderer.composer
          | `Full -> Text_renderer.full
          | `Mixing -> Text_renderer.mixing
          | `Quiet -> Text_renderer.quiet
          | `Verbose -> Text_renderer.verbose
        in base
      | None ->
        (* Auto-discover .alsdiff.json when neither --config nor --preset specified *)
        match discover_config_file () with
        | Some auto_config -> load_and_report_config auto_config
        | None -> Text_renderer.quiet
  in

  let renderer_config = { base_renderer_config with
                          prefix_added = (match config.prefix_added with Some s -> s | None -> base_renderer_config.prefix_added);
                          prefix_removed = (match config.prefix_removed with Some s -> s | None -> base_renderer_config.prefix_removed);
                          prefix_modified = (match config.prefix_modified with Some s -> s | None -> base_renderer_config.prefix_modified);
                          prefix_unchanged = (match config.prefix_unchanged with Some s -> s | None -> base_renderer_config.prefix_unchanged);
                          note_name_style = (match config.note_name_style with Some s -> s | None -> base_renderer_config.note_name_style);
                          max_collection_items = (match config.max_collection_items with Some n -> Some n | None -> base_renderer_config.max_collection_items);
                        } in

  let liveset1, liveset2 = Fiber.pair
      (fun () -> load_liveset ~domain_mgr file1)
      (fun () -> load_liveset ~domain_mgr file2)
  in

  let liveset_patch = Liveset.diff liveset1 liveset2 in

  let liveset_change =
    if Liveset.Patch.is_empty liveset_patch then
      `Unchanged
    else
      `Modified liveset_patch
  in

  let views = create_views liveset_change in

  let output = render_views renderer_config views in
  Fmt.pr "%s@." output

let file1 =
  let doc = "First .als file to compare (required for diff, not needed with --dump-schema)" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"FILE1.als" ~doc)

let file2 =
  let doc = "Second .als file to compare (required for diff, not needed with --dump-schema)" in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"FILE2.als" ~doc)

let config_file =
  let doc = "Load configuration from JSON file. Overrides --preset, individual CLI options override config values." in
  Arg.(value & opt (some string) None & info ["config"] ~docv:"CONFIG.json" ~doc)

let preset =
  let doc = "Output detail preset. Ignored when --config is specified. $(b,compact)=show names+symbols only, $(b,full)=show all details, $(b,mixing)=optimized for stem track mixing workflows, $(b,composer)=MIDI composition and sample processing only, $(b,quiet)=minimal output, $(b,verbose)=show everything including unchanged" in
  Arg.(value & opt (some (enum ["compact", `Compact; "composer", `Composer; "full", `Full; "mixing", `Mixing; "quiet", `Quiet; "verbose", `Verbose])) None & info ["preset"] ~docv:"PRESET" ~doc)

let prefix_added =
  let doc = "Prefix for added items (default from preset: '+')" in
  Arg.(value & opt (some string) None & info ["prefix-added"] ~docv:"PREFIX" ~doc)

let prefix_removed =
  let doc = "Prefix for removed items (default from preset: '-')" in
  Arg.(value & opt (some string) None & info ["prefix-removed"] ~docv:"PREFIX" ~doc)

let prefix_modified =
  let doc = "Prefix for modified items (default from preset: '*')" in
  Arg.(value & opt (some string) None & info ["prefix-modified"] ~docv:"PREFIX" ~doc)

let prefix_unchanged =
  let doc = "Prefix for unchanged items (default from preset: '')" in
  Arg.(value & opt (some string) None & info ["prefix-unchanged"] ~docv:"PREFIX" ~doc)

let note_name_style =
  let doc = "Note name display style. $(b,Sharp)=C# D# etc., $(b,Flat)=Db Eb etc. (default from preset: Sharp)" in
  Arg.(value & opt (some (enum ["Sharp", Sharp; "Flat", Flat])) None & info ["note-name-style"] ~docv:"STYLE" ~doc)

let max_collection_items =
  let doc = "Maximum number of items to show in collections (default from preset: None/10/50 depending on preset)" in
  Arg.(value & opt (some int) None & info ["max-collection-items"] ~docv:"N" ~doc)

let dump_schema =
  let doc = "Dump JSON schema for configuration to stdout (or to FILE if specified) and exit. Does not require FILE1.als or FILE2.als." in
  Arg.(value & opt ~vopt:"-" string "" & info ["dump-schema"] ~docv:"FILE" ~doc)

let validate_config =
  let doc = "Validate a configuration file against the JSON schema and exit. Reports validation errors without running diff." in
  Arg.(value & opt (some string) None & info ["validate-config"] ~docv:"FILE" ~doc)

let config_ref = ref None

let cmd =
  let doc = "Compare two Ableton Live Set (.als) files and show differences" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) compares two Ableton Live Set files and displays the differences between them using various output formats.";
    `P "The tool supports multiple output presets and allows fine-grained customization of display through command-line options.";
    `P "Configuration can be loaded from a JSON file, with CLI options able to override individual values.";
    `S Manpage.s_examples;
    `P "Compare two files with default quiet preset:";
    `Pre "$(cmd) v1.als v2.als";
    `P "Compare with compact output:";
    `Pre "$(cmd) v1.als v2.als --preset compact";
    `P "Compare with full details:";
    `Pre "$(cmd) v1.als v2.als --preset full";
    `P "Verbose comparison (show everything including unchanged):";
    `Pre "$(cmd) v1.als v2.als --preset verbose";
    `P "Customize prefixes:";
    `Pre "$(cmd) v1.als v2.als --prefix-added \"[+]\" --prefix-removed \"[-]\" --prefix-modified \"[*]\"";
    `P "Use flat note names for MIDI:";
    `Pre "$(cmd) v1.als v2.als --note-name-style Flat";
    `P "Limit collection items to 100:";
    `Pre "$(cmd) v1.als v2.als --max-collection-items 100";
    `P "Combine multiple options:";
    `Pre "$(cmd) v1.als v2.als --preset compact --prefix-added \"ADD \" --max-collection-items 50";
    `P "Load configuration from JSON file:";
    `Pre "$(cmd) v1.als v2.als --config myconfig.json";
    `P "Use config file with CLI override:";
    `Pre "$(cmd) v1.als v2.als --config myconfig.json --max-collection-items 100";
    `P "Config file with prefix override:";
    `Pre "$(cmd) v1.als v2.als --config myconfig.json --prefix-added \"ADD \"";
    `P "Auto-discover configuration from .alsdiff.json:";
    `Pre "$(cmd) v1.als v2.als";
    `P "Dump configuration JSON schema to stdout:";
    `Pre "$(cmd) --dump-schema";
    `P "Dump configuration JSON schema to file:";
    `Pre "$(cmd) --dump-schema config-schema.json";
    `P "Validate a configuration file:";
    `Pre "$(cmd) --validate-config myconfig.json";
    `P "Configuration search order (when --config not specified):";
    `P "1. --preset PRESET (if specified)";
    `P "2. .alsdiff.json in git repository root";
    `P "3. .alsdiff.json in user's home directory (~)";
    `P "4. quiet preset (default)";
    `S Manpage.s_options;
    `P "$(b,--config FILE) loads configuration from JSON file. Takes precedence over auto-discovery. The --preset option is ignored when --config is specified. Individual CLI options override values from config file.";
    `P "$(b,--preset PRESET) sets the output detail preset. Available presets: $(b,compact), $(b,composer), $(b,full), $(b,mixing), $(b,quiet) (default), $(b,verbose). Takes precedence over auto-discovery but ignored when --config is specified.";
    `P "$(b,--prefix-added PREFIX) overrides prefix for added items from config file.";
    `P "$(b,--prefix-removed PREFIX) overrides prefix for removed items from config file.";
    `P "$(b,--prefix-modified PREFIX) overrides prefix for modified items from config file.";
    `P "$(b,--prefix-unchanged PREFIX) overrides prefix for unchanged items from config file.";
    `P "$(b,--note-name-style STYLE) overrides note name style from config file.";
    `P "$(b,--max-collection-items N) overrides max collection items from config file.";
    `P "$(b,--dump-schema [FILE]) dumps JSON schema for configuration to stdout or FILE and exits.";
    `P "$(b,--validate-config FILE) validates a configuration file against the JSON schema and exits. Useful for checking config files before use.";
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/krfantasy/alsdiff/issues";
  ] in
  Cmd.make (Cmd.info "alsdiff" ~version:(match version () with
      | None -> "dev"
      | Some v -> Version.to_string v) ~doc ~man) @@
  let+ file1 and+ file2 and+ config_file and+ preset and+ prefix_added and+ prefix_removed and+ prefix_modified and+ prefix_unchanged and+ note_name_style and+ max_collection_items and+ dump_schema and+ validate_config in
  let cfg = { file1; file2; config_file; preset; prefix_added; prefix_removed; prefix_modified; prefix_unchanged; note_name_style; max_collection_items; dump_schema; validate_config } in
  config_ref := Some cfg;
  ()

let main () =
  Printexc.record_backtrace true;
  let exit_code = Cmd.eval cmd in
  if exit_code = 0 then
    match !config_ref with
    | None -> 0
    | Some cfg ->
      (* Handle --validate-config first *)
      (match cfg.validate_config with
       | Some config_path ->
         (match Text_renderer.validate_config_file config_path with
          | Ok () ->
            Fmt.pr "Configuration file %s is valid@." config_path;
            0
          | Error msg ->
            Fmt.epr "%s@." msg;
            1)
       | None ->
         (* Handle --dump-schema *)
         if cfg.dump_schema <> "" then begin
           (* Dump to file or stdout *)
           if cfg.dump_schema = "-" then begin
             print_endline (Text_renderer.detail_config_schema_to_string ());
             0
           end else begin
             Text_renderer.write_schema_to_file cfg.dump_schema;
             Fmt.pr "Schema written to %s@." cfg.dump_schema;
             0
           end
         end else begin
           (* Normal diff operation - requires both files *)
           match cfg.file1, cfg.file2 with
           | Some _, Some _ ->
             Eio_main.run @@ fun env ->
             let domain_mgr = Eio.Stdenv.domain_mgr env in
             diff_cmd ~config:cfg ~domain_mgr;
             0
           | _ ->
             Fmt.epr "Error: FILE1.als and FILE2.als are required for diff@.";
             Fmt.epr "Use --dump-schema to generate configuration schema without files.@.";
             Fmt.epr "Use --validate-config FILE to validate a configuration file.@.";
             1
         end)
  else
    exit exit_code

let () =
  if !Sys.interactive then () else exit (main ())
