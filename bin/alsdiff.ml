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
  positional_args: string list;
  git_mode: bool;
  config_file: string option;
  preset: [ `Compact | `Composer | `Full | `Inline | `Mixing | `Quiet | `Verbose ] option;
  dump_preset: [ `Compact | `Composer | `Full | `Inline | `Mixing | `Quiet | `Verbose ] option;
  prefix_added: string option;
  prefix_removed: string option;
  prefix_modified: string option;
  prefix_unchanged: string option;
  note_name_style: note_display_style option;
  max_collection_items: int option;
  dump_schema: bool;
  validate_config: string option;
}

type git_args = {
  path: string;
  old_file: string;
  new_file: string;
}

let parse_git_args args =
  match args with
  | [path; old_file; _old_hex; _old_mode; new_file; _new_hex; _new_mode] ->
    Ok { path; old_file; new_file }
  | _ ->
    Error "Git mode requires exactly 7 positional arguments: path old-file old-hex old-mode new-file new-hex new-mode"

let load_config_from_json file_path =
  match Text_renderer.load_and_validate_config file_path with
  | Ok cfg -> cfg
  | Error msg ->
    Fmt.epr "%s@." msg;
    exit 1

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

let discover_config_file ~reference_path =
  (* Try reference_path directory config first - highest priority *)
  let check_path_dir_config () =
    let path_dir = Filename.dirname reference_path in
    let path_config = Filename.concat path_dir ".alsdiff.json" in
    if Sys.file_exists path_config then Some path_config else None
  in
  (* Try git root config *)
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
  (* Priority: path dir > git config > home config *)
  match check_path_dir_config () with
  | Some _ as result -> result
  | None ->
    match check_git_config () with
    | Some _ as result -> result
    | None -> check_home_config ()

let load_and_report_config config_path =
  Fmt.pr "Loading configuration from %s@." config_path;
  load_config_from_json config_path

let diff_cmd ~config ~domain_mgr : int =
  let file1, file2, reference_path =
    if config.git_mode then
      match parse_git_args config.positional_args with
      | Error msg -> failwith msg
      | Ok git_args -> (git_args.old_file, git_args.new_file, git_args.path)
    else
      match config.positional_args with
      | [f1; f2] -> (f1, f2, f2)
      | _ -> failwith "FILE1.als and FILE2.als are required for diff"
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
          | `Inline -> Text_renderer.inline
          | `Mixing -> Text_renderer.mixing
          | `Quiet -> Text_renderer.quiet
          | `Verbose -> Text_renderer.verbose
        in base
      | None ->
        (* Auto-discover .alsdiff.json when neither --config nor --preset specified *)
        match discover_config_file ~reference_path with
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
  let has_changes = not (Liveset.Patch.is_empty liveset_patch) in

  let liveset_change =
    if has_changes then
      `Modified liveset_patch
    else
      `Unchanged
  in

  let views = create_views liveset_change in

  let output = render_views renderer_config views in
  Fmt.pr "%s@." output;

  (* Exit code: git mode uses trustExitCode semantics *)
  if config.git_mode then
    if has_changes then 1 else 0
  else
    0

let positional_args =
  let doc = "Positional arguments. Normal mode: FILE1.als FILE2.als. \
             Git mode (--git): path old-file old-hex old-mode new-file new-hex new-mode" in
  Arg.(value & pos_all string [] & info [] ~docv:"ARGS" ~doc)

let git_mode =
  let doc = "Enable git external diff driver mode. Expects exactly 7 positional arguments: \
             path old-file old-hex old-mode new-file new-hex new-mode. \
             Exit code 0 = no changes, 1 = changes found (for trustExitCode). \
             All other flags (--preset, --config, etc.) work in git mode." in
  Arg.(value & flag & info ["git"] ~doc)

let config_file =
  let doc = "Load configuration from JSON file. Overrides --preset, individual CLI options override config values." in
  Arg.(value & opt (some string) None & info ["config"] ~docv:"CONFIG.json" ~doc)

let preset =
  let doc = "Output detail preset. Ignored when --config is specified. $(b,compact)=show structure only, $(b,full)=show all details (multiline), $(b,inline)=show all details (single line), $(b,mixing)=optimized for stem track mixing, $(b,composer)=MIDI composition only, $(b,quiet)=minimal output, $(b,verbose)=show everything including unchanged" in
  Arg.(value & opt (some (enum ["compact", `Compact; "composer", `Composer; "full", `Full; "inline", `Inline; "mixing", `Mixing; "quiet", `Quiet; "verbose", `Verbose])) None & info ["preset"] ~docv:"PRESET" ~doc)

let dump_preset =
  let doc = "Dump preset configuration as JSON to stdout and exit. Same format as --config file." in
  Arg.(value & opt (some (enum ["compact", `Compact; "composer", `Composer; "full", `Full; "inline", `Inline; "mixing", `Mixing; "quiet", `Quiet; "verbose", `Verbose])) None & info ["dump-preset"] ~docv:"PRESET" ~doc)

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
  let doc = "Dump JSON schema for configuration to stdout and exit. Does not require FILE1.als or FILE2.als." in
  Arg.(value & flag & info ["dump-schema"] ~doc)

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
    `P "Validate a configuration file:";
    `Pre "$(cmd) --validate-config myconfig.json";
    `P "Dump preset configuration as JSON to stdout:";
    `Pre "$(cmd) --dump-preset full";
    `P "Dump preset configuration to file:";
    `Pre "$(cmd) --dump-preset compact > mypreset.json";
    `P "Configuration search order (when --config not specified):";
    `P "1. --preset PRESET (if specified)";
    `P "2. .alsdiff.json in directory of FILE2.als";
    `P "3. .alsdiff.json in git repository root";
    `P "4. .alsdiff.json in user's home directory (~)";
    `P "5. quiet preset (default)";
    `S "GIT DIFF DRIVER MODE";
    `P "$(cmd) can be used as a git external diff driver. When invoked with $(b,--git), \
        it expects exactly 7 positional arguments as passed by git:";
    `P "$(b,path old-file old-hex old-mode new-file new-hex new-mode)";
    `P "In git mode, exit code 0 means no differences found, and exit code 1 means \
        differences were found. This is compatible with git's $(b,trustExitCode) setting.";
    `P "Configure git to use alsdiff (.gitconfig):";
    `Pre "[diff \"als\"]";
    `Pre "    command = alsdiff --preset quiet --git";
    `Pre "    trustExitCode = true";
    `P "Configure .gitattributes:";
    `Pre "*.als diff=als";
    `P "Git mode with custom preset:";
    `Pre "$(cmd) --preset inline --git path old-file old-hex old-mode new-file new-hex new-mode";
    `P "Git mode with config file:";
    `Pre "$(cmd) --config myconfig.json --git path old-file old-hex old-mode new-file new-hex new-mode";
    `S Manpage.s_options;
    `P "$(b,--config FILE) loads configuration from JSON file. Takes precedence over auto-discovery. The --preset option is ignored when --config is specified. Individual CLI options override values from config file.";
    `P "$(b,--preset PRESET) sets the output detail preset. Available presets: $(b,compact), $(b,composer), $(b,full), $(b,mixing), $(b,quiet) (default), $(b,verbose). Takes precedence over auto-discovery but ignored when --config is specified.";
    `P "$(b,--prefix-added PREFIX) overrides prefix for added items from config file.";
    `P "$(b,--prefix-removed PREFIX) overrides prefix for removed items from config file.";
    `P "$(b,--prefix-modified PREFIX) overrides prefix for modified items from config file.";
    `P "$(b,--prefix-unchanged PREFIX) overrides prefix for unchanged items from config file.";
    `P "$(b,--note-name-style STYLE) overrides note name style from config file.";
    `P "$(b,--max-collection-items N) overrides max collection items from config file.";
    `P "$(b,--dump-preset PRESET) dumps preset configuration as JSON to stdout and exits. Same format as --config file. Available presets: $(b,compact), $(b,composer), $(b,full), $(b,inline), $(b,mixing), $(b,quiet), $(b,verbose).";
    `P "$(b,--dump-schema) dumps JSON schema for configuration to stdout and exits.";
    `P "$(b,--validate-config FILE) validates a configuration file against the JSON schema and exits. Useful for checking config files before use.";
    `P "$(b,--git) enables git external diff driver mode. Expects exactly 7 positional arguments from git. Exit code 0 = no differences, 1 = differences found. All other options work in git mode.";
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/krfantasy/alsdiff/issues";
  ] in
  let exits =
    Cmd.Exit.info 0 ~doc:"success (normal mode), or no differences found (git mode with trustExitCode)." ::
    Cmd.Exit.info 1 ~doc:"differences found (git mode with trustExitCode), or known errors in normal mode." ::
    Cmd.Exit.info 2 ~doc:"invalid arguments (e.g., wrong number of positional args in git mode)." ::
    List.filter (fun e -> Cmd.Exit.info_code e <> 123 && Cmd.Exit.info_code e <> 0) Cmd.Exit.defaults
  in
  Cmd.make (Cmd.info "alsdiff" ~version:(match version () with
      | None -> "dev"
      | Some v -> Version.to_string v) ~doc ~man ~exits) @@
  let+ positional_args and+ git_mode and+ config_file and+ preset and+ dump_preset and+ prefix_added and+ prefix_removed and+ prefix_modified and+ prefix_unchanged and+ note_name_style and+ max_collection_items and+ dump_schema and+ validate_config in
  let cfg = { positional_args; git_mode; config_file; preset; dump_preset; prefix_added; prefix_removed; prefix_modified; prefix_unchanged; note_name_style; max_collection_items; dump_schema; validate_config } in
  config_ref := Some cfg;
  ()

let main () =
  Printexc.record_backtrace true;

  (* Helper to run the command safely *)
  let safe_run cmd_term =
    (* Helper to get error exit code based on mode *)
    let error_exit_code () =
      match !config_ref with
      | Some cfg when cfg.git_mode -> 2
      | _ -> 1
    in
    try
      let exit_code = Cmd.eval cmd_term in
      if exit_code = 0 then
        match !config_ref with
        | None -> 0
        | Some cfg ->
          (* Handle --dump-preset first *)
          (match cfg.dump_preset with
           | Some preset ->
             let preset_config = match preset with
               | `Compact -> Text_renderer.compact
               | `Composer -> Text_renderer.composer
               | `Full -> Text_renderer.full
               | `Inline -> Text_renderer.inline
               | `Mixing -> Text_renderer.mixing
               | `Quiet -> Text_renderer.quiet
               | `Verbose -> Text_renderer.verbose
             in
             let json = Text_renderer.detail_config_to_yojson_with_schema preset_config in
             print_endline (Yojson.Safe.pretty_to_string json);
             0
           | None ->
             (* Handle --validate-config *)
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
                if cfg.dump_schema then begin
                  print_endline (Text_renderer.detail_config_schema_to_string ());
                  0
                end else begin
                  (* Normal diff operation - validate args based on mode *)
                  let has_valid_args =
                    if cfg.git_mode then
                      (* Git mode needs exactly 7 positional args *)
                      List.length cfg.positional_args = 7
                    else
                      (* Normal mode needs exactly 2 positional args *)
                      List.length cfg.positional_args = 2
                  in
                  if has_valid_args then
                    Eio_main.run @@ fun env ->
                    let domain_mgr = Eio.Stdenv.domain_mgr env in
                    diff_cmd ~config:cfg ~domain_mgr
                  else if cfg.git_mode then begin
                    Fmt.epr "Error: --git mode requires exactly 7 positional arguments@.";
                    Fmt.epr "Usage: alsdiff --git path old-file old-hex old-mode new-file new-hex new-mode@.";
                    2
                  end else begin
                    Fmt.epr "Error: FILE1.als and FILE2.als are required for diff@.";
                    Fmt.epr "Use --dump-schema to generate configuration schema without files.@.";
                    Fmt.epr "Use --dump-preset PRESET to dump a preset configuration as JSON.@.";
                    Fmt.epr "Use --validate-config FILE to validate a configuration file.@.";
                    1
                  end
                end))
      else
        exit_code
    with
    | File.File_error (file, msg) ->
      let bt = Printexc.get_backtrace () in
      Fmt.epr "Error: Failed to process file '%s': %s@." file msg;
      Fmt.epr "%s@." bt;
      error_exit_code ()
    | Xml.Xml_error (xml, msg) ->
      let bt = Printexc.get_backtrace () in
      Fmt.epr "Error: Invalid XML format: %s@.%a@." msg Xml.pp xml;
      Fmt.epr "%s@." bt;
      error_exit_code ()
    | Upath.Path_not_found (path, xml) ->
      let bt = Printexc.get_backtrace () in
      Fmt.epr "Error: Required path '%s' not found in @.%a@." path Xml.pp xml;
      Fmt.epr "%s@." bt;
      error_exit_code ()
    | Sys_error msg ->
      let bt = Printexc.get_backtrace () in
      Fmt.epr "System Error: %s@." msg;
      Fmt.epr "%s@." bt;
      error_exit_code ()
    | Failure msg ->
      let bt = Printexc.get_backtrace () in
      Fmt.epr "Error: %s@." msg;
      Fmt.epr "%s@." bt;
      error_exit_code ()
    | exn ->
      let bt = Printexc.get_backtrace () in
      Fmt.epr "Unexpected error: %s@.%s@." (Printexc.to_string exn) bt;
      Fmt.epr "Please report this bug at https://github.com/krfantasy/alsdiff/issues@.";
      error_exit_code ()
  in

  safe_run cmd

let () =
  if !Sys.interactive then () else exit (main ())
