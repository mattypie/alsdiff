(** Estimate buffer size based on compressed file size and typical compression ratios.
   Ableton Live sets typically compress around 10-20:1, so we use a conservative
   15:1 estimate with minimum buffer sizes to handle small files efficiently.
*)
let estimate_buffer_size compressed_size =
  let min_buffer = 65536 in (* 64KB minimum for small files *)
  let estimated_expansion = 15 in (* Conservative 15:1 compression ratio estimate *)
  let estimated_size = compressed_size * estimated_expansion in
  max min_buffer (min estimated_size (16*1024*1024)) (* Cap at 16MB for typical Ableton projects *)

(** Decompress the .als file with [filename] and return its contents as a string. *)
let decompress_als_to_string filename =
  let compressed_size = Unix.stat filename |> (fun st -> st.st_size) in
  let buffer = Buffer.create (estimate_buffer_size compressed_size) in
  let gz_in = Gzip.open_in filename in
  let chunk = Bytes.create 8192 in
  let rec copy_loop () =
    let bytes_read = Gzip.input gz_in chunk 0 (Bytes.length chunk) in
    if bytes_read > 0 then (
      Buffer.add_subbytes buffer chunk 0 bytes_read;
      copy_loop ()
    )
  in
  (try copy_loop () with End_of_file -> ());
  Gzip.close_in gz_in;
  Buffer.contents buffer

let decompress_als filename =
  let basename = Filename.basename filename |> Filename.remove_extension in
  let temp_file = Filename.temp_file basename ".xml" in
  let gz_in = Gzip.open_in filename in
  let out_chan = open_out temp_file in
  let buffer = Bytes.create 4096 in
  let rec copy_loop () =
    let bytes_read = Gzip.input gz_in buffer 0 (Bytes.length buffer) in
    if bytes_read > 0 then (
      output out_chan buffer 0 bytes_read;
      copy_loop ()
    )
  in
  (try copy_loop () with End_of_file -> ());
  Gzip.close_in gz_in;
  close_out out_chan;
  temp_file

(** open the .als file with [filename], and return the parsed XML tree. *)
let open_als filename =
  filename
  |> decompress_als_to_string
  |> Xml.read_string

let time_it (f : unit -> 'a)  =
  let start_time = Sys.time() in
  let result = f () in
  let end_time = Sys.time() in
  Printf.printf "Execute time: %f seconds\n" (end_time -. start_time);
  result
