open Alsdiff_base.Equality

(** The payload for a `Modified` change, containing the old and new values. *)
type 'a modified = { old : 'a; new_ : 'a }

(** A patch for a simple value, representing the change from an old to a new value.

    USE CASES:
    - Simple/primitive values (int, string, float, bool)
    - Atomic fields that can be directly compared
    - When you only need: old value, new value, or unchanged
    - Examples: volume, pan, mute, solo, name, start_time, end_time
*)
type 'a flat_change = [
  | `Unchanged
  | `Added of 'a
  | `Removed of 'a
  | `Modified of 'a modified
]

(** A constrained variant that only includes unchanged and modified cases.
    This is compatible with flat_change due to structural subtyping.
*)
type 'a simple_flat_change = [
  | `Unchanged
  | `Modified of 'a modified
]

(** A patch for a complex or list-like value.
    The `Patched` constructor holds a dedicated patch type `'p` which describes
    the internal changes to the value `'a`.

    USE CASES:
    - Complex objects that have their own Patch.t type
    - When you need to distinguish "added/removed" vs "internally modified"
    - Examples: Loop (has Loop.Patch.t), Send (has Send.Patch.t)

*)
type ('a, 'p) structured_change = [
  | `Unchanged
  | `Added of 'a
  | `Removed of 'a
  | `Patched of 'p
]

(** A constrained variant that only includes unchanged and patched cases.
    This is compatible with structured_change due to structural subtyping.
*)
type 'p simple_structured_change = [
  | `Unchanged
  | `Patched of 'p
]

type ('a, 'p) collection_change = [
  | `Unchanged
  | `Added of 'a
  | `Removed of 'a
  | `Modified of 'a flat_change list
  | `Reordered of (int * int) list
  | `Patched of 'p
]

(* TODO: Jeez! Do I really need that many of '_change' types,
   just for PRECISELY distinguishing different kind of changes?
   Sounds pretty over-engineered for me.
   But since they are all polymorphic variants, the APIs rarely needs to updated.
*)

(**
   BEST PRACTICES FOR CHOOSING DIFF TYPES:

   1. flat_change - Use for:
      - Simple/primitive values (int, string, float, bool)
      - Atomic fields that can be directly compared
      - Examples: volume, pan, mute, solo, name, start_time, end_time

   2. structured_change - Use for:
      - Complex objects that have their own Patch.t type
      - When you need to distinguish "added/removed" vs "internally modified"
      - Examples: Loop (has Loop.Patch.t), Send (has Send.Patch.t)

   3. collection_change - Use for:
      - Advanced scenarios with complex collection operations
      - When you need explicit reordering/move tracking
      - When batch operations on collections are needed
      - NOTE: Currently not used in codebase, but available for future needs

   4. diff_list - Use for:
      - Set-based comparisons where order doesn't matter
      - Simple existence tracking (what was added/removed)

   5. diff_list_ord - Use for:
      - Ordered collections where sequence position matters
      - When you need to preserve order and track moves
      - Examples: MIDI notes in clips, automation points

   6. CHOOSING COLLECTION TYPES:
      - For flat_change list: Individual elements can be independently added/removed/modified
      - For structured_change list: Complex objects with internal patches in a collection
      - For collection_change: Advanced operations like reordering, batch ops

   EXAMPLES FROM CODEBASE:
      - Mixer.sends uses (Send.t, Send.Patch.t) structured_change list
      - MidiClip.notes uses MidiNote.t flat_change list (individual note tracking)
      - AudioClip.loop uses Loop.Patch.t (single complex object)
*)


(** A generic helper to find added and removed items between two lists.
    This is a simple set-based diff, not a sequence-based one.

    USE CASES:
    - Simple set operations where you only care about existence, not position
    - When order doesn't matter (e.g., set of tags, categories)
*)
let diff_list (type a) (module Eq : EQUALABLE with type t = a) (old_list : a list) (new_list : a list) =
  let old_seq = List.to_seq old_list in
  let new_seq = List.to_seq new_list in

  let removed = Seq.filter (fun old_item ->
    not (Seq.exists (Eq.equal old_item) new_seq)
  ) old_seq |> Seq.map (fun item -> `Removed item) in

  let added = Seq.filter (fun new_item ->
    not (Seq.exists (Eq.equal new_item) old_seq)
  ) new_seq |> Seq.map (fun item -> `Added item) in

  List.of_seq (Seq.append removed added)

(** A sequence-based diff algorithm that preserves order and can detect moves.

    Returns a list of changes with their positions.

    USE CASES:
    - Ordered collections where position matters
    - When you need to preserve sequence information
    - When moves/reordering should be tracked
    - Examples: notes in a MIDI clip, automation points, events list

    CONTRAST WITH diff_list:
    Use diff_list when: order/position doesn't matter
    Use diff_list_ord when: sequence is important
*)
let diff_list_ord (type a) (module Eq : EQUALABLE with type t = a) (old_list : a list) (new_list : a list) : a flat_change list =
  let old_len = List.length old_list in
  let new_len = List.length new_list in

  (* Handle edge cases *)
  if old_len = 0 then
    List.map (fun item -> `Added item) new_list
  else if new_len = 0 then
    List.map (fun item -> `Removed item) old_list
  else
    (* Convert lists to arrays for efficient indexing *)
    let old_arr = Array.of_list old_list in
    let new_arr = Array.of_list new_list in

    (* Create a matrix to store the longest common subsequence lengths *)
    let dp = Array.make_matrix (old_len + 1) (new_len + 1) 0 in

    (* Fill the DP matrix *)
    for i = 1 to old_len do
      for j = 1 to new_len do
        if Eq.equal old_arr.(i - 1) new_arr.(j - 1) then
          dp.(i).(j) <- dp.(i - 1).(j - 1) + 1
        else
          dp.(i).(j) <- max dp.(i - 1).(j) dp.(i).(j - 1)
      done
    done;
    (* Note the semicolon above - we're sequencing the for loops with the backtrack *)

    (* Backtrack to find the diff operations *)
    let rec backtrack i j acc =
      if i = 0 && j = 0 then
        acc
      else if i = 0 then
        backtrack i (j - 1) (`Added new_arr.(j - 1) :: acc)
      else if j = 0 then
        backtrack (i - 1) j (`Removed old_arr.(i - 1) :: acc)
      else if Eq.equal old_arr.(i - 1) new_arr.(j - 1) then
        backtrack (i - 1) (j - 1) (`Unchanged :: acc)
      else if dp.(i).(j - 1) >= dp.(i - 1).(j) then
        backtrack i (j - 1) (`Added new_arr.(j - 1) :: acc)
      else
        backtrack (i - 1) j (`Removed old_arr.(i - 1) :: acc)
    in

    backtrack old_len new_len []

(** Myers' O(ND) diff algorithm - based on Eugene W. Myers' 1986 paper.
    Returns a list of changes representing the shortest edit script.
    Time complexity: O((N+M)D) where D is the size of the edit script.
    Space complexity: O((N+M)D) for trace storage.
*)
let diff_list_myers (type a) (module Eq : EQUALABLE with type t = a) (old_list : a list) (new_list : a list) : a flat_change list =
  let old_arr = Array.of_list old_list in
  let new_arr = Array.of_list new_list in
  let n = Array.length old_arr in
  let m = Array.length new_arr in

  (* Handle edge cases *)
  if n = 0 then List.map (fun x -> `Added x) new_list
  else if m = 0 then List.map (fun x -> `Removed x) old_list
  else
    (* Myers O(ND) algorithm implementation *)
    let max_d = n + m in
    let offset = max_d in (* Offset to handle negative k indices *)

    (* V array stores the furthest x position for each k-line *)
    let v = Array.make (2 * max_d + 1) 0 in
    (* Trace array stores V states for backtracking *)
    let traces = Array.init (max_d + 1) (fun _ -> Array.make (2 * max_d + 1) 0) in

    (* Follow diagonal (matching elements) as far as possible *)
    let rec follow_snake x y =
      if x < n && y < m && Eq.equal old_arr.(x) new_arr.(y) then
        follow_snake (x + 1) (y + 1)
      else (x, y)
    in

    (* Forward search to find the shortest edit distance *)
    let rec search d =
      if d > max_d then failwith "Myers algorithm: exceeded maximum edit distance";

      (* Store current V state for backtracking *)
      traces.(d) <- Array.copy v;

      (* Search k-lines from -d to +d *)
      let rec search_k k =
        if k > d then false (* Not found at this edit distance *)
        else
          (* Calculate x position based on previous k-lines *)
          let x =
            if k = -d || (k <> d && v.(k - 1 + offset) < v.(k + 1 + offset)) then
              v.(k + 1 + offset) (* Move down (insertion) *)
            else
              v.(k - 1 + offset) + 1 (* Move right (deletion) *)
          in
          let y = x - k in

          (* Follow diagonal (snake) *)
          let x_end, y_end = follow_snake x y in
          v.(k + offset) <- x_end;

          (* Check if we've reached the end *)
          if x_end >= n && y_end >= m then true
          else search_k (k + 2)
      in

      if search_k (-d) then d (* Found solution at edit distance d *)
      else search (d + 1) (* Try next edit distance *)
    in

    (* Find the edit distance *)
    let edit_distance = search 0 in

    (* Backtrack to reconstruct the edit script *)
    let result = ref [] in

    let rec backtrack d x y =
      if d = 0 then
        (* At edit distance 0, everything is unchanged *)
        let rec add_unchanged i =
          if i >= 0 then (
            result := `Unchanged :: !result;
            add_unchanged (i - 1)
          )
        in
        add_unchanged (x - 1)
      else
        let prev_v = traces.(d) in
        let k = x - y in

        (* Determine which previous k-line we came from *)
        let prev_k =
          if k = -d || (k <> d && prev_v.(k - 1 + offset) < prev_v.(k + 1 + offset)) then
            k + 1 (* Came from insertion *)
          else
            k - 1 (* Came from deletion *)
        in

        let prev_x = prev_v.(prev_k + offset) in
        let prev_y = prev_x - prev_k in

        (* Calculate where the snake started *)
        let snake_start_x, snake_start_y =
          if prev_k = k - 1 then (prev_x + 1, prev_y) (* After deletion *)
          else (prev_x, prev_y + 1) (* After insertion *)
        in

        (* Add unchanged elements from the snake *)
        let rec add_snake curr_x curr_y =
          if curr_x > snake_start_x && curr_y > snake_start_y then (
            result := `Unchanged :: !result;
            add_snake (curr_x - 1) (curr_y - 1)
          )
        in
        add_snake x y;

        (* Add the edit operation *)
        if prev_k = k - 1 then
          (* Deletion *)
          result := `Removed old_arr.(snake_start_x - 1) :: !result
        else
          (* Insertion *)
          result := `Added new_arr.(snake_start_y - 1) :: !result;

        (* Continue backtracking *)
        backtrack (d - 1) prev_x prev_y
    in

    backtrack edit_distance n m;
    List.rev !result


let diff_list_id (type a) (module ID : IDENTIFIABLE with type t = a)
    (old_list : a list) (new_list : a list) : a flat_change list =
    let old_seq = List.to_seq old_list in
    let new_seq = List.to_seq new_list in

    (* Find items that were removed (no matching ID in new list) *)
    let removed = Seq.filter (fun old_item ->
      not (Seq.exists (ID.has_same_id old_item) new_seq)
    ) old_seq |> Seq.map (fun item -> `Removed item) in

    (* Find items that were added (no matching ID in old list) *)
    let added = Seq.filter (fun new_item ->
      not (Seq.exists (ID.has_same_id new_item) old_seq)
    ) new_seq |> Seq.map (fun item -> `Added item) in

    (* Find items with same ID that may have been modified or unchanged *)
    let modified_or_unchanged = Seq.filter_map (fun new_item ->
      match Seq.find (ID.has_same_id new_item) old_seq with
      | Some old_item ->
          if old_item = new_item then
            Some `Unchanged
          else
            Some (`Modified { old = old_item; new_ = new_item })
      | None -> None  (* already handled as added *)
    ) new_seq in

    List.of_seq (Seq.append removed (Seq.append added modified_or_unchanged))

(** A sequence-based diff algorithm that preserves order and can detect moves.
    Like diff_list_ord, but also tracks identifiers to produce Modified changes.
    Items with the same ID are considered the same element, even if their values differ.
*)
let diff_list_ord_id (type a) (module ID : IDENTIFIABLE with type t = a) (old_list : a list) (new_list : a list) : a flat_change list =
  let old_len = List.length old_list in
  let new_len = List.length new_list in

  (* Handle edge cases *)
  if old_len = 0 then
    List.map (fun item -> `Added item) new_list
  else if new_len = 0 then
    List.map (fun item -> `Removed item) old_list
  else
    (* Convert lists to arrays for efficient indexing *)
    let old_arr = Array.of_list old_list in
    let new_arr = Array.of_list new_list in

    (* Create a matrix to store the longest common subsequence lengths *)
    let dp = Array.make_matrix (old_len + 1) (new_len + 1) 0 in

    (* Fill the DP matrix using identifier matching *)
    for i = 1 to old_len do
      for j = 1 to new_len do
        if ID.has_same_id old_arr.(i - 1) new_arr.(j - 1) then
          dp.(i).(j) <- dp.(i - 1).(j - 1) + 1
        else
          dp.(i).(j) <- max dp.(i - 1).(j) dp.(i).(j - 1)
      done
    done;

    (* Backtrack to find the diff operations *)
    let rec backtrack i j acc =
      if i = 0 && j = 0 then
        acc
      else if i = 0 then
        backtrack i (j - 1) (`Added new_arr.(j - 1) :: acc)
      else if j = 0 then
        backtrack (i - 1) j (`Removed old_arr.(i - 1) :: acc)
      else if ID.has_same_id old_arr.(i - 1) new_arr.(j - 1) then
        (* Same identifier - check if values are equal or modified *)
        let change =
          if old_arr.(i - 1) = new_arr.(j - 1) then
            `Unchanged
          else
            `Modified { old = old_arr.(i - 1); new_ = new_arr.(j - 1) }
        in
        backtrack (i - 1) (j - 1) (change :: acc)
      else if dp.(i).(j - 1) >= dp.(i - 1).(j) then
        backtrack i (j - 1) (`Added new_arr.(j - 1) :: acc)
      else
        backtrack (i - 1) j (`Removed old_arr.(i - 1) :: acc)
    in

    backtrack old_len new_len []


let identify x = x [@@inline]


module type PATCH = sig
  type t
  val is_empty : t -> bool
end

(* Utility functions *)
let simple_structured_change_of_patch (type a) (module P : PATCH with type t = a)
    (x : a) : a simple_structured_change =
  if P.is_empty x then
    `Unchanged
  else
    `Patched x
