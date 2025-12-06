open Equality

module type PATCH = sig
  type t
  val is_empty : t -> bool
end

module type DIFFABLE_ID = sig
  type t
  include IDENTIFIABLE with type t := t

  module Patch : PATCH

  val diff : t -> t -> Patch.t
end

module type DIFFABLE_EQ = sig
  type t
  include EQUALABLE with type t := t

  module Patch : PATCH

  val diff : t -> t -> Patch.t
end

(** The unified type to describe the change of an value.
    TODO: adding a [`Moved] or [`Reordered] variant,
    currently the Myers diff algorithm can't really detect an item moved/reordered in a sequence.
*)
type ('a, 'p) change = [
  | `Unchanged
  | `Added of 'a
  | `Removed of 'a
  | `Modified of 'p
]

type 'a atomic_patch = { oldval : 'a; newval : 'a }
type 'a atomic_change = ('a, 'a atomic_patch) change


type 'p update = [
  | `Unchanged
  | `Modified of 'p
]

type 'a atomic_update = [
  | `Unchanged
  | `Modified of 'a atomic_patch
]

let diff_value ~equal ~diff old_value new_value =
  if equal old_value new_value then
    `Unchanged
  else
    `Modified (diff old_value new_value)

let diff_value_opt ~diff_some old_value new_value =
  match (old_value, new_value) with
  | (Some oldval, None) -> `Removed oldval
  | (None, Some newval) -> `Added newval
  | (Some oldval, Some newval) -> diff_some oldval newval
  | (None, None) -> `Unchanged

let diff_atomic_value (type a) (module EQ : EQUALABLE with type t = a)
    old_value new_value : a atomic_update =
  diff_value ~equal:EQ.equal ~diff:(fun oldval newval -> { oldval; newval }) old_value new_value

let diff_atomic_value_opt (type a) (module EQ : EQUALABLE with type t = a)
    (old_value : a option) (new_value : a option) : a atomic_change =
  diff_value_opt
    ~diff_some:(fun o n -> (diff_atomic_value (module EQ) o n :> a atomic_change))
    old_value new_value


let diff_complex_value (type a p)
    (module EQ : DIFFABLE_EQ with type t = a and type Patch.t = p)
    (old_value : a)
    (new_value : a) : p update =
  diff_value ~equal:EQ.equal ~diff:EQ.diff old_value new_value

let diff_complex_value_id (type a p)
    (module ID : DIFFABLE_ID with type t = a and type Patch.t = p)
    (old_value : a)
    (new_value : a) : p update =
  diff_value ~equal:ID.has_same_id ~diff:ID.diff old_value new_value

let diff_complex_value_opt (type a p)
    (module EQ : DIFFABLE_EQ with type t = a and type Patch.t = p)
    (old_value : a option)
    (new_value : a option) : (a, p) change =
  diff_value_opt
    ~diff_some:(fun o n -> (diff_complex_value (module EQ) o n :> (a, p) change))
    old_value new_value


let diff_complex_value_id_opt (type a p)
    (module ID : DIFFABLE_ID with type t = a and type Patch.t = p)
    (old_value : a option)
    (new_value : a option) : (a, p) change =
  diff_value_opt
    ~diff_some:(fun o n -> (diff_complex_value_id (module ID) o n :> (a, p) change))
    old_value new_value


(* Module type for a hashable type, used by diff_set_generic *)
module type HASHER = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end


(** Generic Myers algorithm for ordered list diffing.

    @param compare Function to compare elements for equality
    @param on_match Function to generate change type for matching elements
    @param old_list Original list
    @param new_list Modified list
    @return List of flat changes representing the minimal edit sequence
*)
let diff_list_generic (type a p)
    ~(compare: a -> a -> bool)
    ~(on_match: a -> a -> (a, p) change)
    (old_list : a list) (new_list : a list) : (a, p) change list =
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
      if x < n && y < m && compare old_arr.(x) new_arr.(y) then
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
            result := on_match old_arr.(i) new_arr.(i) :: !result;
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
            result := on_match old_arr.(curr_x - 1) new_arr.(curr_y - 1) :: !result;
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
    !result


(** Myers' O(ND) diff algorithm - based on Eugene W. Myers' 1986 paper.
    Returns a list of changes representing the shortest edit script.
    Time complexity: O((N+M)D) where D is the size of the edit script.
    Space complexity: O((N+M)D) for trace storage.
*)
let diff_list (type a p) (module EQ : DIFFABLE_EQ with type t = a and type Patch.t = p) (old_list : a list) (new_list : a list) : (a, p) change list =
  diff_list_generic
    ~compare:EQ.equal
    ~on_match:(fun old_item new_item ->
      if EQ.equal old_item new_item then
        `Unchanged
      else
        `Modified (EQ.diff old_item new_item)
    )
    old_list new_list

let diff_list_id (type a p) (module ID : DIFFABLE_ID with type t = a and type Patch.t = p) (old_list : a list) (new_list : a list) : (a, p) change list =
  diff_list_generic
    ~compare:ID.has_same_id
    ~on_match:(fun old_item new_item ->
      if ID.equal old_item new_item then
        `Unchanged
      else
        `Modified (ID.diff old_item new_item)
    )
    old_list new_list

(* Utility functions *)
let update_of_patch (type a) (module P : PATCH with type t = a)
    (x : a) : a update =
  if P.is_empty x then
    `Unchanged
  else
    `Modified x

let update_of_atomic (type a p)
    (module D : DIFFABLE_EQ with type t = a and type Patch.t = p)
    (x : a atomic_update) : p update =
  match x with
  | `Modified { oldval; newval } -> `Modified (D.diff oldval newval)
  | `Unchanged -> `Unchanged

let change_of_atomic (type a p)
    (module D : DIFFABLE_EQ with type t = a and type Patch.t = p)
    (x : a atomic_change) : (a, p) change =
  match x with
  | `Added a -> `Added a
  | `Removed a -> `Removed a
  | `Unchanged -> `Unchanged
  | `Modified { oldval; newval } -> `Modified (D.diff oldval newval)


(** Post-process a change list to merge adjacent Removed+Added pairs into Modified.

    This enables replacement detection for anonymous sequences (without IDs) by
    converting patterns like [`Removed old; `Added new] into [`Modified patch].

    The merging only happens for immediately adjacent pairs. For example:
    - [`Removed 1; `Added 2; `Unchanged] becomes [`Modified {1,2}; `Unchanged]
    - [`Removed 1; `Unchanged; `Added 2] stays unchanged (not adjacent)

    @param diff Function to create a patch from old and new values
    @param changes The change list from Myers diff
    @return Change list with adjacent Removed+Added pairs merged into Modified
*)
let merge_adjacent_changes (type a p)
    ~(diff : a -> a -> p)
    (changes : (a, p) change list) : (a, p) change list =
  let rec aux = function
    | `Removed old :: `Added new_ :: rest ->
        `Modified (diff old new_) :: aux rest
    | x :: rest -> x :: aux rest
    | [] -> []
  in
  aux changes


(** Convenience function combining diff_list with merge_adjacent_changes.

    This provides replacement detection for equality-based diffing by first
    computing the Myers diff, then merging adjacent Removed+Added pairs.

    Note: This may produce different results than diff_list for the same input,
    as adjacent insert+delete pairs are collapsed into modifications.
*)
let diff_list_merged (type a p)
    (module EQ : DIFFABLE_EQ with type t = a and type Patch.t = p)
    (old_list : a list) (new_list : a list) : (a, p) change list =
  diff_list (module EQ) old_list new_list
  |> merge_adjacent_changes ~diff:EQ.diff
