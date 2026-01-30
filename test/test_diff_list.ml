open Alcotest
open Alsdiff_base.Diff

(* Create a simple DIFFABLE_EQ module for int *)
module IntDiffEq = struct
  type t = int
  let equal a b = a = b
  module Patch = struct
    type t = int atomic_patch
    let is_empty = function {oldval; newval} -> oldval = newval
  end
  let diff a b = { oldval = a; newval = b}
end

(** Test helpers *)
let pp_atomic_patch pp_val ppf {oldval; newval} =
  Fmt.pf ppf "{oldval=%a; newval=%a}" pp_val oldval pp_val newval

let change_testable (type a p) (pp_a : a Fmt.t) (pp_p : p Fmt.t) (eq_a : a -> a -> bool) (eq_p : p -> p -> bool) =
  let pp ppf = function
    | `Unchanged -> Fmt.pf ppf "`Unchanged"
    | `Added v -> Fmt.pf ppf "`Added %a" pp_a v
    | `Removed v -> Fmt.pf ppf "`Removed %a" pp_a v
    | `Modified p -> Fmt.pf ppf "`Modified %a" pp_p p
  in
  let eq x y = match x, y with
    | `Unchanged, `Unchanged -> true
    | `Added a, `Added b -> eq_a a b
    | `Removed a, `Removed b -> eq_a a b
    | `Modified a, `Modified b -> eq_p a b
    | _ -> false
  in
  Alcotest.testable pp eq

let int_change_testable = change_testable Fmt.int (pp_atomic_patch Fmt.int) Int.equal (fun a b -> a = b)
let string_change_testable = change_testable Fmt.string (pp_atomic_patch Fmt.string) String.equal (fun a b -> a = b)

(** Test basic Myers algorithm functionality *)
let test_diff_list_basic () =

  (* Test case 1: Empty lists *)
  let result1 = diff_list (module IntDiffEq) [] [] in
  check (list int_change_testable) "empty lists" [] result1;

  (* Test case 2: Adding elements *)
  let result2 = diff_list (module IntDiffEq) [] [1; 2; 3] in
  check (list int_change_testable) "add elements" [`Added 1; `Added 2; `Added 3] result2;

  (* Test case 3: Removing elements *)
  let result3 = diff_list (module IntDiffEq) [1; 2; 3] [] in
  check (list int_change_testable) "remove elements" [`Removed 1; `Removed 2; `Removed 3] result3;

  (* Test case 4: No changes *)
  let result4 = diff_list (module IntDiffEq) [1; 2; 3] [1; 2; 3] in
  check (list int_change_testable) "no changes" [`Unchanged; `Unchanged; `Unchanged] result4;

  (* Test case 5: Simple replacement *)
  let result5 = diff_list (module IntDiffEq) [1; 2; 3] [1; 4; 3] in
  check (list int_change_testable) "replacement" [`Unchanged; `Removed 2; `Added 4; `Unchanged] result5;

  (* Test case 6: Mixed operations *)
  let result6 = diff_list (module IntDiffEq) [1; 2; 3] [2; 4] in
  (* Should be: remove 1, unchanged 2, remove 3, add 4 *)
  check (list int_change_testable) "mixed operations" [`Removed 1; `Unchanged; `Removed 3; `Added 4] result6;

  ()

(* Create a DIFFABLE_EQ module for strings *)
module StringDiffEq = struct
  type t = string
  let equal = String.equal
  module Patch = struct
    type t = string atomic_patch
    let is_empty = function {oldval; newval} -> oldval = newval
  end
  let diff a b = { oldval = a; newval = b}
end

(** Test Myers algorithm with strings *)
let test_diff_list_strings () =
  (* Test case 1: String replacement *)
  let result1 = diff_list (module StringDiffEq) ["a"; "b"; "c"] ["a"; "x"; "c"] in
  check (list string_change_testable) "string replacement" [`Unchanged; `Removed "b"; `Added "x"; `Unchanged] result1;

  (* Test case 2: String addition *)
  let result2 = diff_list (module StringDiffEq) ["hello"] ["hello"; "world"] in
  check (list string_change_testable) "string addition" [`Unchanged; `Added "world"] result2;

  (* Test case 3: String removal *)
  let result3 = diff_list (module StringDiffEq) ["foo"; "bar"; "baz"] ["foo"] in
  check (list string_change_testable) "string removal" [`Unchanged; `Removed "bar"; `Removed "baz"] result3;

  ()

(** Test Myers algorithm edge cases *)
let test_diff_list_edge_cases () =

  (* Test case 1: Single element identical *)
  let result1 = diff_list (module IntDiffEq) [42] [42] in
  check (list int_change_testable) "single identical" [`Unchanged] result1;

  (* Test case 2: Single element different *)
  let result2 = diff_list (module IntDiffEq) [1] [2] in
  check (list int_change_testable) "single different" [`Removed 1; `Added 2] result2;

  (* Test case 3: Duplicate elements *)
  let result3 = diff_list (module IntDiffEq) [1; 1; 1] [1; 1] in
  check (list int_change_testable) "duplicates" [`Unchanged; `Unchanged; `Removed 1] result3;

  (* Test case 4: Reverse order *)
  let result4 = diff_list (module IntDiffEq) [1; 2; 3] [3; 2; 1] in
  check (list int_change_testable) "reverse" [`Removed 1; `Removed 2; `Unchanged; `Added 2; `Added 1] result4;

  (* Test case 5: Completely different *)
  let result5 = diff_list (module IntDiffEq) [1; 2; 3] [4; 5; 6] in
  check (list int_change_testable) "completely different"
    [`Removed 1; `Removed 2; `Removed 3; `Added 4; `Added 5; `Added 6] result5;

  ()

(** Test Myers algorithm optimal edit distance *)
let test_diff_list_optimal () =

  (* Test case 1: Move detection [1,2,3] -> [2,3,1] *)
  let result1 = diff_list (module IntDiffEq) [1; 2; 3] [2; 3; 1] in
  (* Myers should find optimal solution with 4 operations: remove 1, unchanged 2, unchanged 3, add 1 *)
  check (list int_change_testable) "move detection" [`Removed 1; `Unchanged; `Unchanged; `Added 1] result1;

  (* Test case 2: Swap adjacent [1,2,3,4] -> [2,1,3,4] *)
  let result2 = diff_list (module IntDiffEq) [1; 2; 3; 4] [2; 1; 3; 4] in
  (* Should be: remove 1, unchanged 2, add 1, unchanged 3, unchanged 4 *)
  check (list int_change_testable) "adjacent swap" [`Removed 1; `Unchanged; `Added 1; `Unchanged; `Unchanged] result2;

  (* Test case 3: Complex rearrangement *)
  let result3 = diff_list (module IntDiffEq) [1; 2; 3; 4; 5] [3; 4; 1; 2; 5] in
  (* Myers should find minimal edit script *)
  check (list int_change_testable) "complex rearrangement"
    [`Removed 1; `Removed 2; `Unchanged; `Unchanged; `Added 1; `Added 2; `Unchanged] result3;
  ()



(** Test Myers algorithm with custom equality *)
let test_diff_list_custom () =
  let module CustomIntDiffEq = struct
    type t = int
    let equal a b = abs (a - b) <= 1  (* Equal if within 1 *)
    module Patch = struct
      type t = int atomic_patch
      let is_empty = function {oldval; newval} -> oldval = newval
    end
    let diff a b = { oldval = a; newval = b}
  end in

  let result = diff_list (module CustomIntDiffEq) [1; 5; 10] [2; 6; 10] in
  (* All should be unchanged since 1≈2, 5≈6, 10=10 *)
  (* All should be unchanged since 1≈2, 5≈6, 10=10 *)
  check (list int_change_testable) "custom equality unchanged" [`Unchanged; `Unchanged; `Unchanged] result;
  ()


(** Additional test to check correctness of Myers implementation *)
let test_myers_correctness () =
  (* Test case: [1; 2; 3; 4] -> [1; 3; 4; 5] *)
  let old_list = [1; 2; 3; 4] in
  let new_list = [1; 3; 4; 5] in

  let myers_result = diff_list (module IntDiffEq) old_list new_list in

  Printf.printf "Test case: [1; 2; 3; 4] -> [1; 3; 4; 5]\n";
  Printf.printf "Myers result length: %d\n" (List.length myers_result);

  (* Myers should produce reasonable number of operations *)
  (* Myers should produce reasonable number of operations *)
  (* 1->1(U), 2 removed, 3->3(U), 4->4(U), 5 added *)
  check (list int_change_testable) "Myers correctness"
    [`Unchanged; `Removed 2; `Unchanged; `Unchanged; `Added 5] myers_result;
  ()


(** Test merge_adjacent_changes function *)
let test_merge_adjacent_changes () =
  let diff a b = { oldval = a; newval = b } in

  (* Test case 1: Adjacent Removed+Added becomes Modified *)
  let changes1 = [`Removed 1; `Added 2; `Unchanged] in
  let result1 = merge_adjacent_changes ~diff changes1 in
  check (list int_change_testable) "adjacent removed+added"
    [`Modified { oldval = 1; newval = 2 }; `Unchanged] result1;

  (* Test case 2: Non-adjacent pairs stay separate *)
  let changes2 = [`Removed 1; `Unchanged; `Added 2] in
  let result2 = merge_adjacent_changes ~diff changes2 in
  check (list int_change_testable) "non-adjacent stays separate"
    [`Removed 1; `Unchanged; `Added 2] result2;

  (* Test case 3: Multiple adjacent pairs *)
  let changes3 = [`Removed 1; `Added 2; `Removed 3; `Added 4] in
  let result3 = merge_adjacent_changes ~diff changes3 in
  check (list int_change_testable) "multiple adjacent pairs"
    [`Modified { oldval = 1; newval = 2 }; `Modified { oldval = 3; newval = 4 }] result3;

  (* Test case 4: Empty list *)
  let result4 = merge_adjacent_changes ~diff [] in
  check (list int_change_testable) "empty list" [] result4;

  (* Test case 5: Only unchanged *)
  let changes5 = [`Unchanged; `Unchanged] in
  let result5 = merge_adjacent_changes ~diff changes5 in
  check (list int_change_testable) "only unchanged" [`Unchanged; `Unchanged] result5;

  (* Test case 6: Added then Removed (wrong order, should not merge) *)
  let changes6 = [`Added 2; `Removed 1] in
  let result6 = merge_adjacent_changes ~diff changes6 in
  check (list int_change_testable) "wrong order no merge"
    [`Added 2; `Removed 1] result6;

  ()


(** Test diff_list_merged - full integration test *)
let test_diff_list_merged () =
  (* Test case 1: Simple replacement [1;2;3] -> [4;2;3] *)
  let result1 = diff_list_merged (module IntDiffEq) [1; 2; 3] [4; 2; 3] in
  check (list int_change_testable) "simple replacement at start"
    [`Modified { oldval = 1; newval = 4 }; `Unchanged; `Unchanged] result1;

  (* Test case 2: Replacement in middle *)
  let result2 = diff_list_merged (module IntDiffEq) [1; 2; 3] [1; 5; 3] in
  check (list int_change_testable) "replacement in middle"
    [`Unchanged; `Modified { oldval = 2; newval = 5 }; `Unchanged] result2;

  (* Test case 3: No changes - should be same as diff_list *)
  let result3 = diff_list_merged (module IntDiffEq) [1; 2; 3] [1; 2; 3] in
  check (list int_change_testable) "no changes"
    [`Unchanged; `Unchanged; `Unchanged] result3;

  (* Test case 4: All replaced [1;2] -> [3;4]

     Note: We use semantic correctness checks rather than exact structural assertions
     because the Myers diff algorithm can produce multiple equally-valid minimal edit
     scripts for the same input. The linear-space variant (used by Git) may produce
     different orderings than the quadratic-space version depending on implementation
     details like diagonal iteration order. Both are correct as long as:
     - All old values appear in Removed or Modified operations
     - All new values appear in Added or Modified operations
     - The total edit distance is minimal

     See: https://blog.jcoglan.com/2017/03/22/myers-diff-in-linear-space-theory/ *)
  let result4 = diff_list_merged (module IntDiffEq) [1; 2] [3; 4] in

  (* Verify semantic correctness: all old and new values are accounted for *)
  let old_values = result4
    |> List.filter_map (function
        | `Removed v | `Modified {oldval=v; _} -> Some v
        | _ -> None)
    |> List.sort Int.compare in
  let new_values = result4
    |> List.filter_map (function
        | `Added v | `Modified {newval=v; _} -> Some v
        | _ -> None)
    |> List.sort Int.compare in

  (* All old values [1;2] should be present *)
  check (list int) "all replaced - all old values present" [1; 2] old_values;
  (* All new values [3;4] should be present *)
  check (list int) "all replaced - all new values present" [3; 4] new_values;
  ()


(** Alcotest test suite setup. *)
let () =
  run "Diff List Algorithms" [
    "Myers Diff", [
      test_case "Test basic Myers functionality" `Quick test_diff_list_basic;
      test_case "Test Myers with strings" `Quick test_diff_list_strings;
      test_case "Test edge cases" `Quick test_diff_list_edge_cases;
      test_case "Test optimal edit distance" `Quick test_diff_list_optimal;
      test_case "Test custom equality" `Quick test_diff_list_custom;
      test_case "Test correctness" `Quick test_myers_correctness;
    ];
    "Merge Adjacent Changes", [
      test_case "Test merge_adjacent_changes" `Quick test_merge_adjacent_changes;
      test_case "Test diff_list_merged" `Quick test_diff_list_merged;
    ]
  ]
