open Alcotest
open Alsdiff_base.Equality
open Alsdiff_live.Diff

(** Test basic Myers algorithm functionality *)
let test_diff_list_myers_basic () =
  let (module IntEq) = (module IntEq : EQUALABLE with type t = int) in

  (* Test case 1: Empty lists *)
  let result1 = diff_list_myers (module IntEq) [] [] in
  check int "empty lists" 0 (List.length result1);

  (* Test case 2: Adding elements *)
  let result2 = diff_list_myers (module IntEq) [] [1; 2; 3] in
  check int "add elements length" 3 (List.length result2);
  check bool "all added" true (List.for_all (function `Added _ -> true | _ -> false) result2);

  (* Test case 3: Removing elements *)
  let result3 = diff_list_myers (module IntEq) [1; 2; 3] [] in
  check int "remove elements length" 3 (List.length result3);
  check bool "all removed" true (List.for_all (function `Removed _ -> true | _ -> false) result3);

  (* Test case 4: No changes *)
  let result4 = diff_list_myers (module IntEq) [1; 2; 3] [1; 2; 3] in
  check int "no changes length" 3 (List.length result4);
  check bool "all unchanged" true (List.for_all (function `Unchanged -> true | _ -> false) result4);

  (* Test case 5: Simple replacement *)
  let result5 = diff_list_myers (module IntEq) [1; 2; 3] [1; 4; 3] in
  check int "replacement length" 4 (List.length result5);

  (* Test case 6: Mixed operations *)
  let result6 = diff_list_myers (module IntEq) [1; 2; 3] [2; 4] in
  (* Should be: remove 1, unchanged 2, remove 3, add 4 *)
  check int "mixed operations length" 4 (List.length result6);

  ()

(** Test Myers algorithm with strings *)
let test_diff_list_myers_strings () =
  let module StringEq = struct
    type t = string
    let equal = String.equal
  end in
  let (module StrEq) = (module StringEq : EQUALABLE with type t = string) in

  (* Test case 1: String replacement *)
  let result1 = diff_list_myers (module StrEq) ["a"; "b"; "c"] ["a"; "x"; "c"] in
  check int "string replacement length" 4 (List.length result1);

  (* Test case 2: String addition *)
  let result2 = diff_list_myers (module StrEq) ["hello"] ["hello"; "world"] in
  check int "string addition length" 2 (List.length result2);

  (* Test case 3: String removal *)
  let result3 = diff_list_myers (module StrEq) ["foo"; "bar"; "baz"] ["foo"] in
  check int "string removal length" 3 (List.length result3);

  ()

(** Test Myers algorithm edge cases *)
let test_diff_list_myers_edge_cases () =
  let (module IntEq) = (module IntEq : EQUALABLE with type t = int) in

  (* Test case 1: Single element identical *)
  let result1 = diff_list_myers (module IntEq) [42] [42] in
  check int "single identical length" 1 (List.length result1);
  check bool "single identical unchanged"
    true (match result1 with [`Unchanged] -> true | _ -> false);

  (* Test case 2: Single element different *)
  let result2 = diff_list_myers (module IntEq) [1] [2] in
  check int "single different length" 2 (List.length result2);

  (* Test case 3: Duplicate elements *)
  let result3 = diff_list_myers (module IntEq) [1; 1; 1] [1; 1] in
  check int "duplicates length" 3 (List.length result3);

  (* Test case 4: Reverse order *)
  let result4 = diff_list_myers (module IntEq) [1; 2; 3] [3; 2; 1] in
  check int "reverse length" 5 (List.length result4);

  (* Test case 5: Completely different *)
  let result5 = diff_list_myers (module IntEq) [1; 2; 3] [4; 5; 6] in
  check int "completely different length" 6 (List.length result5);

  ()

(** Test Myers algorithm optimal edit distance *)
let test_diff_list_myers_optimal () =
  let (module IntEq) = (module IntEq : EQUALABLE with type t = int) in

  (* Test case 1: Move detection [1,2,3] -> [2,3,1] *)
  let result1 = diff_list_myers (module IntEq) [1; 2; 3] [2; 3; 1] in
  (* Myers should find optimal solution with 4 operations: remove 1, unchanged 2, unchanged 3, add 1 *)
  check int "move detection length" 4 (List.length result1);

  (* Test case 2: Swap adjacent [1,2,3,4] -> [2,1,3,4] *)
  let result2 = diff_list_myers (module IntEq) [1; 2; 3; 4] [2; 1; 3; 4] in
  (* Should be: remove 1, unchanged 2, add 1, unchanged 3, unchanged 4 *)
  check int "adjacent swap length" 5 (List.length result2);

  (* Test case 3: Complex rearrangement *)
  let result3 = diff_list_myers (module IntEq) [1; 2; 3; 4; 5] [3; 4; 1; 2; 5] in
  (* Myers should find minimal edit script *)
  check int "complex rearrangement length" 7 (List.length result3);

  ()

(** Test Myers algorithm vs LCS algorithm comparison *)
let test_diff_list_myers_vs_lcs () =
  let (module IntEq) = (module IntEq : EQUALABLE with type t = int) in

  let test_cases = [
    ([1; 2; 3], [1; 4; 3]);  (* Simple replacement *)
    ([1; 2; 3; 4], [2; 3; 1; 4]);  (* Move *)
    ([1; 2; 1; 2], [2; 1; 2; 1]);  (* Duplicates with moves *)
  ] in

  List.iteri (fun i (old_list, new_list) ->
    let myers_result = diff_list_myers (module IntEq) old_list new_list in
    let lcs_result = diff_list_ord (module IntEq) old_list new_list in

    let myers_ops = List.length myers_result in
    let lcs_ops = List.length lcs_result in

    (* Myers should never produce more operations than LCS *)
    check bool "myers <= lcs" (myers_ops <= lcs_ops) true;

    Printf.printf "Test case %d: Myers=%d, LCS=%d\n" (i+1) myers_ops lcs_ops;
  ) test_cases;

  ()

(** Test Myers algorithm with custom equality *)
let test_diff_list_myers_custom () =
  let module CustomInt = struct
    type t = int
    let equal a b = abs (a - b) <= 1  (* Equal if within 1 *)
  end in

  let (module CustomEq) = (module CustomInt : EQUALABLE with type t = int) in

  let result = diff_list_myers (module CustomEq) [1; 5; 10] [2; 6; 10] in
  (* All should be unchanged since 1≈2, 5≈6, 10=10 *)
  check int "custom equality unchanged" 3 (List.length result);
  check bool "custom equality all unchanged"
    true (List.for_all (function `Unchanged -> true | _ -> false) result);

  ()

(** Additional test to check correctness of Myers implementation *)
let test_myers_correctness () =
  let (module IntEq) = (module IntEq : EQUALABLE with type t = int) in

  (* Test case: [1; 2; 3; 4] -> [1; 3; 4; 5] *)
  let old_list = [1; 2; 3; 4] in
  let new_list = [1; 3; 4; 5] in

  let myers_result = diff_list_myers (module IntEq) old_list new_list in
  let lcs_result = diff_list_ord (module IntEq) old_list new_list in

  Printf.printf "Test case: [1; 2; 3; 4] -> [1; 3; 4; 5]\n";
  Printf.printf "Myers result length: %d\n" (List.length myers_result);
  Printf.printf "LCS result length: %d\n" (List.length lcs_result);

  (* Myers should have optimal edit distance - let's check the lengths *)
  check bool "Myers should have equal or fewer ops than LCS" (List.length myers_result <= List.length lcs_result) true;

  ()

(** Helper to create a testable for 'a flat_change list *)
let flat_change_list_testable (type a) (elt_testable : a Alcotest.testable) : a flat_change list Alcotest.testable =
  let pp_flat_change ppf = function
    | `Unchanged -> Fmt.string ppf "Unchanged"
    | `Added x -> Fmt.pf ppf "Added %a" (Alcotest.pp elt_testable) x
    | `Removed x -> Fmt.pf ppf "Removed %a" (Alcotest.pp elt_testable) x
    | `Modified m -> Fmt.pf ppf "Modified (%a -> %a)" (Alcotest.pp elt_testable) m.old (Alcotest.pp elt_testable) m.new_
  in
  let equal_flat_change a b =
    match a, b with
    | `Unchanged, `Unchanged -> true
    | `Added x, `Added y -> Alcotest.equal elt_testable x y
    | `Removed x, `Removed y -> Alcotest.equal elt_testable x y
    | `Modified m1, `Modified m2 -> Alcotest.equal elt_testable m1.old m2.old && Alcotest.equal elt_testable m1.new_ m2.new_
    | _ -> false
  in
  Alcotest.(list (testable pp_flat_change equal_flat_change))

let int_fc_list = flat_change_list_testable Alcotest.int
let string_fc_list = flat_change_list_testable Alcotest.string

(** Test the sequence-based diff function *)
let test_diff_list_ord_basic () =
  (* Test with simple integer lists *)
  let (module IntEq) = (module IntEq : EQUALABLE with type t = int) in

  (* Test case 1: Empty lists *)
  let result1 = diff_list_ord (module IntEq) [] [] in
  check int_fc_list "empty lists" [] result1;

  (* Test case 2: Adding elements *)
  let result2 = diff_list_ord (module IntEq) [] [1; 2; 3] in
  check int_fc_list "add elements" [`Added 1; `Added 2; `Added 3] result2;

  (* Test case 3: Removing elements *)
  let result3 = diff_list_ord (module IntEq) [1; 2; 3] [] in
  check int_fc_list "remove elements" [`Removed 1; `Removed 2; `Removed 3] result3;

  (* Test case 4: No changes *)
  let result4 = diff_list_ord (module IntEq) [1; 2; 3] [1; 2; 3] in
  check int_fc_list "no changes" [`Unchanged; `Unchanged; `Unchanged] result4;

  (* Test case 5: Mixed changes *)
  let result5 = diff_list_ord (module IntEq) [1; 2; 3] [1; 4; 3] in
  check int_fc_list "mixed changes" [`Unchanged; `Removed 2; `Added 4; `Unchanged] result5;

  ()

(** Additional tests for diff_list_ord *)
let test_diff_list_ord_advanced () =
  (* Test with simple integer lists *)
  let (module IntEq) = (module IntEq : EQUALABLE with type t = int) in

  (* Test case 1: No changes - verify all unchanged *)
  let result1 = diff_list_ord (module IntEq) [1; 2; 3; 4] [1; 2; 3; 4] in
  check int_fc_list "ord all unchanged" [`Unchanged; `Unchanged; `Unchanged; `Unchanged] result1;

  (* Test case 2: Replacement in middle *)
  let result2 = diff_list_ord (module IntEq) [1; 2; 3] [1; 9; 3] in
  check int_fc_list "ord middle replacement" [`Unchanged; `Removed 2; `Added 9; `Unchanged] result2;

  (* Test case 3: Multiple changes *)
  let result3 = diff_list_ord (module IntEq) [1; 2; 3; 4] [1; 5; 6; 4] in
  check int_fc_list "ord multiple changes" [`Unchanged; `Removed 2; `Removed 3; `Added 5; `Added 6; `Unchanged] result3;

  ()

(** Test diff_list_ord with different data types *)
let test_diff_list_ord_strings () =
  (* Test with string lists *)
  let module StringEq = struct
    type t = string
    let equal = String.equal
  end in

  let (module StrEq) = (module StringEq : EQUALABLE with type t = string) in

  (* Test case 1: Basic string diffing *)
  let result1 = diff_list_ord (module StrEq) ["a"; "b"; "c"] ["a"; "x"; "c"] in
  check string_fc_list "ord strings replacement" [`Unchanged; `Removed "b"; `Added "x"; `Unchanged] result1;

  (* Test case 2: String addition *)
  let result2 = diff_list_ord (module StrEq) ["hello"] ["hello"; "world"] in
  check string_fc_list "ord strings add" [`Unchanged; `Added "world"] result2;

  (* Test case 3: String removal *)
  let result3 = diff_list_ord (module StrEq) ["foo"; "bar"; "baz"] ["foo"] in
  check string_fc_list "ord strings remove" [`Unchanged; `Removed "bar"; `Removed "baz"] result3;

  ()

(** Test edge cases for diff_list_ord *)
let test_diff_list_ord_edge_cases () =
  let (module IntEq) = (module IntEq : EQUALABLE with type t = int) in

  (* Test case 1: Addition at the beginning *)
  let result1 = diff_list_ord (module IntEq) [1; 2; 3] [9; 1; 2; 3] in
  check int_fc_list "add at beginning" [`Added 9; `Unchanged; `Unchanged; `Unchanged] result1;

  (* Test case 2: Addition at the end *)
  let result2 = diff_list_ord (module IntEq) [1; 2; 3] [1; 2; 3; 9] in
  check int_fc_list "add at end" [`Unchanged; `Unchanged; `Unchanged; `Added 9] result2;

  (* Test case 3: Removal from the beginning *)
  let result3 = diff_list_ord (module IntEq) [9; 1; 2; 3] [1; 2; 3] in
  check int_fc_list "remove from beginning" [`Removed 9; `Unchanged; `Unchanged; `Unchanged] result3;

  (* Test case 4: Removal from the end *)
  let result4 = diff_list_ord (module IntEq) [1; 2; 3; 9] [1; 2; 3] in
  check int_fc_list "remove from end" [`Unchanged; `Unchanged; `Unchanged; `Removed 9] result4;

  (* Test case 5: Duplicate elements *)
  let result5 = diff_list_ord (module IntEq) [1; 1; 1] [1; 1] in
  check int_fc_list "duplicate elements" [`Removed 1; `Unchanged; `Unchanged] result5;

  (* Test case 6: Reordering *)
  let result6 = diff_list_ord (module IntEq) [1; 2; 3] [3; 2; 1] in
  check int_fc_list "reordering" [`Removed 1; `Removed 2; `Unchanged; `Added 2; `Added 1] result6;

  (* Test case 7: Completely different lists *)
  let result7 = diff_list_ord (module IntEq) [1; 2; 3] [4; 5; 6] in
  check int_fc_list "completely different" [`Removed 1; `Removed 2; `Removed 3; `Added 4; `Added 5; `Added 6] result7;

  ()

(** Alcotest test suite setup. *)
let () =
  run "Diff List Algorithms" [
    "Myers Diff", [
      test_case "Test basic Myers functionality" `Quick test_diff_list_myers_basic;
      test_case "Test Myers with strings" `Quick test_diff_list_myers_strings;
      test_case "Test edge cases" `Quick test_diff_list_myers_edge_cases;
      test_case "Test optimal edit distance" `Quick test_diff_list_myers_optimal;
      test_case "Compare with LCS algorithm" `Quick test_diff_list_myers_vs_lcs;
      test_case "Test custom equality" `Quick test_diff_list_myers_custom;
      test_case "Test correctness" `Quick test_myers_correctness;
    ];
    "Ordered Diff (LCS)", [
      test_case "Test basic sequence-based diffing" `Quick test_diff_list_ord_basic;
      test_case "Test advanced cases" `Quick test_diff_list_ord_advanced;
      test_case "Test with strings" `Quick test_diff_list_ord_strings;
      test_case "Test edge cases" `Quick test_diff_list_ord_edge_cases;
    ];
  ]
