open OUnit2
open Board
open Replayer
open Opener
open Application
open Trie

let replay_tests = []

let board_tests = []

let open_tests = []

let app_tests = []

let all_tests =
  replay_tests @
  board_tests @
  open_tests @
  app_tests


let suite = "Final Proj Test Suite" >:::
  all_tests

let _ = run_test_tt_main suite
