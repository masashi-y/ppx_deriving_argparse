
open OUnit2


type common_types = {
  a1: string;
  b1: int;
  c1: float;
  d1: string option;
  e1: string list;
  f1: int array;
  g1: int list;
  h1: bool;
  j1: string list option;
  k1: int list option;
} [@@deriving show, argparse]


let simple ctxt =
    let argv = [|
        "cmd";
        "-a1"; "apple";
        "-b1"; "123";
        "-c1"; "1.20";
        "-d1"; "yes";
        "-e1"; "apple,banana,pear";
        "-f1"; "1,2,3,4,5";
        "-g1"; "100,200,300";
        "-h1";
        "-k1"; "1,2,3,4,5"
    |] in
    let default = {
        a1 = "";
        b1 = 0;
        c1 = 0.0;
        d1 = None;
        e1 = [];
        f1 = [||];
        g1 = [];
        h1 = false;
        j1 = None;
        k1 = None
    } in
    let expected = {
        a1 = "apple";
        b1 = 123;
        c1 = 1.20;
        d1 = Some "yes";
        e1 = ["apple";"banana";"pear"];
        f1 = [|1;2;3;4;5|];
        g1 = [100;200;300];
        h1 = true;
        j1 = None;
        k1 = Some [1;2;3;4;5]
    } in
    let result, _ = argparse_common_types default "test" argv in
    assert_equal ~printer:show_common_types result expected


let default ctxt =
    let argv = [|
        "cmd";
    |] in
    let default = {
        a1 = "";
        b1 = 0;
        c1 = 0.0;
        d1 = None;
        e1 = [];
        f1 = [||];
        g1 = [];
        h1 = false;
        j1 = None;
        k1 = None
    } in
    let result, _ = argparse_common_types default "test" argv in
    assert_equal ~printer:show_common_types result default


module M = struct
    type t = int * int [@@deriving show]
    let fst (f,_) = f
    let snd (_,s) = s

    let of_string s =
        try
            let sepi = String.index s '|' in
            let fst = String.sub s 0 sepi in
            let snd = String.sub s (sepi + 1) ((String.length s) - sepi - 1) in
            (int_of_string fst, int_of_string snd)
        with _ -> invalid_arg (Printf.sprintf "Couldn't parse `%s`" s)

    let to_string t =
        Printf.sprintf "%d|%d" (fst t) (snd t)
end

type custom_types = {
    foo: M.t; [@parse M.of_string] [@print M.to_string]
    bar: M.t; [@parse M.of_string] [@print M.to_string]
} [@@deriving argparse, show]

let customs ctxt =
    let argv = [|
        "cmd";
        "-foo"; "11|200";
        "-bar"; "0|13";
    |] in
    let default = {
        foo = (0, 0);
        bar = (0, 0)
    } in
    let expected = {
        foo = (11, 200);
        bar = (0, 13)
    } in
    let result, _ = argparse_custom_types default "test" argv in
    assert_equal ~printer:show_custom_types result expected


module Misc =
struct
    type t = {
        foo : int [@short "-f"];
        bar : int [@short "-b"];
        baz : bool [@short "-z"] [@set_false];
    } [@@deriving show, argparse { positional =
            ["test", "test";
            "test2", "test2"] }]
end


let misc ctxt =
    let argv = [|
        "cmd";
        "-f"; "1";
        "-b"; "1";
        "-z";
        "test";
        "test2"
    |] in
    let default = Misc.{
        foo = 0;
        bar = 0;
        baz = true;
    } in
    let expected = Misc.{
        foo = 1;
        bar = 1;
        baz = false;
    } in
    let args_expected = [|"test"; "test2"|] in
    let result, args = Misc.argparse default "test" argv in
    assert_equal ~printer:Misc.show result expected;
    assert_equal ~printer:[%derive.show: string array] args args_expected


let all_tests = "Test ppx_deriving_argparse" >::: [
    "simple" >:: simple;
    "default" >:: default;
    "customs" >:: customs;
    "misc" >:: misc
]

let _ = run_test_tt_main all_tests

