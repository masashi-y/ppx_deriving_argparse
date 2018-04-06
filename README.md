# [@@deriving argparse]

## Install

Available from opam:
```sh
➜  opam install ppx_deriving_argparse
```

## Usage

```ocaml
(* Simple neural network example *)

type activation = Relu | Sigmoid | Tanh | Silu | Softmax [@@deriving show]

let parse_activation = function 
    "relu" -> Relu | "sigmoid" -> Sigmoid | "tanh" -> Tanh | "silu" -> Silu | "softmax" -> Softmax
    | s -> raise (Invalid_argument s)

let show_activation = function
    | Relu -> "relu" | Sigmoid -> "sigmoid" | Tanh -> "tanh" | Silu -> "silu" | Softmax -> "softmax"


type t = {
    (* [@short] for short form of the argument *)
    nlayers        : int   [@short "-l"]
        (** the number of layers *);

    (* with [@set_false], "-use-dropout" flag turns the value false. *)
    use_dropout    : bool  [@set_false]
        (** true if use dropout *);

    dropout_rate   : float option
        (** dropout rate *);

    test : int list option
        (** this is an test argument *);

    test2 : int option list;

    (* arguments with custom type *)
    activation : activation [@print show_activation] [@parse parse_activation];
        (** activation function in feed forward layers *)

    activation2 : activation [@parse parse_activation];
        (** activation function in feed forward layers *)
} [@@deriving show, argparse {
    (* You can pass positional argument, which is (string * string) list.  *)
    positional = 
        ["train", "train file";
        "eval", "some file"];

    (* also some description. *)
    description = "some neural networks";
}]

(*
   You can omit either of positional and description 
   as in [@@deriving argparse { description = "some ..." }]
   or entirely: [@@deriving argparse].
*)


(* default arguments *)
let default = {
    nlayers = 2;
    use_dropout = true;
    dropout_rate = Some 0.1;
    test = Some [1;2;3];
    test2 = [Some 1;Some 2;Some 3; None];
    activation = Relu;
    activation2 = Sigmoid;
}

(*
   [@@argparse] generates the following functions:
   val argparse : t -> string -> string array -> t * string array
   val prerr_argparse : string -> t -> unit

   while for types whose name is different from t (say config):
   val argparse_config : config -> string -> string array -> config * string array
   val prerr_config_argparse : string -> config -> unit
*)

let () =
    let cfg, rest = argparse default "example" Sys.argv in
    prerr_argparse "example" cfg;
    print_endline (show cfg);
    Array.iter print_endline rest

```

derives a command line parser with an error message function:

```
➜ ./example.exe -h
Usage: example [-help] [-nlayers NLAYERS]  [-use-dropout]
                [-dropout-rate DROPOUT_RATE]  [-test TEST]  [-test2 TEST2]
                [-activation ACTIVATION]  [-activation2 ACTIVATION2]
                TRAIN EVAL

some neural networks

Arguments:
  TRAIN                      : train file
  EVAL                       : some file

Options:
  -l, -nlayers NLAYERS       :  the number of layers  {2}
  -use-dropout               :  true if use dropout  {true}
  -dropout-rate DROPOUT_RATE :  dropout rate  {0.1}
  -test TEST                 :  this is an test argument  {[1, 2, 3]}
  -test2 TEST2               :  {[1, 2, 3, none]}
  -activation ACTIVATION     :  activation function in feed forward layers  {relu}
  -activation2 ACTIVATION2   :  activation function in feed forward layers  {<unknown>}
  -h, -help                  :  show this help message and exit
```

On parse error:

```
➜  ./example.exe -l a
PARSE ERROR: Invalid argument for keyword option "-l": "a"
Usage: example [-help] [-nlayers NLAYERS]  [-use-dropout]
                [-dropout-rate DROPOUT_RATE]  [-test TEST]  [-test2 TEST2]
                [-activation ACTIVATION]  [-activation2 ACTIVATION2]
                TRAIN EVAL
```

The parsing results are in `type t` for optional arguments and in `string array` for positional ones.

```ocaml
(* print_endline (show cfg) *)
{ Example.src_vocab_size = 3; tgt_vocab_size = 100; num_units = 512;
  nheads = 8; nlayers = 6; use_dropout = false; dropout_rate = (Some 0.1);
  test = (Some [1; 1; 1]); test2 = [(Some 1); (Some 2); (Some 3); None];
  activation = Example.Relu; activation2 = Example.Sigmoid } 
  
(* Array.iter print_endline rest *)
train.txt
test.txt
```

For simplicity, `[@@argparse]` does not take care of the structure of positional arguments
except that the resulting array's length is more than or equal to that of `positional` list.
This can be exploited to do:
```ocaml
[@@argparse { positional = ["files ...", "a list of files to process"] }]
```
for arguments of variable length.

## Contact

Reporting issues, comments, or any suggestions (especially on the interface) are welcome: yoshikawa.masashi.yh8@is.naist.jp

