# [@@deriving argparse]

Currently alpha version.
```ocaml

(* custom type with parse & print functions *)
type activation = Relu | Sigmoid | Tanh [@@deriving show]

let parse_activation = function 
    "relu" -> Relu | "sigmoid" -> Sigmoid | "tanh" -> Tanh
    | s -> raise (Invalid_argument s)

let show_activation = function
    Relu -> "relu" | Sigmoid -> "sigmoid" | Tanh -> "tanh"


type t = {
    src_vocab_size : int [@short "-s"]
        (** the source vocabulary size *);
    tgt_vocab_size : int [@short "-t"]
        (** the target vocabulary size *);
    num_units      : int [@short "-u"]
        (** the number of units *);
    nheads         : int [@short "-H"]
        (** the number of multi-head attention *);
    nlayers        : int
        (** the number of layers *);
        
    (* with [@set_false], "-use-dropout" turns the boolean value false. *)
    use_dropout    : bool  [@set_false]
        (** true if use dropout *);
    dropout_rate   : float option
        (** dropout rate *);
    test : int list option
        (** this is an test argument *);
    test2 : int option list;
    activation : activation [@print show_activation] [@parse parse_activation];
        (** activation function in feed forward layers *)
    activation2 : activation [@parse parse_activation];
        (** activation function in feed forward layers *)
} [@@deriving show, argparse { positional =
        ["train", "path to train file";
         "eval", "path to evaluation file"] }]


(* default arguments *)
let default = {
    src_vocab_size = 0;
    tgt_vocab_size = 0;
    num_units = 512;
    nheads = 8;
    nlayers = 6;
    use_dropout = true;
    dropout_rate = Some 0.1;
    test = Some [1;2;3];
    test2 = [Some 1;Some 2;Some 3; None];
    activation = Relu;
    activation2 = Sigmoid;
}

let () =
    let cfg, rest = argparse default "example" Sys.argv in
    prerr_argparse "example" cfg;
    print_endline (show cfg);
    Array.iter print_endline rest
```

derives a command line parser with an error message function:

```
➜  ./example.byte -src-vocab-size 3 -t 100 -activation relu -test "1,1,1" train.txt test.txt

Usage: example [-src-vocab-size SRC_VOCAB_SIZE]
                 [-tgt-vocab-size TGT_VOCAB_SIZE]  [-num-units NUM_UNITS]
                 [-nheads NHEADS]  [-nlayers NLAYERS]  [-use-dropout]
                 [-dropout-rate DROPOUT_RATE]  [-test TEST]  [-test2 TEST2]
                 [-activation ACTIVATION]  [-activation2 ACTIVATION2]
                 TRAIN EVAL

Arguments:
  TRAIN                              : path to train file
  EVAL                               : path to evaluation file

Options:
  -s, -src-vocab-size SRC_VOCAB_SIZE :  the source vocabulary size  {3}
  -t, -tgt-vocab-size TGT_VOCAB_SIZE :  the target vocabulary size  {100}
  -u, -num-units NUM_UNITS           :  the number of units  {512}
  -H, -nheads NHEADS                 :  the number of multi-head attention  {8}
  -nlayers NLAYERS                   :  the number of layers  {6}
  -use-dropout                       :  true if use dropout  {true}
  -dropout-rate DROPOUT_RATE         :  dropout rate  {0.1}
  -test TEST                         :  this is an test argument  {[1, 1, 1]}
  -test2 TEST2                       :  {[1, 2, 3, none]}
  -activation ACTIVATION             :  activation function in feed forward layers  {relu}
  -activation2 ACTIVATION2           :  activation function in feed forward layers  {<unknown>}
  -h, --help                         :  show this help message and exit
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

