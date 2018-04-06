

(* Simple neural network example *)

type activation = Relu | Sigmoid | Tanh | Silu | Softmax [@@deriving show]

let parse_activation = function 
    "relu" -> Relu | "sigmoid" -> Sigmoid | "tanh" -> Tanh | "silu" -> Silu | "softmax" -> Softmax
    | s -> raise (Invalid_argument s)


let show_activation = function
    | Relu -> "relu" | Sigmoid -> "sigmoid" | Tanh -> "tanh" | Silu -> "silu" | Softmax -> "softmax"


type t = {
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
   You can omit either of positional and description:
   e.g. [@@ argparse { description = "some ..." }]
   or entirely: [@@ argparse]
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

let () =
    let cfg, rest = argparse default "example" Sys.argv in
    prerr_argparse "example" cfg;
    print_endline (show cfg);
    Array.iter print_endline rest

