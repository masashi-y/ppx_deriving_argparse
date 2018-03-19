
type activation = Relu | Sigmoid | Tanh | Silu | Softmax

let parse_activation = function 
    "relu" -> Relu | "sigmoid" -> Sigmoid | "tanh" -> Tanh | "silu" -> Silu | "softmax" -> Softmax
    | s -> raise (Invalid_argument s)

let show_activation = function
    | Relu -> "relu"
    | Sigmoid -> "sigmoid"
    | Tanh -> "tanh"
    | Silu -> "silu"
    | Softmax -> "softmax"

type t = {
    src_vocab_size : int   [@help "the source vocabulary size"];
    tgt_vocab_size : int   [@help "the target vocabulary size"];
    num_units      : int   [@help "the number of units"];
    nheads         : int   [@help "the number of multi-head attention"];
    nlayers        : int   [@help "the number of layers"];
    use_dropout    : bool  [@help "true if use dropout"];
    dropout_rate   : float option [@help "dropout rate"];
    test : int list option [@help "this is an test argument"];
    test2 : int option list;
    activation : activation [@help "activation function in feed forward layers"]
                            [@print show_activation] [@parse parse_activation];
    activation2 : activation [@help "activation function in feed forward layers"]
                            [@parse parse_activation];
} [@@deriving argparse]


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
    let cfg, rest = argparse default Sys.argv in
    argparse_perr cfg;
    Array.iter print_endline rest

