open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
open Result

let deriver = "argparse"
let raise_errorf = Ppx_deriving.raise_errorf

type options = { positional : (string * string) list }

let string_pair = Ppx_deriving.Arg.(function
    | [%expr ([%e? a], [%e? b])] ->
            begin match string a, string b with
                | Ok a, Ok b -> Ok (a, b)
                | _ -> Error "string_pair"
            end
    | _ -> Error "string_pair")

let parse_options options =
  let positional = ref [] in
  options |> List.iter (fun (name, expr) ->
    match name with
    | "positional" -> positional := Ppx_deriving.Arg.(get_expr ~deriver (list string_pair)) expr
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name);
  { positional = !positional }

let attr_help attrs =
  Ppx_deriving.(attrs |> attr ~deriver "ocaml.doc" |> Arg.(get_attr ~deriver string))

let attr_parse attrs =
  Ppx_deriving.(attrs |> attr ~deriver "parse" |> Arg.(get_attr ~deriver expr))

let attr_print attrs =
  Ppx_deriving.(attrs |> attr ~deriver "print" |> Arg.(get_attr ~deriver expr))

let attr_set_false attrs =
  Ppx_deriving.(attrs |> attr ~deriver "set_false" |> Arg.get_flag ~deriver)

let string_replace c_in c_out str =
    String.init (String.length str)
        (fun i -> if str.[i] = c_in then c_out else str.[i])

let prerr_msg { pld_name = { txt = name; loc }; pld_type; pld_attributes } =
  let simple x =
      [%expr fun out -> Ppx_deriving_runtime.Format.fprintf out [%e str (x ^ "%!")]] in
  let format x name0 name msg =
      [%expr Ppx_deriving_runtime.prerr_string
            [%e str (Printf.sprintf "  %s %s\t: %s {" name0 (String.uppercase_ascii name) msg)];
            [%e x] Ppx_deriving_runtime.Format.err_formatter [%e Exp.ident (lid name)]; 
            Ppx_deriving_runtime.prerr_endline "}"] in
  let rec aux = function
      | [%type: int]         -> simple "%d"
      | [%type: int32]
      | [%type: Int32.t]     -> simple "%ldl"
      | [%type: int64]
      | [%type: Int64.t]     -> simple "%LdL"
      | [%type: nativeint]
      | [%type: Nativeint.t] -> simple "%ndn"
      | [%type: float]       -> simple "%F"
      | [%type: bool]        -> simple "%B"
      | [%type: char]        -> simple "%C"
      | [%type: string]
      | [%type: String.t]    -> simple "%S"
      | [%type: [%t? typ] list]  ->
              let f = [%expr fun out v ->
                  Ppx_deriving_runtime.Format.pp_print_string out "[";
                  ignore (List.fold_left (fun sep v ->
                      if sep then Ppx_deriving_runtime.Format.fprintf out "; ";
                        [%e aux typ] out v; true) false v);
                  Ppx_deriving_runtime.Format.pp_print_string out "]"] in
        [%expr fun out -> Ppx_deriving_runtime.Format.fprintf out "%a%!" [%e f]]
      | [%type: [%t? typ] array] ->
              let f = [%expr fun out v ->
                  Ppx_deriving_runtime.Format.pp_print_string out "[|";
                  ignore (Array.fold_left (fun sep v ->
                      if sep then Ppx_deriving_runtime.Format.fprintf out ", ";
                        [%e aux typ] out v; true) false v);
                  Ppx_deriving_runtime.Format.pp_print_string out "|]"] in
        [%expr fun out -> Ppx_deriving_runtime.Format.fprintf out "%a%!" [%e f]]
      | [%type: [%t? typ] option] ->
            let f = [%expr fun out v ->
                match v with
                    | None -> Ppx_deriving_runtime.Format.pp_print_string out "None"
                    | Some v -> (Ppx_deriving_runtime.Format.pp_print_string out "Some ";
                                 [%e aux typ] out v;)] in
        [%expr fun out -> Ppx_deriving_runtime.Format.fprintf out "%a%!" [%e f]]
      | _ -> begin match attr_print pld_attributes with
          | Some f ->
                let f = [%expr fun out v -> Ppx_deriving_runtime.Format.fprintf out "%s%!" ([%e f] v)] in
                [%expr fun out -> Ppx_deriving_runtime.Format.fprintf out "%a%!" [%e f]]
          | None -> 
                  let f = [%expr fun out v ->
                      Ppx_deriving_runtime.Format.pp_print_string out "<unknown>"] in
                [%expr fun out -> Ppx_deriving_runtime.Format.fprintf out "%a%!" [%e f]]
      end in
    match pld_type with
    | [%type: bool] ->
          fun name0 name msg ->
              [%expr Ppx_deriving_runtime.prerr_string
                    [%e str (Printf.sprintf "  %s\t: %s {" name0 msg)];
                    Ppx_deriving_runtime.Format.eprintf "%B%!" [%e Exp.ident (lid name)]; 
                    Ppx_deriving_runtime.prerr_endline "}"]
    | _ -> format (aux pld_type)


let get_parse_fun { pld_name = { txt = name; loc }; pld_type; pld_attributes } =
    let wrap f =
        [%expr fun v -> try [%e f] v with _ -> raise (Invalid_argument v)] in
    let rec aux = function
        | [%type: int]         -> wrap [%expr Ppx_deriving_runtime.int_of_string]
        | [%type: int32]
        | [%type: Int32.t]     -> wrap [%expr Ppx_deriving_runtime.Int32.of_string]
        | [%type: int64]
        | [%type: Int64.t]     -> wrap [%expr Ppx_deriving_runtime.Int64.of_string]
        | [%type: nativeint]
        | [%type: Nativeint.t] -> wrap [%expr Ppx_deriving_runtime.Nativeint.of_string]
        | [%type: float]       -> wrap [%expr Ppx_deriving_runtime.float_of_string]
        | [%type: bool]        -> wrap [%expr Ppx_deriving_runtime.bool_of_string]
        | [%type: char]        -> wrap [%expr fun s -> if Ppx_deriving_runtime.String.length s = 1
                                            then s.[0] else raise Invalid_argument s]
        | [%type: string]
        | [%type: String.t]    -> wrap [%expr fun s -> s]
        | [%type: [%t? typ] list]  ->
                let parse_comma_sep = [%expr fun v ->
                    let v = Ppx_deriving_runtime.String.split_on_char ',' v in
                    Ppx_deriving_runtime.List.map (fun v -> [%e aux typ] (Ppx_deriving_runtime.String.trim v)) v] in
                wrap parse_comma_sep
        | [%type: [%t? typ] array] ->
                let parse_comma_sep = [%expr fun v ->
                    let v = Ppx_deriving_runtime.String.split_on_char ',' v in
                    let v = Ppx_deriving_runtime.List.map (fun v -> [%e aux typ] (Ppx_deriving_runtime.String.trim v)) v in
                    Ppx_deriving_runtime.Array.of_list v] in
                wrap parse_comma_sep
        | [%type: [%t? typ] option] -> wrap [%expr fun v -> Some ([%e aux typ] v)]
        | _ -> begin match attr_parse pld_attributes with
            | Some f -> wrap f
            | None -> raise_errorf ~loc "deriver %s requires [@parse] annotation for type: %s"
                                         deriver (Ppx_deriving.string_of_core_type pld_type)
        end
    in
    aux pld_type


let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let { positional } = parse_options options in
  let msg0 = [%expr Ppx_deriving_runtime.prerr_endline "\nOptions:"] in
  let msg0 = match positional with
      | [] -> msg0
      | ps -> let args = List.fold_right (fun (name, help) exprs ->
                  let expr = [%expr Ppx_deriving_runtime.Printf.eprintf "  %s\t: %s\n" [%e str (String.uppercase_ascii name)] [%e str help]] in
                  expr :: exprs) ps [msg0] in
              sequence ([%expr Ppx_deriving_runtime.prerr_endline "\nRequired:"] :: args) in
  let prerrfun_name = Ppx_deriving.mangle_type_decl (`PrefixSuffix ("prerr", deriver)) type_decl in
  let argparse_name = Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl in
  let quoter = Ppx_deriving.create_quoter () in
  let error_cases = 
        [Exp.case [%pat? arg :: rest] ~guard:[%expr is_option arg]
            [%expr Ppx_deriving_runtime.Printf.eprintf "PARSE ERROR: Option without required argument: \"%s\"\n" arg;
            [%e evar prerrfun_name] progname default; Ppx_deriving_runtime.exit 2];
        Exp.case [%pat? arg :: rest] ~guard:[%expr arg.[0] = '-']
            [%expr Ppx_deriving_runtime.Printf.eprintf "PARSE ERROR: Invalid option: \"%s\"\n" arg;
            [%e evar prerrfun_name] progname default; Ppx_deriving_runtime.exit 2];
        Exp.case [%pat? rest] [%expr (cfg, rest)]] in
  let init_cases =
        [Exp.case (pnil ()) [%expr (cfg, [])];
        Exp.case [%pat? "-h" :: rest] [%expr [%e evar prerrfun_name] progname default; Ppx_deriving_runtime.exit 2];
        Exp.case [%pat? "-help" :: rest] [%expr [%e evar prerrfun_name] progname default; Ppx_deriving_runtime.exit 2];
        Exp.case [%pat? "--" :: rest] [%expr (cfg, rest)]] in
  let help_msg = [%expr Ppx_deriving_runtime.prerr_endline "  -h, --help\tshow this help message and exit"] in
  let usages0 = List.map (fun (s, _) -> String.uppercase_ascii s) positional in
  match type_decl.ptype_kind with
  | Ptype_record labels ->
    let msgs, cases, options, usage =
        List.fold_right (fun ({ pld_name = { txt = name; loc }; pld_type; pld_attributes } as pld) (msgs, cases, options, usages) ->
          let option = "-" ^ (string_replace '_' '-' name) in
          let msg0 = match attr_help pld_attributes with
              | Some msg -> msg
              | None -> "" in
          let msg = prerr_msg pld option name msg0 in
          let case = match pld_type with
              | [%type: bool] ->
                      let v = if attr_set_false pld_attributes then [%expr false] else [%expr true] in
                      let expr = [%expr aux [%e Exp.record [lid name, v] (Some [%expr cfg])] rest] in
                      Exp.case [%pat? [%p pstr option] :: rest] expr
              | _ -> let expr = [%expr aux [%e Exp.record [lid name, [%expr [%e get_parse_fun pld] i]] (Some [%expr cfg])] rest] in
                      Exp.case [%pat? [%p pstr option] :: i :: rest] ~guard:[%expr not (is_option i)] expr in
          let usage = match pld_type with
              | [%type: bool] -> "[" ^ option ^ "]@ "
              | _ -> Printf.sprintf "[%s %s]@ " option (String.uppercase_ascii name) in
          msg :: msgs, case :: cases, str option :: options, usage :: usages)
          labels ([help_msg], error_cases, [], usages0) in
    let cases = init_cases @ cases in
    let fields = List.map (fun { pld_name = { txt }} -> (txt, pvar txt)) labels in
    let usage = [%expr Ppx_deriving_runtime.Format.eprintf
            [%e str ("\nUsage: %s @[" ^ String.concat " " usage ^ "@]\n")] progname] in
    let prerr_fun = [%expr fun progname [%p precord fields] -> [%e sequence (usage :: msg0 :: msgs)]] in
    let argparse0 = [%expr fun cfg args ->
        try [%e Exp.match_ (evar "args") cases]
        with Invalid_argument s ->
            Ppx_deriving_runtime.Printf.eprintf
                "PARSE ERROR: Invalid argument for keyword option \"%s\": \"%s\"\n"
                (Ppx_deriving_runtime.List.hd args) s; Ppx_deriving_runtime.exit 2] in
    let argparse = [%expr fun default progname args ->
            let is_option o = Ppx_deriving_runtime.List.mem o [%e list options] in
            let rec aux = [%e argparse0] in
            let cfg, rest = aux default (Ppx_deriving_runtime.List.tl (Ppx_deriving_runtime.Array.to_list args)) in
            cfg, Ppx_deriving_runtime.Array.of_list rest] in
    [Vb.mk (pvar prerrfun_name) (Ppx_deriving.sanitize ~quoter prerr_fun);
     Vb.mk (pvar argparse_name) (Ppx_deriving.sanitize ~quoter argparse)]
  | _ -> raise_errorf ~loc "%s can be derived only for record types" deriver


let sig_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  ignore (parse_options options);
  let typ0 = Ppx_deriving.core_type_of_type_decl type_decl in
  let typ_parse = [%type: [%t typ0] -> string -> string array -> [%t typ0] * string array] in
  let typ_prerr = [%type: string -> [%t typ0] -> unit] in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl)) typ_parse);
  Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`PrefixSuffix ("prerr", deriver)) type_decl)) typ_prerr)]


let () =
  Ppx_deriving.(register (create deriver
    ~type_decl_str: (fun ~options ~path type_decls ->
       [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path) type_decls))])
    ~type_decl_sig: (fun ~options ~path type_decls ->
       List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
