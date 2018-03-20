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

let attr_short attrs =
  Ppx_deriving.(attrs |> attr ~deriver "short" |> Arg.(get_attr ~deriver string))

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
      [%expr fun out -> Format.fprintf out [%e str (x ^ "%!")]] in
  let format x option_arg name msg =
      [%expr Printf.eprintf "  %-*s: %s {" spacing [%e str option_arg] [%e str msg];
            [%e x] Format.err_formatter [%e Exp.ident (lid name)]; 
            prerr_endline "}"] in
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
                  Format.pp_print_string out "[";
                  ignore (List.fold_left (fun sep v ->
                      if sep then Format.fprintf out ", ";
                        [%e aux typ] out v; true) false v);
                  Format.pp_print_string out "]"] in
        [%expr fun out -> Format.fprintf out "%a%!" [%e f]]
      | [%type: [%t? typ] array] ->
              let f = [%expr fun out v ->
                  Format.pp_print_string out "[|";
                  ignore (Array.fold_left (fun sep v ->
                      if sep then Format.fprintf out ", ";
                        [%e aux typ] out v; true) false v);
                  Format.pp_print_string out "|]"] in
        [%expr fun out -> Format.fprintf out "%a%!" [%e f]]
      | [%type: [%t? typ] option] ->
            let f = [%expr fun out v ->
                match v with
                    | None -> Format.pp_print_string out "none"
                    | Some v -> ([%e aux typ] out v;)] in
        [%expr fun out -> Format.fprintf out "%a%!" [%e f]]
      | _ -> begin match attr_print pld_attributes with
          | Some f ->
                let f = [%expr fun out v -> Format.fprintf out "%s%!" ([%e f] v)] in
                [%expr fun out -> Format.fprintf out "%a%!" [%e f]]
          | None -> 
                  let f = [%expr fun out v ->
                      Format.pp_print_string out "<unknown>"] in
                [%expr fun out -> Format.fprintf out "%a%!" [%e f]]
      end in
    format (aux pld_type)


let get_parse_fun { pld_name = { txt = name; loc }; pld_type; pld_attributes } =
    let wrap f =
        [%expr fun v -> try [%e f] v with _ -> raise (Invalid_argument v)] in
    let rec aux = function
        | [%type: int]         -> wrap [%expr int_of_string]
        | [%type: int32]
        | [%type: Int32.t]     -> wrap [%expr Int32.of_string]
        | [%type: int64]
        | [%type: Int64.t]     -> wrap [%expr Int64.of_string]
        | [%type: nativeint]
        | [%type: Nativeint.t] -> wrap [%expr Nativeint.of_string]
        | [%type: float]       -> wrap [%expr float_of_string]
        | [%type: bool]        -> wrap [%expr bool_of_string]
        | [%type: char]        -> wrap [%expr fun s -> if String.length s = 1
                                            then s.[0] else raise Invalid_argument s]
        | [%type: string]
        | [%type: String.t]    -> wrap [%expr fun s -> s]
        | [%type: [%t? typ] list]  ->
                let parse_comma_sep = [%expr fun v ->
                    let v = String.split_on_char ',' v in
                    List.map (fun v -> [%e aux typ] (String.trim v)) v] in
                wrap parse_comma_sep
        | [%type: [%t? typ] array] ->
                let parse_comma_sep = [%expr fun v ->
                    let v = String.split_on_char ',' v in
                    let v = List.map (fun v -> [%e aux typ] (String.trim v)) v in
                    Array.of_list v] in
                wrap parse_comma_sep
        | [%type: [%t? typ] option] -> wrap [%expr fun v -> Some ([%e aux typ] v)]
        | _ -> begin match attr_parse pld_attributes with
            | Some f -> wrap f
            | None -> raise_errorf ~loc "deriver %s requires [@parse] annotation for type: %s"
                                         deriver (Ppx_deriving.string_of_core_type pld_type)
        end
    in
    aux pld_type


let make_case ({ pld_name = { txt = name; loc }; pld_type; pld_attributes } as pld) option = match pld_type with
    | [%type: bool] -> let v = if attr_set_false pld_attributes then [%expr false] else [%expr true] in
                       let expr = [%expr aux [%e Exp.record [lid name, v] (Some [%expr cfg])] rest] in
                       Exp.case [%pat? [%p pstr option] :: rest] expr
    | _ -> let expr = [%expr aux [%e Exp.record [lid name, [%expr [%e get_parse_fun pld] i]] (Some [%expr cfg])] rest] in
           Exp.case [%pat? [%p pstr option] :: i :: rest] ~guard:[%expr not (is_option i)] expr


let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let { positional } = parse_options options in
  let nargs = List.length positional in
  let prerrfun_name = Ppx_deriving.mangle_type_decl (`PrefixSuffix ("prerr", deriver)) type_decl in
  let argparse_name = Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl in
  let quoter = Ppx_deriving.create_quoter () in
  let msg0 = [%expr prerr_endline "\nOptions:"] in
  let msg0, spacing = List.fold_right (fun (name, help) (exprs, spacing) ->
          let expr = [%expr Printf.eprintf "  %-*s: %s\n" spacing [%e str (String.uppercase_ascii name)] [%e str help]] in
          let spacing = max spacing (String.length name) in
          expr :: exprs, spacing) positional ([msg0], 0) in
  let msg0 = sequence ([%expr prerr_endline "\nArguments:"] :: msg0) in
  let error_cases = 
        [Exp.case [%pat? arg :: rest] ~guard:[%expr is_option arg]
            [%expr Printf.eprintf "PARSE ERROR: Option without required argument: \"%s\"\n" arg;
            [%e evar prerrfun_name] progname default; exit 2];
        Exp.case [%pat? arg :: rest] ~guard:[%expr arg.[0] = '-']
            [%expr Printf.eprintf "PARSE ERROR: Invalid option: \"%s\"\n" arg;
            [%e evar prerrfun_name] progname default; exit 2];
        Exp.case [%pat? rest] [%expr (cfg, rest)]] in
  let init_cases =
        [Exp.case (pnil ()) [%expr (cfg, [])];
        Exp.case [%pat? "-h" :: rest] [%expr [%e evar prerrfun_name] progname default; exit 2];
        Exp.case [%pat? "-help" :: rest] [%expr [%e evar prerrfun_name] progname default; exit 2];
        Exp.case [%pat? "--" :: rest] [%expr (cfg, rest)]] in
  let usages0 = List.map (fun (s, _) -> String.uppercase_ascii s) positional in
  match type_decl.ptype_kind with
  | Ptype_record labels ->
    let help_msg = [%expr Printf.eprintf "  %-*s:  show this help message and exit\n%!" spacing "-h, --help"] in
    let msgs, cases, options, usage, spacing, _ =
        List.fold_right (fun ({ pld_name = { txt = name; loc }; pld_type; pld_attributes } as pld) (msgs, cases, options, usages, spacing, shorts) ->
          let name_upper = String.uppercase_ascii name in
          let option = "-" ^ (string_replace '_' '-' name) in
          let msg0 = match attr_help pld_attributes with
              | Some msg -> msg
              | None -> "" in
          let usage = match pld_type with
              | [%type: bool] -> "[" ^ option ^ "]@ "
              | _ -> Printf.sprintf "[%s %s]@ " option name_upper in
          let option_arg, cases, shorts, options = match attr_short pld_attributes with
            | None -> (match pld_type with
              | [%type: bool] -> option | _ -> option ^ " " ^ name_upper),
                      make_case pld option :: cases, shorts, str option :: options
            | Some short -> if List.mem short shorts then
                            raise_errorf ~loc 
                                "deriver %s: duplicate short optional argument: \"%s\"" deriver short
                            else (short ^ ", " ^ match pld_type with
              | [%type: bool] -> option | _ -> option ^ " " ^ name_upper),
                      make_case pld short :: make_case pld option :: cases,
                      short :: shorts, str short :: str option :: options in
          let msg = prerr_msg pld option_arg name msg0 in
          let spacing = max spacing (String.length option_arg) in
          msg :: msgs, cases, options, usage :: usages, spacing, shorts)
        labels ([help_msg], error_cases, [], usages0, spacing, ["-h"]) in
    let cases = init_cases @ cases in
    let fields = List.map (fun { pld_name = { txt }} -> (txt, pvar txt)) labels in
    let usage = [%expr Format.eprintf [%e str ("\nUsage: %s @[" ^ String.concat " " usage ^ "@]\n")] progname] in
    let prerr_fun = [%expr fun progname [%p precord fields] ->
            let spacing = [%e int (spacing + 1)] in
            [%e sequence (usage :: msg0 :: msgs)]] in
    let argparse = [%expr fun default progname args ->
            let is_option o = List.mem o [%e list options] in
            let rec aux cfg args =
                try [%e Exp.match_ (evar "args") cases]
                with Invalid_argument s ->
                    Printf.eprintf "PARSE ERROR: Invalid argument for keyword option \"%s\": \"%s\"\n"
                    (List.hd args) s; [%e evar prerrfun_name] progname default; exit 2 in
            let cfg, rest = aux default (List.tl (Array.to_list args)) in
            if List.length rest < [%e int nargs] then
                (Printf.eprintf "PARSE ERROR: Invalid number of required arguments\n";
                [%e evar prerrfun_name] progname default; exit 2);
            cfg, Array.of_list rest] in
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
