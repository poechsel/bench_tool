open Macroperf

module List = struct
  include List
  let filter_map f l =
    List.fold_left (fun a e -> match f e with Some v -> v::a | None -> a) [] l |> List.rev
end

module StringList = struct
  let settrip l = SSet.(of_list l |> elements)
end

module String = struct
  include String
  let prefix s s' =
    length s <= length s' && s = sub s' 0 @@ length s
end

type copts = {
  output: [`Stdout | `File of string | `None];
  ignore_out: [`Stdout | `Stderr] list;
  opamroot: string option;
}

let write_res_copts copts res =
  let res = List.fold_left (fun a s -> Result.strip s a) res copts.ignore_out in

  (* Write the result into stdout, or <file> if specified *)
  (match copts.output with
   | `None -> ()
   | `Stdout ->
     Summary.of_result res  |> Summary.output_hum stdout

     (*Result.output_hum stdout res*)
   | `File fn ->
     try (*Result.save_hum fn res*)
       Summary.of_result res  |> Summary.save_hum fn
       with Sys_error m -> Printf.eprintf "%s\n" m

         (* Sexplib cannot create temporary file, aborting*)
  )

let run inlining_args copts switch bench =
  let opamroot = copts.opamroot in
  let bench =
    Benchmark.load_conv_exn ~switch ~opamroot bench
  in
  let res =
    Runner.run_exn ~opamroot ~switch ~inlining_args bench
  in
  write_res_copts copts res

let help man_format cmds topic = match topic with
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
      let topics = "topics" :: "patterns" :: "environment" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
      | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
      | `Ok t ->
          let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
          `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)

let list copts switches =
  let print files_names =
      let max_name_len = List.fold_left
          (fun a (n,fn) ->
             let len = String.length n in
             if len > a then len else a)
          0 files_names
      in
      List.iter (fun (n,(_, fn)) ->
          Printf.printf "%-*s %s\n" max_name_len n fn
        ) files_names in
  let print_all switches =
    List.iter (fun s ->
        Printf.printf "# %s\n" s;
        print @@ BenchmarkOpam.find_installed ?opamroot:copts.opamroot s;
        print_endline ""
      ) switches in
  let switches = match switches with
    | [] -> Util.Opam.switches ~opamroot:copts.opamroot
    | s ->
        StringList.settrip @@
        List.(flatten @@ map Util.Opam.switches_matching s) in
  print_all switches

open Cmdliner

(* Help sections common to all commands *)

let copts_sect = "COMMON OPTIONS"
let help_secs = [
  `S copts_sect;
  `P "These options are common to some commands (including this one).";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
  `S "BUGS"; `P "Report bugs at <http://github.com/OCamlPro/oparf-macro>.";]

let copts batch output_file opamroot ignore_out =
  let _ = Printf.printf "%%%%%%%%%%5 %s\n" output_file in
  let output =
    if not batch then `None
    else if output_file = "" then `Stdout
    else let _ = Printf.printf "Indeed its a file\n" in let _ = flush_all() in `File output_file in
  { output;
    opamroot;
    ignore_out=List.map
        (function
          | "stdout" -> `Stdout
          | "stderr" -> `Stderr
          | _ -> invalid_arg "copts"
        )
        ignore_out
  }

let output_file =
  let docs = copts_sect in
  let doc = "File to write the result to (default: stdout)." in
  Arg.(value & opt string "" & info ["o"; "output"] ~docv:"file" ~docs ~doc)

let copts_t =
  let docs = copts_sect in
  let opamroot =
    let doc = "Specify the opam root (default: use OPAMROOT environment \
        variable)" in
    Arg.(value & opt (some string) None & info ["opamroot"] ~docv:"<dir>" ~docs ~doc) in
  let batch =
    let doc = "Run in batch mode, i.e. print result files to stdout \
        instead of printing information about progression." in
    Arg.(value & flag & info ["batch"] ~docs ~doc) in
  let ignore_out =
    let doc = "Discard program output (default: none)." in
    Arg.(value & opt (list string) [] & info ["discard"] ~docv:"<channel>" ~docs ~doc) in
  Term.(pure copts $ batch $ output_file $ opamroot $ ignore_out)

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "Display help about macroperf and macroperf commands." in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about macroperf commands and other subjects..."] @ help_secs
  in
  Term.(ret (pure help $ Term.man_format $ Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "Macrobenchmarking suite for OCaml." in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "macrorun" ~version:"0.1" ~sdocs:copts_sect ~doc ~man

let switch =
  let doc = "Look for benchmarks installed in another switch, instead of the current one." in
  Arg.(value & opt (some string) None & info ["s"; "switch"] ~docv:"glob_pat" ~doc)


let round =
  let doc = "Round number" in
  Arg.(value & opt int 1 & info ["round"] ~doc)

let inline_branch_factor =
  let doc = "Inline branch factor" in
  Arg.(value & opt (some float) None & info ["inline-branch-factor"] ~doc)

let inline =
  let doc = "Inline" in
  Arg.(value & opt (some int) None & info ["inline"] ~doc)

let inline_toplevel =
  let doc = "Inline toplevel" in
  Arg.(value & opt (some int) None & info ["inline-toplevel"] ~doc)

let inline_alloc_cost =
  let doc = "Inline alloc cost" in
  Arg.(value & opt (some int) None & info ["inline-alloc-cost"] ~doc)

let inline_branch_cost =
  let doc = "Inline branch cost" in
  Arg.(value & opt (some int) None & info ["inline-branch-cost"] ~doc)

let inline_prim_cost =
  let doc = "Inline prim cost" in
  Arg.(value & opt (some int) None & info ["inline-prim-cost"] ~doc)

let inline_call_cost =
  let doc = "Inline call cost" in
  Arg.(value & opt (some int) None & info ["inline-call-cost"] ~doc)

let inline_indirect_cost =
  let doc = "Inline indirect cost" in
  Arg.(value & opt (some int) None & info ["inline-indirect-cost"] ~doc)

let inline_lifting_benefit =
  let doc = "Inline lifting benefit" in
  Arg.(value & opt (some int) None & info ["inline-lifting-benefit"] ~doc)

let inline_max_specialise =
  let doc = "Inline max specialise" in
  Arg.(value & opt (some int) None & info ["inline-max-specialise"] ~doc)

let inline_max_depth =
  let doc = "Inline max depth" in
  Arg.(value & opt (some int) None & info ["inline-max-depth"] ~doc)

let unbox_closures =
  let doc = "Unbox closures" in
  Arg.(value & opt (some bool) None & info ["unbox_closures"] ~doc)

let unbox_closures_factor =
  let doc = "Unbox closures_factor" in
  Arg.(value & opt (some int) None & info ["unbox_closures_factor"] ~doc)

let inline_max_unroll =
  let doc = "Inline max unroll" in
  Arg.(value & opt (some int) None & info ["inline max unroll"] ~doc)

let remove_unused_arguments =
  let doc = "Remove unused arguments" in
  Arg.(value & opt (some bool) None & info ["remove-unused-arguments"] ~doc)

let round_2_multiplier =
  let doc = "Multiplier for parameters of round 2" in
  Arg.(value & opt float 1. & info ["round-2-multiplier"] ~doc)

let round_3_multiplier =
  let doc = "Multiplier for parameters of round 3" in
  Arg.(value & opt float 1. & info ["round-3-multiplier"] ~doc)

let make_inlining_args round inline_branch_factor inline inline_toplevel inline_alloc_cost
      inline_branch_cost inline_prim_cost inline_call_cost inline_indirect_cost
      inline_lifting_benefit inline_max_depth unbox_closures unbox_closures_factor
      inline_max_unroll remove_unused_arguments inline_max_specialise
      round_2_multiplier round_3_multiplier
  =
  let rs = string_of_int (round - 1) in
  let make_int_arg name v =
    match v with
    | None -> ""
    | Some i ->
      name ^ "=0=" ^ string_of_int i ^ "," ^
      name ^ "=1=" ^ string_of_int
                       (int_of_float @@ round_2_multiplier *. (float_of_int i))
      ^ "," ^
      name ^ "=2=" ^ string_of_int
                       (int_of_float @@ round_3_multiplier *. (float_of_int i))
  in
  let make_float_arg_branch name v =
    match v with
    | None -> ""
    | Some i ->
      name ^ "=0=" ^ string_of_float i ^ "," ^
      name ^ "=1=" ^ string_of_float i ^ "," ^
      name ^ "=2=" ^ string_of_float 0.
  in
  let make_bool_arg name v =
    match v with
    | Some true -> name ^ "=1"
    | _ -> ""
  in
  let a =
    "O3=1" ::
    make_float_arg_branch "inline_branch_factor" inline_branch_factor ::
    make_int_arg "inline" inline ::
    make_int_arg "inline-toplevel" inline_toplevel ::
    make_int_arg "inline-alloc-cost" inline_alloc_cost ::
    make_int_arg "inline-branch-cost" inline_branch_cost ::
    make_int_arg "inline-prim-cost" inline_prim_cost ::
    make_int_arg "inline-call-cost" inline_call_cost ::
    make_int_arg "inline-indirect-cost" inline_indirect_cost ::
    make_int_arg "inline-lifting-benefit" inline_lifting_benefit ::
    make_int_arg "inline-max-depth" inline_max_depth ::
    make_int_arg "inline-max-specialise" inline_max_depth ::
    make_bool_arg "unbox-closures" unbox_closures ::
    make_int_arg "unbox-closures-factor" unbox_closures_factor ::
    (*make_int_arg "inline-max-unroll" inline_max_unroll ::*)
    make_bool_arg "remove-unused-arguments" remove_unused_arguments ::
    []
  in
  String.concat "," a ^ ",_"

let inlining_args = Term.(pure make_inlining_args $ round $ inline_branch_factor $
                          inline $ inline_toplevel $ inline_alloc_cost $
                          inline_branch_cost $ inline_prim_cost $ inline_call_cost $
                          inline_indirect_cost $ inline_lifting_benefit $
                          inline_max_depth $ unbox_closures $ unbox_closures_factor $
                          inline_max_unroll $ remove_unused_arguments $
                          inline_max_specialise $ round_2_multiplier $ round_3_multiplier)

let run_cmd =
  let bench =
    let doc = "Name of the bench file" in
    Arg.(value & pos 0 string "" & info [] ~docv:"<file>" ~doc)
  in
  let doc = "Run macrobenchmarks from files." in
  let man = [
    `S "DESCRIPTION";
    `P "Run macrobenchmarks from files."] @ help_secs
  in
  Term.(pure run $ inlining_args $ copts_t $ switch $ bench ),
  Term.info "run" ~doc ~sdocs:copts_sect ~man


let list_cmd =
  let switches =
    let doc = "List installed benchmarks for this switch." in
    Arg.(value & pos_all string [] & info [] ~docv:"<switch>" ~doc) in
  let doc = "List installed OPAM benchmarks." in
  let man = [
    `S "DESCRIPTION";
    `P "List installed OPAM benchmarks."] @ help_secs
  in
  Term.(pure list $ copts_t $ switches),
  Term.info "list" ~doc ~sdocs:copts_sect ~man

(* Arguments common to summarize, rank. *)

let switches =
  let doc = "compiler switches to select" in
  Arg.(value & opt (list string) [] & info ["s";"switches"] ~docv:"switch name" ~doc)

let normalize =
  let doc = "Normalize against the value of a context_id (compiler)." in
  Arg.(value & opt string "" & info ["n"; "normalize"] ~docv:"context_id" ~doc)

let backend =
  let doc = "Select backend (one of 'sexp','csv','qt','svg')." in
  let enum_f =
    ["sexp", `Sexp; "csv", `Csv; ]
  in
  Arg.(value & opt (enum enum_f) `Sexp & info ["b"; "backend"] ~doc)

let no_normalize =
  let doc = "Don't normalize values." in
  Arg.(value & flag & info ["no-normalize"] ~doc)


let objective_function run base =
  let run =
    Summary.load_conv_exn run
  in
  let base =
    Summary.load_conv_exn base
  in
  let value x =
    Summary.get_mean run x /. Summary.get_mean base x
  in
  let v =
   value "cycles"
  in
  Printf.printf "%f\n" v

let function_cmd =
  let run =
    let doc = "Path to the results of the current run." in
    Arg.(required & opt (some string) None & info ["run"] ~doc)
  in
  let base =
    let doc = "Path to the results of the witness run." in
    Arg.(required & opt (some string) None & info ["base"] ~doc)
  in
  Term.(pure objective_function $ run $ base),
  Term.info "function"

let cmds = [help_cmd; run_cmd; list_cmd; function_cmd ]

let () = match Term.eval_choice ~catch:false default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
