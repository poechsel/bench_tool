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
  );

  (* Write the result in cache too if cache exists *)
  let rex = Re.Pcre.regexp " " in
  let name = res.Result.bench.Benchmark.name |> String.trim in
  let name = Re.Pcre.substitute ~rex ~subst:(fun _ -> "_") name in
  let interactive = copts.output = `None in
  let opamroot = copts.opamroot in
  try
    let res_file =
      Util.FS.(macro_dir / name / res.Result.context_id ^ ".result") in
    XDGBaseDir.mkdir_openfile
      (fun fn ->
         let filename = Filename.chop_extension fn in
         let outputname = filename ^ ".output" in
         Result.save_output outputname res;
         let res = Runner.run_check ?opamroot ~interactive res in
         Result.save_hum fn res) res_file
  with Not_found -> ()

let kind_of_file filename =
  let open Unix in
  try
    let st = Unix.stat filename in
    match st.st_kind with
    | S_REG -> `File
    | S_DIR -> `Directory
    | _     -> `Other_kind
  with Unix_error (ENOENT, _, _) -> `Noent

let is_benchmark_file filename =
  kind_of_file filename = `File &&
  Filename.check_suffix filename ".bench"

let run inlining_args copts switch selectors skip force fixed time_limit =
  let opamroot = copts.opamroot in
  let switch = match switch with
    | None -> Util.Opam.cur_switch ~opamroot
    | Some switch -> switch in
  let switch = try
      List.hd @@ Util.Opam.switches_matching ?opamroot switch
    with Failure _ ->
      Printf.eprintf "Pattern %s do not match any existing switch. Aborting.\n" switch;
      exit 1
  in
  let interactive = copts.output = `None in

  let already_run switch b =
    match
      Result.load_conv @@
      Util.FS.(macro_dir / b.Benchmark.name / switch ^ ".result")
    with
    | `Result _ -> true
    | _ -> false
    | exception Sys_error _ -> false
  in

  let files, names = List.partition Sys.file_exists selectors in
  let files = List.map (fun x -> ("ERROR", x)) files in
  let selectors = match files, names with
    | [], [] -> List.map snd @@ Benchmark.find_installed ?opamroot switch
    | files, [] -> files
    | _ ->
        if skip then
          Benchmark.find_installed ?opamroot ~glob:(`Exclude names) switch
          |> List.map snd
          |> List.append files
        else
          Benchmark.find_installed ?opamroot ~glob:(`Matching names) switch
          |> List.map snd
          |> List.append files
  in
  (* If selector is a file, run the benchmark in the file, if it is
     a directory, run all benchmarks in the directory *)
  let rec run_inner (modname, selector) =
    let run_bench filename =
      let open Benchmark in
      let b = load_conv_exn filename in
      let already_run = (already_run switch b && not force) in
      let already_run = false in
      let binary_missing =
        try Util.FS.is_file (List.hd b.cmd) <> Some true
        with _ -> true in
      if already_run || binary_missing
      then
        (if interactive then
           let reason = List.fold_left2
               (fun a b s -> if b then s::a else a) []
               [already_run; binary_missing]
               ["already run";
                Printf.sprintf "No binary at path \"%s\"" (List.hd b.cmd)]
           in let reason_str = String.concat ", " reason in
           Printf.printf "Skipping %s (%s)\n" b.name reason_str)
      else
        let res =
          Runner.run_exn ?opamroot ~inlining_args ~use_perf:true ~context_id:switch
            ~modname ~interactive ~fixed ~time_limit b
        in
        write_res_copts copts res
    in
    match kind_of_file selector with
    | `Other_kind ->
        Printf.eprintf "Warning: %s is not a file nor a directory.\n" selector
    | `Directory ->
        (* Get a list of .bench files in the directory and run them *)
        let benchs = Util.FS.ls selector in
        if interactive && benchs = [] && selectors <> [] then
          Printf.printf "No benchmark files (*.bench) found in %s.\n" selector
        else
          List.(map (Filename.concat selector) benchs
                |> filter is_benchmark_file
                |> iter run_bench)
    | `File ->
        List.iter run_bench [selector]
    | _ -> assert false
  in
  if interactive then
    Printf.printf "Running benchmarks installed in %s...\n" switch;
  List.iter run_inner selectors

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
        print @@ Benchmark.find_installed ?opamroot:copts.opamroot s;
        print_endline ""
      ) switches in
  let switches = match switches with
    | [] -> Util.Opam.switches ~opamroot:copts.opamroot
    | s ->
        StringList.settrip @@
        List.(flatten @@ map Util.Opam.switches_matching s) in
  print_all switches

(* [selectors] are bench _names_ *)
let summarize output ref_ctx_id pp selectors force ctx_ids no_normalize =
  (* [selectors] are directories hopefully containing .summary
     files. *)
  let selectors = match selectors with
    | [] -> SSet.of_list @@ Util.FS.[macro_dir; micro_dir]
    | ss -> List.fold_left
              (fun a s -> try
                  if Sys.is_directory s
                  then SSet.add s a (* selector is a directory, looking for content *)
                  else a (* selector is a file, do nothing *)
                with _ ->
                  (* Not a file nor a dir: benchmark glob expression *)
                  (try
                     let macro_benchs =
                       try Util.FS.(ls ~prefix:true  ~glob:s macro_dir) with _ -> [] in
                     let micro_benchs =
                       try Util.FS.(ls ~prefix:true  ~glob:s micro_dir) with _ -> [] in
                     SSet.union a @@ SSet.of_list (micro_benchs @ macro_benchs)
                   with Sys_error _ -> a)
              )
              SSet.empty ss
  in

  (* Make sure all .result files have an up-to-date corresponding
     .summary *)
  SSet.iter Summary.summarize_dir selectors;

  (* Create the DB *)
  let data = SSet.fold (fun dn db -> DB.of_dir ~acc:db dn)
      selectors DB.empty in

  (* Filter on context ids *)
  let data = match ctx_ids with
    | [] -> data
    | ctx_ids ->
        let res = List.map
            (fun p -> Re.Glob.globx ~anchored:true p |> Re.compile) ctx_ids in
        SMap.map
          (SMap.filter
             (fun ctx _ ->
                List.fold_left (fun a re -> Re.execp re ctx || a) false res
             ))
          data in

  (* Create the DB2 from DB *)
  let data = DB.fold_data
      (fun bench context_id topic measure a ->
         DB2.add topic bench context_id measure a
      )
      data DB2.empty in

  let data =
    if no_normalize
    then data
    else
      match ref_ctx_id with
      | "" -> DB2.normalize ~against:`Biggest data
      | context_id -> DB2.normalize ~against:(`Ctx context_id) data
  in
  match pp with
  | `Sexp ->
      (match output with
      | "" -> DB2.output_hum stdout Summary.Aggr.sexp_of_t data
      | fn -> DB2.save_hum fn Summary.Aggr.sexp_of_t data)
  | `Csv ->
      (match output with
       | "" -> ignore @@ DB2.to_csv stdout data
       | fn -> Util.File.with_oc_safe (fun oc -> ignore @@ DB2.to_csv oc data) fn)
  | _ -> failwith "Not implemented"

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

let time_limit =
  let doc = "Fixed minimun execution time" in
  Arg.(value & opt float 0. & info ["time-limit"] ~doc)

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

let make_inlining_args round inline_branch_factor inline inline_toplevel inline_alloc_cost
      inline_branch_cost inline_prim_cost inline_call_cost inline_indirect_cost
      inline_lifting_benefit inline_max_depth unbox_closures unbox_closures_factor
      inline_max_unroll remove_unused_arguments inline_max_specialise
  =
  let rs = string_of_int (round - 1) in
  let make_int_arg name v =
    match v with
    | None -> ""
    | Some i -> name ^ "=" ^ rs ^ "=" ^ string_of_int i
  in
  let make_float_arg name v =
    match v with
    | None -> ""
    | Some i -> name ^ "=" ^ rs ^ "=" ^ string_of_float i
  in
  let make_bool_arg name v =
    match v with
    | Some true -> name ^ "=1"
    | _ -> ""
  in
  let optimise_param =
    match round with
    | 2 -> "O2=1"
    | 3 -> "O3=1"
    | _ -> ""
  in
  let a =
    optimise_param ::
    make_float_arg "inline_branch_factor" inline_branch_factor ::
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
    make_int_arg "inline-max-unroll" inline_max_unroll ::
    make_bool_arg "remove-unused-arguments" remove_unused_arguments ::
    []
  in
  String.concat "," a ^ ",_"

let inlining_args = Term.(pure make_inlining_args $ round $ inline_branch_factor $ inline $ inline_toplevel $ inline_alloc_cost $
                 inline_branch_cost $ inline_prim_cost $ inline_call_cost $ inline_indirect_cost $
                 inline_lifting_benefit $ inline_max_depth $ unbox_closures $ unbox_closures_factor $
                 inline_max_unroll $ remove_unused_arguments $ inline_max_specialise )

let run_cmd =
  let force =
    let doc = "Force the execution of benchmarks even if \
               a result file is already present in the file system." in
    Arg.(value & flag & info ["f"; "force"] ~doc) in
  let skip =
    let doc = "Inverse benchmark selection. (only when arguments are package globs)." in
    Arg.(value & flag & info ["skip"] ~docv:"benchmark list" ~doc)
  in
  let fixed =
    let doc = "Fixed execution time." in
    Arg.(value & flag & info ["fixed"] ~doc)
  in
  let selector =
    let doc = "If the argument is the path to an existing file, \
               it is taken as a benchmark file (.bench), otherwise \
               the argument is treated as an OPAM package shell pattern. \
               If missing, all OPAM benchmarks installed in \
               the current switch (or the one specified) are executed." in
    Arg.(value & pos_all string [] & info [] ~docv:"<file|package_glob>" ~doc)
  in
  let doc = "Run macrobenchmarks from files." in
  let man = [
    `S "DESCRIPTION";
    `P "Run macrobenchmarks from files."] @ help_secs
  in
  Term.(pure run $ inlining_args $ copts_t $ switch $ selector $ skip $ force $ fixed $ time_limit),
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

let summarize_cmd =
  let force =
    let doc = "Force rebuilding the summary files." in
    Arg.(value & flag & info ["f"; "force"] ~doc) in
  let selector =
    let doc = "If the argument is the path to an existing file, it is taken \
               as a .result file, otherwise it is treated as \
               a shell pattern (glob) matching a benchmark name. \
               If missing, all results of previously ran benchmarks are used." in
    Arg.(value & pos_all string [] & info [] ~docv:"<file|benchmark_glob>" ~doc)
  in
  let doc = "Produce a summary of the result of the desired benchmarks." in
  let man = [
    `S "DESCRIPTION";
    `P "Produce a summary of the result of the desired benchmarks."] @ help_secs
  in
  Term.(pure summarize $ output_file $ normalize $ backend $ selector $ force $ switches $ no_normalize),
  Term.info "summarize" ~doc ~man

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

let cmds = [help_cmd; run_cmd; summarize_cmd; list_cmd; function_cmd ]

let () = match Term.eval_choice ~catch:false default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
