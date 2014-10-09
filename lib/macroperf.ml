open Sexplib.Std

module type SEXPABLE = sig
  type t
  val t_of_sexp : Sexplib.Type.t -> t
  val sexp_of_t : t -> Sexplib.Type.t
end

module Util = struct
  module FS = struct
    let (/) = Filename.concat
    let home = Unix.getenv "HOME"
    let cache_dir = XDGBaseDir.Cache.user_dir ~exists:true () / "operf" / "macro"

    let ls dirname =
      let dh = Unix.opendir dirname in
      let rec loop acc =
        match Unix.readdir dh with
        | name -> loop (name::acc)
        | exception End_of_file -> acc
      in loop []

    let with_ls dirname f =
      let files = ls dirname in
      List.iter f @@ List.filter (fun f -> f <> "." && f <> "..") files

    let rm_rf n =
      let rec rm_rf acc n =
        let open Unix in
        let stats = stat n in
        match stats.st_kind with
        | S_DIR -> with_ls n (fun c -> rm_rf (n::acc) (n / c))
        | _ -> unlink n; List.iter (fun d -> try rmdir d with _ -> ()) acc
      in rm_rf [] n
  end

  module File = struct
    let string_of_ic ic =
      let buf = Buffer.create 4096 in
      let buf_str = Bytes.create 4096 in
      let rec drain () =
        let nb_read = input ic buf_str 0 4096 in
        if nb_read > 0 then
          (Buffer.add_subbytes buf buf_str 0 nb_read;
           drain ())
        else
          Buffer.contents buf
      in drain ()

    let lines_of_ic ic =
      let rec get_line acc = match input_line ic with
        | line -> get_line (line::acc)
        | exception End_of_file -> acc
      in get_line []

    let string_of_file filename =
      let ic = open_in filename in
      try
        let res = really_input_string ic @@ in_channel_length ic
        in close_in ic; res
      with exn ->
        close_in ic; raise exn

    let sexp_of_file_exn filename conv =
      let module SSA = Sexplib.Sexp.Annotated in
      match Sexplib.Sexp.load_sexp_conv filename conv
      with
      | `Error (exn, SSA.Atom ({SSA.start_pos; SSA.end_pos}, _))
      | `Error (exn, SSA.List ({SSA.start_pos; SSA.end_pos; _}, _, _)) ->
          let open SSA in
          Printf.eprintf "%s: Error at line %d, col %d-%d.\n"
            filename start_pos.line start_pos.col end_pos.col;
          raise exn
      | `Result b -> b

    let lines_of_file filename =
      let ic = open_in filename in
      try
        let res = lines_of_ic ic in close_in ic; res
      with exn ->
        close_in ic; raise exn

    let write_string_to_file ~fn str =
      let oc = open_out fn in
      try
        Printf.fprintf oc "%s" str;
        close_out oc
      with exn ->
        close_out oc; raise exn
  end

  module Cmd = struct
    let stdout_of_cmd cmd_string =
    let ic = Unix.open_process_in cmd_string in
    try
      let res = File.string_of_ic ic in Unix.close_process_in ic, res
    with exn ->
      let _ = Unix.close_process_in ic in raise exn
  end

  module Opam = struct
    include FS
    let root = try Unix.getenv "OPAMROOT" with Not_found -> home / ".opam"

    let switch =
      let rex = Re_pcre.regexp "switch: \"([^\"]*)\"" in
      let config_lines = File.lines_of_file @@ root / "config" in
      List.fold_left
        (fun a l ->
           try
             let substrings = Re_pcre.exec ~rex l in
             Re_pcre.get_substring substrings 1
           with _ -> a
        )
        "" config_lines

    let swtch = switch

    let share ?switch () = match switch with
      | None -> root / swtch / "share"
      | Some s -> root / s / "share"
  end
end

module Topic = struct
  type time = [ `Real | `User | `Sys ] with sexp
  type gc =
    [ `Minor_words
    | `Promoted_words
    | `Major_words
    | `Minor_collections
    | `Major_collections
    | `Heap_words
    | `Heap_chunks
    | `Top_heap_words
    | `Live_words
    | `Live_blocks
    | `Free_words
    | `Free_blocks
    | `Largest_free
    | `Fragments
    | `Compactions
    ] with sexp

  let gc_of_string_exn : string -> gc = function
    | "minor_words"       -> `Minor_words
    | "promoted_words"    -> `Promoted_words
    | "major_words"       -> `Major_words
    | "minor_collections" -> `Minor_collections
    | "major_collections" -> `Major_collections
    | "heap_words"        -> `Heap_words
    | "heap_chunks"       -> `Heap_chunks
    | "top_heap_words"    -> `Top_heap_words
    | "live_words"        -> `Live_words
    | "live_blocks"       -> `Live_blocks
    | "free_words"        -> `Free_words
    | "free_blocks"       -> `Free_blocks
    | "largest_free"      -> `Largest_free
    | "fragments"         -> `Fragments
    | "compactions"       -> `Compactions
    | _ -> invalid_arg "gc_of_string_exn"

  let gc_of_string s = try Some (gc_of_string_exn s) with _ -> None

  type _ kind =
    (* Time related *)
    | Time : time kind

    (* GC related *)
    | Gc : gc kind

    (* Use the ocaml-perf binding to perf_event_open(2). *)
    | Libperf : Perf.Attr.kind kind

    (* Use the perf-stat(1) command (need the perf binary, linux
       only) *)
    | Perf : string kind

  type t = Topic : 'a * 'a kind -> t

  let sexp_of_t t =
    let open Sexplib.Sexp in
    match t with
    | Topic (time, Time) -> List [Atom "Time"; sexp_of_time time]
    | Topic (gc, Gc) -> List [Atom "Gc"; sexp_of_gc gc]
    | Topic (libperf, Libperf) -> List [Atom "Libperf"; Perf.Attr.sexp_of_kind libperf]
    | Topic (perf, Perf) -> List [Atom "Perf"; sexp_of_string perf]

  let t_of_sexp s =
    let open Sexplib.Sexp in
    match s with
    | List [Atom "Time"; t] -> Topic (time_of_sexp t, Time)
    | List [Atom "Gc"; t] -> Topic (gc_of_sexp t, Gc)
    | List [Atom "Libperf"; t] -> Topic (Perf.Attr.kind_of_sexp t, Libperf)
    | List [Atom "Perf"; t] -> Topic (string_of_sexp t, Perf)
    | _ -> invalid_arg "t_of_sexp"

  let compare = Pervasives.compare
end

module Measure = struct
  type t = [ `Int of int64 | `Float of float | `Error ] with sexp
  let of_string s =
    try `Int (Int64.of_string s) with _ ->
      try `Float (float_of_string s) with _ ->
        `Error

  let of_float f = `Float f
  let to_float = function
    | `Float f -> f
    | `Int i -> Int64.to_float i
    | _ -> invalid_arg "Measure.to_float: cannot convert `Error to float"
  let of_int64 i = `Int i
  let to_int64 = function
    | `Int i -> i
    | _ -> invalid_arg "Measure.to_int64"
end

module Execution = struct
  type process_status = Unix.process_status

  let sexp_of_process_status ps =
    let open Unix in
    let open Sexplib.Sexp in
    match ps with
    | WEXITED n -> List [Atom "WEXITED"; sexp_of_int n]
    | WSIGNALED n -> List [Atom "WSIGNALED"; sexp_of_int n]
    | WSTOPPED n -> List [Atom "WSTOPPED"; sexp_of_int n]

  let process_status_of_sexp s =
    let open Unix in
    let open Sexplib.Sexp in
    match s with
    | List [Atom "WEXITED"; n] -> WEXITED (int_of_sexp n)
    | List [Atom "WSIGNALED"; n] -> WSIGNALED (int_of_sexp n)
    | List [Atom "WSTOPPED"; n] -> WSTOPPED (int_of_sexp n)
    | _ -> invalid_arg "process_status_of_sexp"

  type exec = {
    process_status: process_status;
    stdout: string;
    stderr: string;
    data: (Topic.t * Measure.t) list;
    checked: bool option
  } with sexp

  type t = [ `Ok of exec | `Timeout | `Error of string ]
  with sexp
  (** Type representing the execution of a benchmark. *)

  let error exn = `Error Printexc.(to_string exn)

  let strip chan t = match t, chan with
    | `Timeout, _
    | `Error _, _ -> t
    | `Ok e, `Stdout -> `Ok { e with stdout="" }
    | `Ok e, `Stderr -> `Ok { e with stderr="" }

  let find kind exec =
    List.filter (fun (t, m) -> t = kind) exec.data

  let duration exec =
    List.hd (find Topic.(Topic (`Real, Time)) exec) |> snd |> Measure.to_int64
end

module Benchmark = struct

  type speed = [`Fast | `Slow | `Slower] with sexp

  type t = {
    name: string;
    descr: string option;
    cmd: string list;
    cmd_check: string list;
    env: string list option;
    speed: speed;
    timeout: int;
    topics: Topic.t list;
  } with sexp

  let make ~name ?descr ~cmd ?(cmd_check=[])
      ?env ~speed ?(timeout=600) ~topics () =
    { name; descr; cmd; cmd_check; env; speed; timeout; topics; }
end

module Result = struct
  type t = {
    src: Benchmark.t;
    context_id: string;
    execs: Execution.t list;
  } with sexp

  let make ~src ?(context_id="") ~execs () = { src; context_id; execs; }

  let strip chan t = match chan with
    | `Stdout ->
        { t with execs = List.map (Execution.strip `Stdout) t.execs }
    | `Stderr ->
        { t with execs = List.map (Execution.strip `Stderr) t.execs }
end

module Summary = struct
  type aggr = { mean: float; stddev: float } with sexp

  type t = {
    name: string;
    context_id: string;
    data: (Topic.t * aggr) list;
  } with sexp

  type all = ((string * string), t) Hashtbl.t with sexp

  let of_result r =
    let data = Hashtbl.create 13 in
    List.iter
      (function
        | `Ok e ->
            List.iter
              (fun (t, m) ->
                 if m = `Error then ()
                 else
                   let m = Measure.to_float m in
                   try Hashtbl.replace data t @@ m::(Hashtbl.find data t)
                   with Not_found -> Hashtbl.add data t [m])
              e.Execution.data
        | _ -> ()
      )
      r.Result.execs;

    let data =
      Hashtbl.fold
        (fun k v a ->
           let mean, variance = Statistics.mean_variance v in
           (k, { mean; stddev = sqrt variance })::a
        )
        data []
    in
    { name = r.Result.src.Benchmark.name;
      context_id = r.Result.context_id;
      data;
    }
end

module Process = struct

  type speed_characterization = {
    max_duration: int64;
    probability: float;
    confidence: float;
  }

  let fast = {
    max_duration = 100_000_000L;
    probability = 0.99;
    confidence = 0.05;
  }
  let slow = {
    max_duration = 1_000_000_000L;
    probability = 0.99;
    confidence = 0.05;
  }
  let slower = {
    max_duration = 300_000_000_000L;
    probability = 0.99;
    confidence = 0.05;
  }

  let run ?(fast=fast) ?(slow=slow) ?(slower=slower) f =

    let run_until ~probability ~confidence init_acc =
      let rec run_until (nb_iter, acc) =
        let durations = List.map
            (function
              |`Ok e -> Execution.duration e |> Int64.to_float
              | _ -> 0.
            ) acc in
        match Statistics.enough_samples ~probability ~confidence durations with
        | true ->
            Printf.printf " %d times... " nb_iter;
            acc
        | false ->
            let exec = f () in
            run_until (succ nb_iter, (exec::acc))
      in
      run_until (1, init_acc)
    in
    let exec = f () in
    (* Remove the OCAML_GC_STATS env variable. *)
    Unix.putenv "OCAML_GC_STATS" "";
    match exec with
    | `Ok e ->
        let duration = Execution.duration e in
        (match duration with
        | t when t < fast.max_duration -> (* Fast *)
            run_until
              ~probability:fast.probability
              ~confidence:fast.confidence []
        | t when t < slow.max_duration -> (* Slow *)
            run_until
              ~probability:slow.probability
              ~confidence:slow.confidence []
        | t ->                            (* Slower: keep the first execution *)
            run_until
              ~probability:slower.probability
              ~confidence:slower.confidence [exec])
    | other -> [other]

  let data_of_gc_stats () =
    let lines = Util.File.lines_of_file "gc_stats" in
    List.map
      (fun s ->
         let i = String.index s ':' in
         let gc = Topic.gc_of_string_exn @@ String.sub s 0 (i-1) in
         let v = Int64.of_string @@ String.sub s (i+1) (String.length s - i - 1) in
         (Topic.(Topic (gc, Gc), Measure.of_int64 v))
      )
      lines
end

module Perf_wrapper = struct
  include Process

  let run_once ?env ?timeout ?chk_cmd cmd evts =
    let perf_cmdline = ["perf"; "stat"; "-x,"; ] in
    let perf_cmdline = match evts with
      | [] -> perf_cmdline
      | _ -> perf_cmdline @ ["-e"; String.concat "," evts] in
    let cmd = perf_cmdline @ cmd in
    let env = match env with
      | None -> [|"LANG=C"|]
      | Some env -> Array.of_list @@ "LANG=C"::env in
    let cmd_string = String.concat " " cmd in
    let time_start = Oclock.(gettime monotonic) in
    let p_stdout, p_stdin, p_stderr = Unix.open_process_full cmd_string env in
    try
      let stdout_string = Util.File.string_of_ic p_stdout in
      Util.File.write_string_to_file "stdout" stdout_string;
      let stderr_lines = Util.File.lines_of_ic p_stderr in
      (* Setup an alarm that will make Unix.close_process_full raise
         EINTR if its process is not terminated by then *)
      let (_:int) = match timeout with None -> 0 | Some t -> Unix.alarm t in
      Sys.(set_signal sigalrm (Signal_handle (fun _ -> ())));
      let process_status =  Unix.close_process_full (p_stdout, p_stdin, p_stderr) in
      let time_end = Oclock.(gettime monotonic) in
      let rex = Re.(str "," |> compile) in
      let stderr_lines = List.map (Re_pcre.split ~rex) stderr_lines in
      `Ok Execution.{
          process_status;
          stdout=stdout_string;
          stderr=""; (* Perf writes its result on stderr... *)
          data=(List.fold_left
                  (fun acc l -> match l with
                     | [v;"";event; ]
                     | [v;"";event; _] ->
                         (Topic.(Topic (event, Perf)), Measure.of_string v)::acc
                     | l ->
                         Printf.printf
                           "Ignoring perf result line [%s]" (String.concat "," l);
                         acc
                  )
                  ((try data_of_gc_stats () with _ -> []) @
                   [Topic.(Topic (`Real, Time), `Int Int64.(rem time_end time_start))])
                  stderr_lines);
          checked=(match chk_cmd with
              | None -> None
              | Some chk -> (match Sys.command (String.concat " " chk)
                             with 0 -> Some true | _ -> Some false))
        }
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> `Timeout
    | exn ->
        ignore @@ Unix.close_process_full (p_stdout, p_stdin, p_stderr);
        Execution.error exn

  let run ?env ?timeout ?chk_cmd cmd evts =
    run (fun () -> run_once ?env ?timeout ?chk_cmd cmd evts)
end

module Libperf_wrapper = struct
  include Process

  let run_once ?env ?timeout ?chk_cmd cmd attrs =
    let open Perf in
    let attrs = List.map Perf.Attr.make attrs in
    (* /!\ Perf.with_process <> Process.with_process, but similar /!\ *)
    Perf.with_process
      ?env ?timeout ~stdout:"stdout" ~stderr:"stderr" cmd attrs |> function
    | `Ok {process_status; stdout; stderr; duration; data;} ->
        let data = List.map (fun (k, v) ->
            Topic.(Topic (k, Libperf)), `Int v) data in
        let data = (Topic.(Topic ((`Real, Time))), `Int duration)::data in
        let data = try data @ data_of_gc_stats () with _ -> data in
        let checked = match chk_cmd with
          | None -> None
          | Some chk -> (match Sys.command (String.concat " " chk) with
              | 0 -> Some true | _ -> Some false) in
        `Ok Execution.{ process_status; stdout; stderr; data; checked; }
    | `Timeout -> `Timeout
    | `Exn e -> Execution.error e

  let run ?env ?timeout cmd evts =
    run (fun () -> run_once ?env ?timeout cmd evts)
end

module Runner = struct
  type execs = {
    time: Topic.time list;
    gc: Topic.gc list;
    libperf: Perf.Attr.kind list;
    perf: string list;
  }

  let run_exn b =
    let open Benchmark in

    (* We run benchmarks in a temporary directory that we create now. *)
    let temp_dir = Filename.temp_file "macrorun" "" in
    Unix.unlink temp_dir;
    Unix.(try
       mkdir temp_dir 0o755
     with Unix_error (EEXIST, _, _) -> ());
    Unix.chdir temp_dir;

    let env = match b.env with
      | None -> ["OCAML_GC_STATS=gc_stats"] @
                Array.to_list @@ Unix.environment ()
      | Some e -> "OCAML_GC_STATS=gc_stats"::e
    in

    (* Transform individial topics into a list of executions *)
    let execs =
      let open Topic in
      List.fold_left
        (fun a -> function
           | Topic (t, Time) -> { a with time=t::a.time }
           | Topic (t, Gc) -> { a with gc=t::a.gc }
           | Topic (t, Libperf) -> { a with libperf=t::a.libperf }
           | Topic (t, Perf) -> { a with perf=t::a.perf }
        )
        {time=[]; gc=[]; libperf=[]; perf=[];}
        b.topics in

    let run_execs { time; gc; libperf; perf; } b =
      (* Launch the executions only if the list of topics is
         non-empty. *)
      let libperf_res = match libperf with
        | [] -> []
        | libperf -> Libperf_wrapper.(run ~env b.cmd libperf) in
      let perf_res = match perf with
        | [] -> []
        | perf -> Perf_wrapper.(run ~env b.cmd perf) in
      (libperf_res @ perf_res)
    in

    let execs = run_execs execs b in

    (* Cleanup temporary directory *)
    Util.FS.rm_rf temp_dir;
    Result.make ~context_id:Util.Opam.switch ~src:b ~execs ()

  let run b =
    try Some (run_exn b) with _ -> None
end
