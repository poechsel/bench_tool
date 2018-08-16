open Sexplib.Std

module Sexpable = struct
  module type S = sig
    type t [@@deriving sexp]
  end
  module type S1 = sig
    type 'a t [@@deriving sexp]
  end
  module type S2 = sig
    type ('a, 'b) t [@@deriving sexp]
  end
  module type S3 = sig
    type ('a, 'b, 'c) t [@@deriving sexp]
  end
end

module SSet = Set.Make(String)

module Opt = struct
  let run = function
    | Some v -> v
    | None -> invalid_arg "Opt.run"

  let default d = function
    | Some v -> v
    | None -> d
end

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

  let last_n_lines count str =
    let rec aux acc count i =
      if count <= 0 || i < 0 then acc else
        let j = try Some (String.rindex_from str i '\n') with Not_found -> None in
        match j with
        | Some j ->
            aux (String.sub str (j+1) (i - j) :: acc) (count - 1) (j-1)
        | None -> String.sub str 0 (i + 1) :: acc
    in
    aux [] count (String.length str - 1)
end

module Util = struct
  module FS = struct
    let (/) = Filename.concat
    let home = Unix.getenv "HOME"
    let cache_dir = XDGBaseDir.Cache.user_dir () / "operf"
    let micro_dir = cache_dir / "micro"
    let macro_dir = cache_dir / "macro"

    let ls ?(preserve_order=false) ?(prefix=false) ?glob dirname =
      let dh = Unix.opendir dirname in
      try
        let rec loop acc =
          match Unix.readdir dh with
          | n when n <> "." && n <> ".." ->
              let n' = if prefix then dirname / n else n
              in
              (match glob with
               | None -> loop (n'::acc)
               | Some pat ->
                   let re = Re.Glob.globx ~anchored:true pat |> Re.compile in
                   if Re.execp re n then loop (n'::acc) else loop acc)
          | _ -> loop acc
          | exception End_of_file ->
              if preserve_order then List.rev acc else acc
        in
        let r = loop [] in
        Unix.closedir dh;
        r
      with e -> Unix.closedir dh; raise e

    let rec iter f n =
      let open Unix in
      match (stat n).st_kind with
      | S_DIR -> List.iter f @@ ls ~prefix:true n; f n
      | _ -> f n

    let rec fold f acc n =
      let open Unix in
      match (stat n).st_kind with
      | S_DIR -> f (List.fold_left (fold f) acc @@ ls ~prefix:true n) n
      | _ -> f acc n

    let rec fold_files f acc n =
      let open Unix in
      match (stat n).st_kind with
      | S_DIR -> List.fold_left (fold_files f) acc @@ ls ~prefix:true n
      | _ -> f acc n

    let rm_r fns =
      List.iter
        (iter
           Unix.(fun n -> match (stat n).st_kind with
               | S_DIR -> rmdir n
               | _ -> unlink n)
        ) fns

    let kind_exn fn = Unix.((stat fn).st_kind)
    let is_file_exn fn = Unix.((stat fn).st_kind = S_REG)
    let is_dir_exn fn = Unix.((stat fn).st_kind = S_DIR)

    let kind fn = try Some (kind_exn fn) with _ -> None
    let is_file fn = try Some (is_file_exn fn) with _ -> None
    let is_dir fn = try Some (is_dir_exn fn) with _ -> None
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
        | exception End_of_file -> List.rev acc
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

    let with_oc_safe f fn =
      let oc = open_out fn in
      try let res = f oc in close_out oc; res
      with exn -> close_out oc; raise exn

    let with_ic_safe f fn =
      let ic = open_in fn in
      try let res = f ic in close_in ic; res
      with exn -> close_in ic; raise exn
  end

  module Cmd = struct
    let with_process_in_safe f cmd_string =
      let p_stdout = Unix.open_process_in cmd_string in
      try
        let res = f p_stdout in Unix.close_process_in p_stdout, res
      with exn ->
        let _ = Unix.close_process_in p_stdout in raise exn

    let with_process_full_safe f cmd_string =
      let p_stdout, p_stdin = Unix.open_process cmd_string in
      try
        let res = f p_stdout p_stdin in Unix.close_process (p_stdout, p_stdin), res
      with exn ->
        let _ = Unix.close_process (p_stdout, p_stdin) in raise exn

    let lines_of_cmd cmd_string = with_process_in_safe File.lines_of_ic cmd_string

    let path_of_exe n =
      List.hd @@ snd @@ lines_of_cmd @@ "command -v " ^ n
  end

  module Opam = struct
    include FS
    let root = try Unix.getenv "OPAMROOT" with Not_found -> home / ".opam"
    let exe = "OPAMROOT=~/.opam2 ~/.opam/4.07.0/lib/opam-devel/opam"

    let call_opam ~opamroot args =
      (*let root = Opt.default root opamroot in*)
      let cmd = String.concat " " (List.map String.escaped (exe::args)) in
      let res, result = Cmd.lines_of_cmd cmd in
      match res with
      | WEXITED code -> (code = 0), result
      | _ -> failwith ("opam call failed: "^cmd)

    let cur_switch ~opamroot =
      match call_opam ~opamroot ["switch"; "show"] with
      | true, [switch] -> switch
      | _ -> failwith "Current switch query failed"

    let switches ~opamroot =
      match call_opam ~opamroot ["switch"; "-s"] with
      | true, switches -> switches
      | false, _ -> failwith "Switch list query failed"

    let switches_matching ?opamroot glob =
      let re = Re.Glob.globx ~anchored:true glob |> Re.compile in
      List.filter (fun s -> Re.execp re s) (switches ~opamroot)

    let share ?opamroot s =
      match call_opam ~opamroot ["var"; "share"; "--switch"; s] with
      | true, [dir] -> dir
      | _ -> failwith "opam var query failed"

    let compiler_path ?opamroot switch =
      let s =
        match switch with
        | None -> []
        | Some x -> ["--switch"; x]
      in
      match call_opam ~opamroot (["var"; "bin"] @ s) with
      | true, [dir] -> dir
      | _ -> failwith "opam var query failed"

    let use_compiler_switch ?opamroot switch =
      "PATH=" ^ compiler_path ?opamroot switch ^ ":$PATH"
  end
end

module Topic = struct
  module Time = struct
    type t = Real | User | Sys | Compile [@@deriving sexp, variants]
    let of_string_exn = function
      | "time_real" -> Real
      | "time_user" -> User
      | "time_sys" -> Sys
      | "time_compile" -> Compile
      | _ -> invalid_arg "time_of_string"

    let to_string = function
      | Real -> "time_real"
      | User -> "time_user"
      | Sys -> "time_sys"
      | Compile -> "time_compile"

    let compare = compare
  end

  module TimeSet = Set.Make(Time)

  module Gc = struct
    type t =
      | Minor_words
      | Major_words
      | Promoted_words
      | Top_heap_words
      | Minor_collections
      | Major_collections
      | Compactions
      | Heap_words
      | Heap_chunks
      | Live_words
      | Live_blocks
      | Free_words
      | Free_blocks
      | Largest_free
      | Fragments
    [@@deriving sexp, variants]

    let of_string_exn : string -> t = function
      | "minor_words"       -> Minor_words
      | "promoted_words"    -> Promoted_words
      | "major_words"       -> Major_words
      | "minor_collections" -> Minor_collections
      | "major_collections" -> Major_collections
      | "heap_words"        -> Heap_words
      | "heap_chunks"       -> Heap_chunks
      | "top_heap_words"    -> Top_heap_words
      | "live_words"        -> Live_words
      | "live_blocks"       -> Live_blocks
      | "free_words"        -> Free_words
      | "free_blocks"       -> Free_blocks
      | "largest_free"      -> Largest_free
      | "fragments"         -> Fragments
      | "compactions"       -> Compactions
      | _ -> invalid_arg "gc_of_string_exn"

    let of_string s = try Some (of_string_exn s) with _ -> None

    let to_string = function
      | Minor_words -> "minor_words"
      | Promoted_words -> "promoted_words"
      | Major_words -> "major_words"
      | Minor_collections -> "minor_collections"
      | Major_collections -> "major_collections"
      | Heap_words -> "heap_words"
      | Heap_chunks -> "heap_chunks"
      | Top_heap_words -> "top_heap_words"
      | Live_words -> "live_words"
      | Live_blocks -> "live_blocks"
      | Free_words -> "free_words"
      | Free_blocks -> "free_blocks"
      | Largest_free -> "largest_free"
      | Fragments -> "fragments"
      | Compactions -> "compactions"

    let compare = compare
  end

  module GcSet = Set.Make(Gc)

  module Perf = struct
    type t =
      (** Hardware *)
      | Cycles
      | Instructions
      | Cache_references
      | Cache_misses
      | Branch_instructions
      | Branch_misses
      | Bus_cycles
      | Stalled_cycles_frontend
      | Stalled_cycles_backend
      | Ref_cpu_cycles
      (** Software *)
      | Cpu_clock
      | Task_clock
      | Page_faults
      | Context_switches
      | Cpu_migrations
      | Page_faults_min
      | Page_faults_maj
      | Alignment_faults
      | Emulation_faults
      | Dummy
    [@@deriving sexp, variants]

    let to_string = function
      | Cycles -> "cycles"
      | Instructions -> "instructions"
      | Cache_references -> "cache_references"
      | Cache_misses -> "cache_misses"
      | Branch_instructions -> "branch_instructions"
      | Branch_misses -> "branch_misses"
      | Bus_cycles -> "bus_cycles"
      | Stalled_cycles_frontend -> "stalled_cycles_frontend"
      | Stalled_cycles_backend -> "stalled_cycles_backend"
      | Ref_cpu_cycles -> "ref_cpu_cycles"

      (** Software *)
      | Cpu_clock -> "cpu_clock"
      | Task_clock -> "task-clock"
      | Page_faults -> "page_faults"
      | Context_switches -> "context_switches"
      | Cpu_migrations -> "cpu_migrations"
      | Page_faults_min -> "page_faults_min"
      | Page_faults_maj -> "page_faults_maj"
      | Alignment_faults -> "alignment_faults"
      | Emulation_faults -> "emulation_faults"
      | Dummy -> "dummy"

    let of_string_exn  s =

      match s with
      | "cycles" -> Cycles
      | "instructions" -> Instructions
      | "cache_references" -> Cache_references
      | "cache_misses" -> Cache_misses
      | "branch_instructions" -> Branch_instructions
      | "branch_misses" -> Branch_misses
      | "bus_cycles" -> Bus_cycles
      | "stalled_cycles_frontend" -> Stalled_cycles_frontend
      | "stalled_cycles_backend" -> Stalled_cycles_backend
      | "ref_cpu_cycles" -> Ref_cpu_cycles
      (** Software *)
      | "cpu_clock" -> Cpu_clock
      | "task-clock" -> Task_clock
      | "page_faults" -> Page_faults
      | "context_switches" -> Context_switches
      | "cpu_migrations" -> Cpu_migrations
      | "page_faults_min" -> Page_faults_min
      | "page_faults_maj" -> Page_faults_maj
      | "alignment_faults" -> Alignment_faults
      | "emulation_faults" -> Emulation_faults
      | "dummy" -> Dummy
      | _ -> invalid_arg "kind_of_sexp"

    let of_string s = try Some (of_string_exn s) with _ -> None

    let compare = compare
  end
  module PerfSet = Set.Make(Perf)

  module Size = struct
    type t =
      | Full
      | Code
      | Data
    [@@deriving sexp, variants]

    let of_string_exn : string -> t = function
      | "size" -> Full
      | "code_size" -> Code
      | "data_size" -> Data
      | _ -> invalid_arg "size_of_string_exn"

    let of_string s = try Some (of_string_exn s) with _ -> None

    let to_string = function
      | Full -> "size"
      | Code -> "code_size"
      | Data -> "data_size"

    let compare = compare
  end

  type _ kind =
    (* Time related *)
    | Time : Time.t kind

    (* GC related *)
    | Gc : Gc.t kind

    (* Use the perf-stat(1) command or ocaml-libperf *)
    | Perf : Perf.t kind

    (* The executable size *)
    | Size : Size.t kind

  type t = Topic : 'a * 'a kind -> t

  let of_string s =
    try Topic (Size.of_string_exn s, Size)
    with Invalid_argument _ ->
    try Topic (Gc.of_string_exn s, Gc)
    with Invalid_argument _ ->
    try Topic (Perf.of_string_exn s, Perf)
    with _ ->
      Topic (Time.of_string_exn s, Time)

  let to_string t =
    match t with
    | Topic (t, Time) -> Time.to_string t
    | Topic (gc, Gc) -> Gc.to_string gc
    | Topic (p, Perf) -> Perf.to_string p
    | Topic (sz, Size) -> Size.to_string sz

  let sexp_of_t t =
    let open Sexplib.Sexp in
    match t with
    | Topic (time, Time) -> List [Atom "Time"; Time.sexp_of_t time]
    | Topic (gc, Gc) -> List [Atom "Gc"; Gc.sexp_of_t gc]
    | Topic (perf, Perf) -> List [Atom "Perf"; Perf.sexp_of_t perf]
    | Topic (sz, Size) ->
        match sz with
        | Size.Full -> Atom "Size"
        | sz -> List [Atom "Size"; Size.sexp_of_t sz]

  let t_of_sexp s =
    let open Sexplib.Sexp in
    match s with
    | List [Atom "Time"; t] -> Topic (Time.t_of_sexp t, Time)
    | List [Atom "Gc"; t] -> Topic (Gc.t_of_sexp t, Gc)
    | List [Atom "Perf"; t] -> Topic (Perf.t_of_sexp t, Perf)
    | Atom "Size" -> Topic (Size.Full, Size)
    | List [Atom "Size"; t] -> Topic (Size.t_of_sexp t, Size)
    | _ -> invalid_arg "t_of_sexp"

  let compare a b = match a, b with
    | Topic (a, Time), Topic (b, Time) -> Time.compare a b
    | Topic (a, Gc), Topic (b, Gc) -> Gc.compare a b
    | Topic (a, Size), Topic (b, Size) -> Size.compare a b
    | Topic (a, Perf), Topic (b, Perf) -> Perf.compare a b
    | Topic (_, Time), _ -> -1
    | _, Topic (_, Time) -> 1
    | Topic (_, Size), _ -> -1
    | _, Topic (_, Size) -> 1
    | Topic (_, Gc), _ -> -1
    | _, Topic (_, Gc) -> 1

  let display_list () =
    let add_meta description string_topic =
      print_endline @@
      "  "
      ^ string_topic
      ^ (if description <> "" then ":  " ^ description else "")
    in
    let add_gc description x =
      add_meta description (Gc.to_string (x.Variantslib.Variant.constructor))
    in
    let add_perf description x =
      add_meta description (Perf.to_string (x.Variantslib.Variant.constructor))
    in
    let add_time description x =
      add_meta description (Time.to_string (x.Variantslib.Variant.constructor))
    in
    let add_size description x =
      add_meta description (Size.to_string (x.Variantslib.Variant.constructor))
    in
    print_endline "GC topics: ";
    Gc.Variants.iter
      ~minor_words:(add_gc "")
      ~major_words:(add_gc "")
      ~promoted_words:(add_gc "")
      ~top_heap_words:(add_gc "")
      ~minor_collections:(add_gc "")
      ~major_collections:(add_gc "")
      ~compactions:(add_gc "")
      ~heap_words:(add_gc "")
      ~heap_chunks:(add_gc "")
      ~live_words:(add_gc "")
      ~live_blocks:(add_gc "")
      ~free_words:(add_gc "")
      ~free_blocks:(add_gc "")
      ~largest_free:(add_gc "")
      ~fragments:(add_gc "");
    print_endline "\nPerf topics: ";
    Perf.Variants.iter
      ~cycles:(add_perf "")
      ~instructions:(add_perf "")
      ~cache_references:(add_perf "")
      ~cache_misses:(add_perf "")
      ~branch_instructions:(add_perf "")
      ~branch_misses:(add_perf "")
      ~bus_cycles:(add_perf "")
      ~stalled_cycles_frontend:(add_perf "")
      ~stalled_cycles_backend:(add_perf "")
      ~ref_cpu_cycles:(add_perf "")
      ~cpu_clock:(add_perf "")
      ~task_clock:(add_perf "")
      ~page_faults:(add_perf "")
      ~context_switches:(add_perf "")
      ~cpu_migrations:(add_perf "")
      ~page_faults_min:(add_perf "")
      ~page_faults_maj:(add_perf "")
      ~alignment_faults:(add_perf "")
      ~emulation_faults:(add_perf "")
      ~dummy:(add_perf "");
    print_endline "\nTime topics: ";
    Time.Variants.iter
      ~real:(add_time "")
      ~user:(add_time "")
      ~sys:(add_time "")
      ~compile:(add_time "");
    print_endline "\nSize topics: ";
    Size.Variants.iter
      ~full:(add_size "")
      ~code:(add_size "")
      ~data:(add_size "");

end


module TSet = struct
  include Set.Make(Topic)

  let t_of_sexp s =
    of_list @@ list_of_sexp Topic.t_of_sexp s

  let sexp_of_t t =
    elements t |> sexp_of_list Topic.sexp_of_t
end

module SMap = struct
  include Map.Make(String)

  let of_list l =
    List.fold_left (fun a (k,v) -> add k v a) empty l

  let filter_map f t =
    fold (fun k v a -> match f v with Some r -> add k r a | None -> a) t empty

  let filter_mapi f t =
    fold (fun k v a -> match f k v with Some r -> add k r a | None -> a) t empty

  type 'a bindings = (string * 'a) list [@@deriving sexp]

  let t_of_sexp sexp_of_elt s = bindings_of_sexp sexp_of_elt s |> of_list
  let sexp_of_t sexp_of_elt t = sexp_of_bindings sexp_of_elt @@ bindings t
end

module TMap = struct
  include Map.Make(Topic)

  let key_of_sexp = Topic.t_of_sexp
  let sexp_of_key = Topic.sexp_of_t

  let of_list l =
    List.fold_left (fun a (k,v) -> add k v a) empty l

  let filter_map f t =
    fold (fun k v a -> match f v with Some r -> add k r a | None -> a) t empty

  let filter_mapi f t =
    fold (fun k v a -> match f k v with Some r -> add k r a | None -> a) t empty

  type 'a bindings = (key * 'a) list [@@deriving sexp]

  let t_of_sexp sexp_of_elt s = bindings_of_sexp sexp_of_elt s |> of_list
  let sexp_of_t sexp_of_elt t = sexp_of_bindings sexp_of_elt @@ bindings t

  let lmerge ts =
    List.fold_left
      (fun a t -> merge
          (fun k v1 v2 -> match v1, v2 with
             | None, Some v -> Some [v]
             | Some aa, Some v -> Some (v::aa)
             | Some aa, None -> Some aa
             | _ -> invalid_arg "lmerge"
          )
          a t)
      empty ts
end

module Measure = struct
  type t = [ `Int of int64 | `Float of float | `Error ] [@@deriving sexp]
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


module Bench = struct
  type opam =
    {
      switch : string;
      bench : string;
    }
  type t =
    | Opam of opam
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
    data: Measure.t TMap.t;
    checked: bool option [@default None]; (* ignored, for compat *)
  } [@@deriving sexp]

  type t = [ `Ok of exec | `Timeout | `Error of string ]
  [@@deriving sexp]
  (** Type representing the execution of a benchmark. *)

  let error exn = `Error Printexc.(to_string exn)

  let strip chan t = match t, chan with
    | `Timeout, _
    | `Error _, _ -> t
    | `Ok e, `Stdout -> `Ok { e with stdout="" }
    | `Ok e, `Stderr -> `Ok { e with stderr="" }

  let find topic exec =
    TMap.filter (fun t m -> t = topic) exec.data

  let duration = function
    | `Ok e ->
        TMap.find Topic.(Topic (Time.Real, Time)) e.data |> Measure.to_int64
    | _ -> 0L
end

module BenchmarkOpam = struct

  type speed = [`Fast | `Slow | `Slower] [@@deriving sexp]

  type t = {
    name: string;
    descr: string [@default ""];
    cmd: string list;
    cmd_check: string list [@default []];
    file_check: (string * string) list [@default []];
    binary: string option [@default None];
    env: string list option [@default None];
    speed: speed [@default `Fast];
    timeout: int [@default 600];
    weight: float [@default 1.];
    discard: [`Stdout | `Stderr] list [@default []];
    topics: TSet.t [@default TSet.empty];
    return_value: int [@default 0];
  } [@@deriving sexp]

  let make ~name ?(descr="") ~cmd ?(cmd_check=[]) ?(file_check=[])
      ?binary ?env ~speed ?(timeout=600) ?(weight=1.) ?(discard=[]) ~topics
      ?(return_value=0) () =
    { name; descr; cmd; cmd_check; file_check; binary; env; speed; timeout;
      weight; discard; topics = TSet.of_list topics; return_value; }

  let cmd_exec t = t.cmd
  let name t = t.name

  let load_conv fn =
    Sexplib.Sexp.load_sexp_conv fn t_of_sexp

  let load_conv_exn fn =
    Util.File.sexp_of_file_exn fn t_of_sexp

  let save_hum fn s =
    sexp_of_t s |> Sexplib.Sexp.save_hum fn

  let output_hum oc s =
    sexp_of_t s |> Sexplib.Sexp.output_hum oc

  let find_installed ?opamroot ?(glob=`None) switch =
    let share = Util.Opam.share ?opamroot switch in
    (try Util.FS.ls share
     with Unix.Unix_error (Unix.ENOENT, _, _) -> [])
    |> List.map (fun n -> n, Filename.concat share n)
    |> List.filter (fun (_, n) -> Unix.((stat n).st_kind = S_DIR))
    |> List.map
      (fun (modname, selector) ->
         let bench_files =
           Util.FS.ls selector
           |> List.map (Filename.concat selector)
           |> List.filter (fun fn -> Filename.check_suffix fn ".bench")
           |> List.map (fun x -> (modname, x))
         in
         let bench_names = List.map
             (fun (_, fn) -> let b = load_conv_exn fn in b.name)
             bench_files in
         List.combine bench_names bench_files
      )
    |> List.flatten
    |> fun l -> match glob with
    | `None -> l
    | `Matching globs ->
        let res = List.map
            (fun re -> Re.compile @@ Re.Glob.globx ~anchored:true re) globs in
        List.filter_map (fun (name, bench) ->
            if List.(map (fun re -> Re.execp re name) res |> mem true)
            then Some (name, bench) else None
          ) l
    | `Exclude globs ->
        let res = List.map
            (fun re -> Re.compile @@ Re.Glob.globx ~anchored:true re) globs in
        List.filter_map (fun (name, bench) ->
            if List.(map (fun re -> Re.execp re name) res |> mem true)
            then None else Some (name, bench)
          ) l

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

  let load opamroot switch selectors =
    let switch = match switch with
      | None -> Util.Opam.cur_switch ~opamroot
      | Some switch -> switch in
    let switch = try
        List.hd @@ Util.Opam.switches_matching ?opamroot switch
      with Failure _ ->
        Printf.eprintf "Pattern %s do not match any existing switch. Aborting.\n" switch;
        exit 1
    in
    let selectors =
      if Sys.file_exists selectors then
        ["", selectors]
      else
        find_installed ?opamroot ~glob:(`Matching [selectors]) switch
        |> List.map snd
    in
    let rec load_inner previous (modname, selector) =
      let load_bench previous filename =
        let b = load_conv_exn filename in
        let binary_missing =
          try Util.FS.is_file (List.hd @@ cmd_exec b) <> Some true
          with _ -> true in
        if binary_missing
        then
          None
        else begin
          if previous <> None then begin
            Printf.eprintf "Several benches match the name. Aborting.\n";
            exit 1;
          end;
          Some (modname, b)
        end

      in
      match kind_of_file selector with
      | `Other_kind ->
        None
      | `Directory ->
        (* Get a list of .bench files in the directory and run them *)
        let benchs = Util.FS.ls selector in
        if benchs = [] && selectors <> [] then
          None
        else
          List.(map (Filename.concat selector) benchs
                |> filter is_benchmark_file
                |> fold_left load_bench previous)
      | `File ->
        load_bench previous selector
      | _ -> assert false
    in
    match List.fold_left load_inner None selectors with
    | None ->
      Printf.eprintf "No matching benches. Aborting.\n";
      exit 1;
    | Some x -> x
end

module Benchmark = struct
  type cmd_custom = {
    build : string list;
    exec : string list;
    dependency : string list [@default []];
    return_value : int [@default 1];
    env : string list option [@default None];
  } [@@deriving sexp]

  type cmd_sexp_t =
    | Opam
    | Custom of cmd_custom
  [@@deriving sexp]

  type sexp_t = {
    name : string;
    iter : int [@default 1];
    cmd : cmd_sexp_t;
  } [@@deriving sexp]

  type cmd_t =
    | Opam of string * BenchmarkOpam.t
    | Custom of cmd_custom

  type t = {
    name : string;
    iter : int;
    cmd : cmd_t;
  }

  let env t =
    match t.cmd with
    | Opam (_, o) -> o.env
    | Custom x -> x.env

  let topics t =
    match t.cmd with
    | Opam (_, o) -> o.topics
    | Custom x -> TSet.empty

  let export (t : t) : sexp_t =
    {
      name = t.name;
      iter = t.iter;
      cmd =
        match (t.cmd : cmd_t) with
        | Opam _ -> Opam
        | Custom x -> Custom x
    }

  let dependency_files t =
    match t.cmd with
    | Opam _ -> []
    | Custom x -> x.dependency


  let return_value t =
    match t.cmd with
    | Opam (_, o) -> o.return_value
    | Custom({return_value}) -> return_value

  let name t =
    t.name

  let convert opamroot switch (t : sexp_t) : t =
    {
      name = t.name;
      iter = t.iter;
      cmd =
        match (t.cmd : cmd_sexp_t) with
        | Opam ->
          let a, b = BenchmarkOpam.load opamroot switch t.name in
          Opam (a, b)
        | Custom x -> Custom x
    }

  let load_conv ~opamroot ~switch fn : t Sexplib.Sexp.Annotated.conv =
    let r =
      Sexplib.Sexp.load_sexp_conv fn sexp_t_of_sexp
    in
    match r with
    | `Result r ->
      `Result (convert opamroot switch r)
    | `Error x ->
      `Error x

  let load_conv_exn ~opamroot ~switch fn =
      Util.File.sexp_of_file_exn fn sexp_t_of_sexp
      |> convert opamroot switch

  let cmd_build ~opamroot ~switch ~inlining_args t =
    let params =
      "OCAMLPARAM=\"timings=1," ^ inlining_args ^ "\""
    in
    match t.cmd with
    | Custom ({build}) ->
          Util.Opam.use_compiler_switch ?opamroot switch ::
          params :: build
    | Opam (modname, _) ->
      params :: [Util.Opam.exe; "reinstall"; modname; "-vvvvvvv";] @
      (match switch with None -> [] | Some x ->  ["--switch"; x])

  let cmd_exec t =
    match t.cmd with
    | Custom ({exec}) ->
      exec
    | Opam (_, o) ->
      o.cmd

  let get_binary ?(absolute="") t =
    match t.cmd with
    | Opam (_, o) ->
      begin match o.binary with
      | Some file -> Some file
      | None -> match o.cmd with
        | [] -> None
        | cmd :: _ -> Some cmd
      end
    | Custom x ->
      match x.exec with
      | cmd :: _ ->
        let cmd =
          match absolute with
          | "" -> cmd
          | a -> a ^ "/" ^ cmd
        in
        Some cmd
      | _ -> None


end



module Result = struct
  type t = {
    bench: Benchmark.sexp_t;
    context_id: string;
    execs: Execution.t list;
    size: int option [@default None];
    size_code: int option [@default None];
    size_data: int option [@default None];
    check: bool option [@default None];
  } [@@deriving sexp]

  let make ~bench ?(context_id="") ?(absolute="") ~execs () =
    let size, size_code, size_data =
      let size file =
        match Util.Cmd.lines_of_cmd ("size -Bd "^file) with
        | Unix.WEXITED 0, [_; szs] ->
            (match Re.Pcre.split ~rex:Re.(compile (rep1 (set " \t\n"))) szs with
             | ""::text::data::bss::total::_
             | text::data::bss::total::_ ->
                 Some (int_of_string total),
                 Some (int_of_string text),
                 Some (int_of_string data + int_of_string bss)
             | _ -> None, None, None)
        | _ -> None, None, None
      in
      match Benchmark.get_binary ~absolute bench with
      | None -> None, None, None
      | Some file -> size file
    in
    let check = None in
    { bench = Benchmark.export bench; context_id; execs; size; size_code; size_data; check }

  let strip chan t = match chan with
    | `Stdout ->
        { t with execs = List.map (Execution.strip `Stdout) t.execs }
    | `Stderr ->
        { t with execs = List.map (Execution.strip `Stderr) t.execs }

  let load_conv fn =
    Sexplib.Sexp.load_sexp_conv fn t_of_sexp

  let load_conv_exn fn =
    Util.File.sexp_of_file_exn fn t_of_sexp

  let save_hum fn s=
    sexp_of_t s |> Sexplib.Sexp.save_hum fn

  let output_hum oc s =
    sexp_of_t s |> Sexplib.Sexp.output_hum oc

  let save_output fn s =
    let oc = open_out fn in
    (match s.execs with
     | (`Ok exec) :: _ -> Printf.fprintf oc "%s" exec.Execution.stdout
     | `Timeout :: _ -> Printf.fprintf oc "Timeout"
     | (`Error str) :: _ -> Printf.fprintf oc "Error :\n%s" str
     | _ -> Printf.fprintf oc "No Execution");
    close_out oc

end

module Summary = struct
  module Aggr = struct
    type t = {
      success: bool [@default true];
      mean: float;
      stddev: float;
      mini: float;
      maxi: float;
      runs: int [@default 1];
    } [@@deriving sexp]

    let create ~success ~mean ~stddev ~mini ~maxi ~runs =
      { success; mean; stddev; mini; maxi; runs }

    let compare t1 t2 = Pervasives.compare t1.mean t2.mean
    let min t1 t2 = if t1.mean <= t2.mean then t1 else t2
    let max t1 t2 = if t1.mean >= t2.mean then t1 else t2
    let of_measures ~success m =
      let measures_float = List.map Measure.to_float m in
      let mean, variance = Statistics.mean_variance measures_float in
      let maxi, mini = List.fold_left
          (fun (ma, mi) v -> Pervasives.(max v ma, min v mi))
          (neg_infinity, infinity) measures_float in
      { success; mean; mini; maxi;
        stddev = sqrt variance;
        runs = List.length m }

    let normalize t =
      if t.mean = 0. then t else
        let m = t.mean in
        { t with
          mean=1.;
          stddev = t.stddev /. m;
          mini = t.mini /. m;
          maxi = t.maxi /. m;
          runs = t.runs }

    (* t1 / t2 *)
    let normalize2 t1 t2 =
      if t2.mean = 0. then t1 else
        { t1 with
          mean = t1.mean /. t2.mean;
          stddev = t1.stddev /. t2.mean;
          mini = t1.mini /. t2.mean;
          maxi = t1.maxi /. t2.mean;
          runs = t1.runs;
        }

    let constant v =
      { success = true; mean = v; stddev = 0.; mini = v; maxi = v; runs = 1 }

  end

  type t = {
    success: bool [@default true];
    name: string;
    context_id: string;
    data: Aggr.t TMap.t;
    error: (string * string) option [@default None];
  } [@@deriving sexp]

  let of_result r =
    let open Execution in
    let open Result in
    (* TODO better check for errors, as in the original version *)
    let success, error = true, None in
    let data = List.fold_left
        (fun a e -> match e with | `Ok e -> e.data::a | _ -> a)
        [] r.execs in
    let data = data
               |> TMap.lmerge
               |> TMap.map @@ Aggr.of_measures ~success in
    let data = match r.Result.size with
      | None -> data
      | Some size ->
          TMap.add Topic.(Topic(Size.Full,Size)) (Aggr.constant (float size)) data
    in
    let data = match r.Result.size_code with
      | None -> data
      | Some size ->
          TMap.add Topic.(Topic(Size.Code,Size)) (Aggr.constant (float size)) data
    in
    let data = match r.Result.size_data with
      | None -> data
      | Some size ->
          TMap.add Topic.(Topic(Size.Data,Size)) (Aggr.constant (float size)) data
    in
    { success;
      name = r.bench.name;
      context_id = r.Result.context_id;
      data;
      error;
    }

  let get_mean s topic =
    (TMap.find (Topic.of_string topic) s.data).mean

  let normalize s =
    { s with data = TMap.map Aggr.normalize s.data }

  let normalize2 s1 s2 =
    { s1 with data = TMap.mapi (fun k v ->
         let v2 = TMap.find k s2.data in
         Aggr.normalize2 v v2) s1.data
    }

  let load_conv fn =
    Sexplib.Sexp.load_sexp_conv fn t_of_sexp

  let load_conv_exn fn =
    Util.File.sexp_of_file_exn fn t_of_sexp

  let save_hum fn s=
    sexp_of_t s |> Sexplib.Sexp.save_hum fn

  let output_hum oc s =
    sexp_of_t s |> Sexplib.Sexp.output_hum oc

  let fold_dir f acc dn =
    let open Unix in
    Util.FS.fold
      (fun acc fn -> match (stat fn).st_kind with
         | S_REG when Filename.check_suffix fn ".summary" -> f acc fn
         | _ -> acc
      )
      acc dn

end

module Process = struct

  type speed_characterization = {
    max_duration: int64;
    probability: float;
    confidence: float;
  }

  let min_duration = 4.e9

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

  let run ~nb_iter (f : unit -> Execution.t) =
    let rec loop i acc =
      if i > nb_iter then acc
        else loop (succ i) (f () :: acc)
    in
    loop 1 []

  let split_stderr_gc_perf stderr_lines =
    List.partition (fun line ->
      try
        ignore @@ String.index line ':';
        true
      with Not_found ->
        false
    ) stderr_lines

  let data_of_gc_stats lines =
    let data =
      List.fold_left
        (fun acc s ->
           try
             let i = String.index s ':' in
             let gc = Topic.Gc.of_string_exn @@ String.sub s 0 i in
             let v = Int64.of_string @@ String.sub s (i+2) (String.length s - i - 2) in
             Topic.(Topic (gc, Gc), Measure.of_int64 v) :: acc
           with _ ->
             acc)
        [] lines
    in
    let data = (* Make promoted_words a ratio of minor_words *)
      try
        let minor =
          Measure.to_int64 (List.assoc Topic.(Topic (Gc.Minor_words, Gc)) data)
        in
        List.map (fun (topic, measure) -> topic, match topic with
            | Topic.Topic (gc, Topic.Gc) when gc = Topic.Gc.Promoted_words ->
                let prom = Measure.to_int64 measure in
                if prom = Int64.zero then Measure.of_float 0. else
                  Measure.of_float (Int64.to_float prom /. Int64.to_float minor)
            | _ -> measure)
          data
      with Not_found -> data
    in
    data

  let data_of_perf_stats lines =
    let rex = Re.(str "," |> compile) in
    let lines = List.map (Re.Pcre.split ~rex) lines in
    List.fold_left
      (fun acc l -> match l with
         | v :: "" :: event :: _ ->
           begin try
             let perf = Topic.Perf.of_string_exn event in
             Topic.(Topic (perf, Perf), Measure.of_string v) :: acc
           with _ ->
             acc
           end
         | l ->
           Printf.eprintf
             "Ignoring perf result line [%s] %s\n" (String.concat "," l) (List.nth l 2);
           acc
      ) [] lines

  let data_of_build lines =
    let rex_timings = Re.(Posix.re ".?( *)([0-9]\.[0-9]*)s.*" |> compile) in
    let lines =
      List.fold_left
        (fun acc line ->
           try
             let groups = Re.exec rex_timings line in
             if String.length(Re.Group.get groups 1) < 2 then
               Re.Group.get groups 2 :: acc
             else
               acc
           with Not_found ->
             acc)
        []
        lines
    in
    let times = List.map float_of_string lines in
    let time = List.fold_left (+.) 0.0 times in
    Topic.(Topic(Topic.Time.Compile, Time), Measure.of_float time) :: []

end

module Perf_wrapper = struct
  include Process

  let bench_build ?env ?timeout ~opamroot ~switch ~inlining_args bench  =
    let cmd = Benchmark.cmd_build ~opamroot ~switch ~inlining_args bench in
    let env = match env with
      | None -> [|"LANG=C"|]
      | Some env -> Array.of_list @@ "LANG=C"::env in
    let cmd_string = String.concat " " cmd in
    Printf.eprintf "building : %s\n%!" cmd_string;
    let p_stdout, p_stdin, p_stderr = Unix.open_process_full cmd_string env in
    try
      let stdout_lines = Util.File.lines_of_ic p_stdout in
      let stderr_lines = Util.File.lines_of_ic p_stderr in
      let build_topics = data_of_build stdout_lines in
      (* Setup an alarm that will make Unix.close_process_full raise
         EINTR if its process is not terminated by then *)
      let (_:int) = match timeout with None -> 0 | Some t -> Unix.alarm t in
      Sys.(set_signal sigalrm (Signal_handle (fun _ -> ())));
      let _ =  Unix.close_process_full (p_stdout, p_stdin, p_stderr) in
      let data = TMap.empty in
      let data = List.fold_left (fun a (k, v) -> TMap.add k v a) data build_topics in
      `Ok data
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> `Timeout
    | exn ->
        ignore @@ Unix.close_process_full (p_stdout, p_stdin, p_stderr);
        Execution.error exn

  let run_once ?env ?timeout ~inlining_args ~opamroot ~switch bench evts =
    let data = bench_build ?env ?timeout ~opamroot ~switch ~inlining_args bench in
    match data with
    | `Timeout -> `Timeout
    | `Error exn -> `Error exn
    | `Ok data ->
    let evts = Topic.PerfSet.elements evts in
    let perf_cmdline = ["perf"; "stat"; "-x,"; ] in
    let perf_cmdline = match evts with
      | [] -> perf_cmdline
      | _ -> perf_cmdline @ ["-e"; String.concat "," @@ List.map Topic.Perf.to_string evts] in
    let cmd = perf_cmdline @ (Benchmark.cmd_exec bench) in
    let env = match env with
      | None -> [|"LANG=C"|]
      | Some env -> Array.of_list @@ "LANG=C"::env in
    let cmd_string = String.concat " " cmd in
    let time_start = Oclock.(gettime monotonic) in
    Printf.eprintf "exec : %s\n%!" cmd_string;
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
      let gc_lines, perf_lines = split_stderr_gc_perf stderr_lines in
      let gc_topics = data_of_gc_stats gc_lines in
      let perf_topics = data_of_perf_stats perf_lines in
      let time_topics = [Topic.(Topic (Time.Real, Time),
                                `Int Int64.(rem time_end time_start))] in
      let data = List.fold_left (fun a (k, v) -> TMap.add k v a) data gc_topics in
      let data = List.fold_left (fun a (k, v) -> TMap.add k v a) data perf_topics in
      let data = List.fold_left (fun a (k, v) -> TMap.add k v a) data time_topics in

      `Ok Execution.{
          process_status;
          stdout=stdout_string;
          stderr=""; (* Perf writes its result on stderr... *)
          data;
          checked=None;
        }
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> `Timeout
    | exn ->
        ignore @@ Unix.close_process_full (p_stdout, p_stdin, p_stderr);
        Execution.error exn

  let run ?env ?timeout ~inlining_args ~return_value ~opamroot ~switch
        (bench : Benchmark.t) evts =
    (* if evts = SSet.empty then [] *)
    (* else *)
    run ~nb_iter:bench.iter
      (fun () -> run_once ?env ?timeout ~inlining_args ~opamroot ~switch bench evts)
end


module Runner = struct
  type execs = {
    time: Topic.TimeSet.t;
    gc: Topic.GcSet.t;
    perf: Topic.PerfSet.t;
  }

  let make_tmp_file suffix =
    let name = Filename.temp_file "" suffix in
    name, Unix.openfile name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o644

  let run_command ?(discard_stdout=false) prog args =
    let cmd = Array.fold_left (fun acc arg -> acc ^ arg ^ " ") "" args in
    let fd_stdout =
      if discard_stdout then snd (make_tmp_file ".out")
      else Unix.stdout in
    let pid = Unix.create_process prog args Unix.stdin fd_stdout Unix.stderr in
    let rpid, status = Unix.waitpid [] pid in
    assert(rpid = pid);
    if discard_stdout then Unix.close fd_stdout;
    match status with
    | Unix.WEXITED 0 -> true
    | Unix.WEXITED n -> (Printf.eprintf "Command return code %i:\n  %s\n%!" n cmd; false)
    | Unix.WSIGNALED n ->
        (Printf.eprintf "Command killed with signal %i:\n  %s\n%!" n cmd; false)
    | Unix.WSTOPPED _n -> false

  let set_ocamlrunparam env p =
    let was_matched = ref false in
    let re = Re.Posix.re "OCAMLRUNPARAM=.*" |> Re.compile in
    let env =
      List.map (fun s ->
        try
          ignore @@ Re.exec re s;
          was_matched := true;
          s ^ "," ^ p
        with Not_found ->
          s)
        env
    in
    if !was_matched then
      env
    else
      ("OCAMLRUNPARAM=" ^ p) :: env


  let run_exn ~opamroot ~switch
        ~inlining_args b =

    (* We run benchmarks in a temporary directory that we create now. *)
    let cwd = Unix.getcwd () in
    let temp_dir = Filename.temp_file "macrorun" "" in
    Unix.unlink temp_dir;
    Unix.(try
       mkdir temp_dir 0o755
     with Unix_error (EEXIST, _, _) -> ());
    Unix.chdir temp_dir;

    (* Now copy the dependencies *)
    List.iter (
      fun file ->
        let cmd = "cp -r " ^ cwd ^ "/" ^ file ^ " ." in
        let p_stdout, p_stdin, p_stderr =
          Unix.open_process_full cmd [||]
        in
        let stderr = Util.File.lines_of_ic p_stderr in
        List.iter print_endline stderr
    ) (Benchmark.dependency_files b);

    let env = match Benchmark.env b with
      | None -> set_ocamlrunparam (Array.to_list @@ Unix.environment ()) "v=0x400"
      | Some e -> set_ocamlrunparam e "v=0x400"
    in

    (* Transform individial topics into a list of executions *)
    let execs =
      let open Topic in
      TSet.fold
        (fun t a -> match t with
           | Topic (t, Time) -> { a with time = TimeSet.add t a.time }
           | Topic (t, Gc) -> { a with gc = GcSet.add t a.gc }
           | Topic (t, Perf) -> { a with perf= PerfSet.add t a.perf }
           | Topic (t, Size) -> a
        )
        (Benchmark.topics b)
        { time = TimeSet.empty;
          gc = GcSet.empty;
          perf = PerfSet.empty;
        }
    in

    let run_execs { time; gc; perf; } b =
      let return_value = Benchmark.return_value b in
        Perf_wrapper.(run ~inlining_args ~switch ~opamroot ~env ~return_value b perf )
    in

    let execs = run_execs execs b in
    Unix.chdir cwd;



    let switch = match switch with
      | Some switch -> switch
      | None -> Util.Opam.cur_switch ~opamroot in
    let r =
      Result.make ~context_id:switch ~bench:b ~absolute:temp_dir ~execs ()
    in
    (* Cleanup temporary directory *)
    Util.FS.rm_r [temp_dir];
    r
end
