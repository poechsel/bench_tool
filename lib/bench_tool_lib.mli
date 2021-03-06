module Sexpable : sig
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

module Util : sig
  module FS : sig
    val (/) : string -> string -> string
    val home : string
    val micro_dir : string
    val macro_dir : string
    val ls : ?preserve_order:bool -> ?prefix:bool -> ?glob:string -> string -> string list
    val iter : (string -> unit) -> string -> unit
    val fold : ('a -> string -> 'a) -> 'a -> string -> 'a
    val fold_files : ('a -> string -> 'a) -> 'a -> string -> 'a
    val rm_r : string list -> unit

    val kind_exn : string -> Unix.file_kind
    val is_file_exn : string -> bool
    val is_dir_exn : string -> bool
    val kind : string -> Unix.file_kind option
    val is_file : string -> bool option
    val is_dir : string -> bool option
  end

  module File : sig
    val string_of_ic : in_channel -> string
    val lines_of_ic : in_channel -> string list
    val string_of_file : string -> string
    val sexp_of_file_exn : string -> (Sexplib.Type.t -> 'a) -> 'a
    val lines_of_file : string -> string list
    val write_string_to_file: fn:string -> string -> unit
    val with_oc_safe : (out_channel -> 'a) -> string -> 'a
    val with_ic_safe : (in_channel -> 'a) -> string -> 'a
  end

  module Cmd : sig
    val path_of_exe : string -> string
  end

  module Opam : sig
    val exe : opamroot:string option -> string

    val (/) : string -> string -> string
    (** Alias to Filename.concat *)

    val home : string
    (** $HOME *)

    val root : string
    (** [root] is either $OPAMROOT or $HOME/.opam if unset *)

    val cur_switch : opamroot:string option -> string
    (** [cur_switch] is the name of the current OPAM switch. *)

    val switches : opamroot:string option -> string list
    (** [switches] is the list of installed OPAM switches. *)

    val switches_matching : ?opamroot:string -> string -> string list
    (** [switches_matching glob] is the list of installed OPAM
        switches that match [glob]. *)

    val share : ?opamroot:string -> string -> string
    (** [share switch] is the name of the share folder under [switch]
        (OPAM switch). *)
  end
end

module Topic : sig
  module Time : sig
    type t = Real | User | Sys | Compile
    val of_string_exn : string -> t
  end

  module Gc : sig
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

    val of_string_exn : string -> t
    val of_string : string -> t option
  end

  module Perf : sig
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

    val of_string_exn : string -> t
    val of_string : string -> t option
    val to_string : t -> string
  end

  module Size: sig
    type t =
      | Full
      | Code
      | Data

    val of_string_exn : string -> t
    val of_string : string -> t option
  end

  type _ kind =
    (** Time related *)
    | Time : Time.t kind

    (** GC related *)
    | Gc : Gc.t kind

    (** Use the perf-stat(1) command or ocaml-libperf *)
    | Perf : Perf.t kind

    (** The executable size *)
    | Size : Size.t kind

  type t =  Topic : 'a * 'a kind -> t

  val of_string : string -> t
  val to_string : t -> string
  val display_list : unit -> unit
end

module SSet : Set.S with type elt = string
module TSet : Set.S with type elt = Topic.t
module TMap : Map.S with type key = Topic.t

(*
module Benchmark : sig
  type t = {
    name : string;
    cmd : string list;
  }

  val get_exec_cmd : t -> string list
  val get_compile_cmd : t -> string list
   end*)

module BenchmarkOpam : sig
  type speed = [`Fast | `Slow | `Slower]

  type t (*= {
    name: string;
    (** Identifier for a benchmark, should be unique amongst
        benchmarks *)
    descr: string;
    (** Description of the benchmark *)
    cmd: string list;
    (** Command line to run the benchmark. The first item of the list
        is the full path of the benchmark executable, or its name if in
        PATH *)
    cmd_check: string list;
    (** Command line of the check program. It is used to check if the
        benchmark has executed correctly. *)
    file_check: (string * string) list;
    (** List of files to be checked. It is used to check if the
        benchmark has executed correctly. *)
    binary: string option;
    (** The program that will be run. It is only used to measure its size. *)
    env: string list option;
    (** Optional environment for the benchmark *)
    speed: speed;
    (** Use to characterize the execution time of a benchmark *)
    timeout: int;
    (** Maximum time per execution, in seconds. *)
    weight: float;
    (** Used to specify the relative importance of this benchmark
        compared to others (default: 1.) *)
    discard: [`Stdout | `Stderr] list;
    (** The runner will discard the output of specified channels in
        the result. *)
    topics: TSet.t;
    (** Set of quantities to measure *)
    return_value: int;
    (** The expected return value from the bench program (generally 0) *)
  }*)

  include Sexpable.S with type t := t

  val make :
    name:string ->
    ?descr:string ->
    cmd:string list ->
    ?cmd_check:string list ->
    ?file_check:(string * string) list ->
    ?binary:string ->
    ?env:string list ->
    speed:speed ->
    ?timeout:int ->
    ?weight:float ->
    ?discard:[`Stdout | `Stderr] list ->
    topics:Topic.t list ->
    ?return_value:int ->
    unit ->
    t

  (** I/O *)

  val load_conv : string -> t Sexplib.Sexp.Annotated.conv
  val load_conv_exn : string -> t

  val save_hum : string -> t -> unit
  val output_hum : out_channel -> t -> unit

  val cmd_exec : t -> string list
  val name : t -> string

  (** Filesystem *)
  val find_installed :
    ?opamroot:string ->
    ?glob:[`None | `Matching of string list | `Exclude of string list] ->
    string -> (string * (string * string)) list
  (** [find_installed ?glob switch] is the list of (benchmarks, path)
      installed in switch [switch], that match the glob expression
      [?glob] if it is defined. *)
end

module Benchmark : sig
  type sexp_t

  type t

  val load_conv : opamroot:string option -> switch:string option -> string -> t Sexplib.Sexp.Annotated.conv
  val load_conv_exn : opamroot:string option -> switch:string option -> string -> t

  val cmd_build : opamroot:string option -> switch:string option -> inlining_args:string -> t -> string list
  val cmd_exec : t -> string list

  val return_value : t -> int
  val name : t -> string
end

module Measure : sig
  type t = [ `Int of int64 | `Float of float | `Error ]
  (** Type of a measure. This is to discriminate between discrete
      events (i.e. cpu cycles), continuous events (i.e. time) and
      errors (the measurement operation failed). *)

  val of_string : string -> t
  (** [of string msr_string] is the measure resulting from the
      cast of [msr_string]. *)
end



module Execution : sig
  type exec = {
    process_status: Unix.process_status;
    stdout: string;
    stderr: string;
    data: Measure.t TMap.t;
    checked: bool option; (* obsolete *)
  }
  (** Type representing the successful execution of a benchmark. *)

  type t = [ `Ok of exec | `Timeout | `Error of string ]
  (** Type representing the execution of a benchmark. *)

  val error : exn -> t
  (** [error exn] is `Error Printexc.(to_string exn) *)

  val duration : t -> Int64.t
  (** [duration e] is the duration of [e] in nanoseconds. *)

end

module Result : sig
  type t = {
    bench: Benchmark.sexp_t;
    (** The benchmark used to produce this result *)
    context_id: string;
    (** A unique identifier for the context used to produce the
        benchmark executable: compiler used, build options of this
        compiler, etc. *)
    execs: Execution.t list;
    (** This contain the list of execution results, containing
        measurements plus additional useful information about the
        individual runs if the execution was possible. *)
    size: int option;
    (** The (stripped) size of the executable (bytes) *)
    size_code: int option;
    (** The code section size of the executable (bytes) *)
    size_data: int option;
    (** The data+bss sections size of the executable (bytes) *)
    check: bool option;
    (** Result of cmd_check or file_check *)
  }
  (** Type of a result. This can correspond to several runs of the
      same benchmark,if requested measures cannot be performed in one
      go. *)


  include Sexpable.S with type t := t

  val make :
    bench:Benchmark.t ->
    ?context_id:string ->
    ?absolute:string ->
    execs:Execution.t list -> unit ->
    t

  val strip : [`Stdout | `Stderr] -> t -> t
  (** [strip t chan] is a result where the output of the program
      executions in [chan] have been disabled. *)

  (** I/O *)

  val load_conv : string -> t Sexplib.Sexp.Annotated.conv
  val load_conv_exn : string -> t

  val save_hum : string -> t -> unit
  val output_hum : out_channel -> t -> unit
  val save_output : string -> t -> unit
end

module Summary : sig
  module Aggr : sig
    type t = { success: bool;
               mean: float;
               stddev: float;
               mini: float;
               maxi: float;
               runs: int;
             }

    include Sexpable.S with type t := t

    val create : success:bool -> mean:float -> stddev:float ->
      mini:float -> maxi:float -> runs:int -> t

    val compare : t -> t -> int
    val max : t -> t -> t
    val min : t -> t -> t

    val of_measures : success:bool -> Measure.t list -> t
    (** [of_measures weight m] is a t *)

    val normalize : t -> t
    (** [normalize a] is [a] where all the fields are divided by
        [a.mean]. *)

    val normalize2 : t -> t -> t
    (** [normalize2 a b] is [b] where all the fields are divided by
        [b.mean]. *)

    val constant : float -> t
    (** the aggr representing a constant *)
  end

  type t = {
    success: bool;
    name: string;
    context_id: string;
    data: Aggr.t TMap.t;
    error: (string * string) option;
    (** stdout, stderr of one failed run, when success = false *)
  }
  (** Content of a "summary file". *)

  include Sexpable.S with type t := t

  val of_result : Result.t -> t

  val get_mean : t -> string -> float

  val normalize : t -> t
  val normalize2 : t -> t -> t
  (** Fails with [Not_found] if the keys of the data TMap.t do not
      match. *)

  (** I/O *)

  val load_conv : string -> t Sexplib.Sexp.Annotated.conv
  val load_conv_exn : string -> t

  val save_hum : string -> t -> unit
  val output_hum : out_channel -> t -> unit

  (** Operation on directories containing .summary files. *)

  val fold_dir : ('a -> string -> 'a) -> 'a -> string -> 'a
  (** [fold_dir f acc dn] is like [Util.FS.fold f acc dn] except it
      folds only on regular files that have suffix .summary *)

end


module Runner : sig
  val run_exn : opamroot:string option ->
    switch:string option -> inlining_args:string ->
    Benchmark.t -> Result.t

end

module ObjectiveFunction : sig
  (* Objective functions are of the form topic1^a1 * ... * topicn^an *)
  type t

  val default : t

  val evaluate : t -> status_quo:Summary.t -> current_run:Summary.t -> float
  val load_conv : string -> t Sexplib.Sexp.Annotated.conv
  val load_conv_exn : string -> t
end
