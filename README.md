# Bench_tool

Based on *operf-macro*

## Introduction

This is a tool made to extract various metrics (compile time, number of cycles, number of allocations, ...)
from an ocaml benchmark compiled with a set of optimiser parameters for Flambda. These metrics can then be 
used to evaluate a *objective function*, which in turn will be used to find the optimal parameters according
to this objective function. 

A run of the benchmark includes the compilation of the benchmarks with a set of inlining parameters passed
as argument to `bench_tool` (and then to the ocaml compiler), as well as the execution of the benchmark
itself.

## Installation

This tool works with **OPAM 2**.

After installing the dependencies, you can use `make` to make and install this tool.

## Basic usage

### Running benchmarks

```
bench_tool run bench_file [-o output_file]? [-s opam_switch]? [--opamroot opamroot]? 
                --inline=xx ... --unbox_closures=xxx
```

This will run the benchmark defined by `bench_file` installed in the OPAM switch you are
currently in, or `opam_switch` if passed as parameter. The ocaml compiler used is the one
installed in this switch. The opam used will be the one in `~/.opam` or `opamroot` if passed. 
If an output file is specified, a summary of the metrics measured will be
saved in it, otherwise it will be printed to stdout.

You can pass a set of arguments to flambda. The list of parameters is:

--inline, --inline max unroll, --inline-alloc-cost, --inline-branch-cost, --inline-branch-factor,--inline-call-cost, --inline-indirect-cost, --inline-lifting-benefit, --inline-max-depth,--inline-max-speculation_depth, --inline-prim-cost, --inline-toplevel, --Oclassic, --remove-unused-arguments, --round-2-multiplier=VAL (absent=1.), --round-3-multiplier=VAL (absent=1.), --unbox_closures, --unbox_closures_factor=VAL

Every benchmark will be compiled in `O3`. `round-3-multiplier` and `round-2-multiplier` each specifies how much we scale
the inlining parameters between (reps) round 2 and 3 (resp 1 and 2).

### Evaluating an objective function

To evaluate an objective function use:

```
bench_tool objective [--function objective_function]? 
          --run=result_current_run --status-quo=result_status_quo
```

The status_quo is a first run which will used as a witness of the "current" result of the benchmark. It used
to convert every metrics of over runs to be relative to this one inside the objective function.

`objective_function` is a file containing the description of an objective function (see below). 
If not provided, the objective function is the number of cycles.

### Listing all topics

`bench_tool list-topics` will list all the available topics.

### Exemple of use

```
# bench_tool run kb_bench -o status_quo

# bench_tool run kb_bench -o run --inline 45 --call-cost 789

# bench_tool objective --status_quo=status_quo --run=run

```


## Advanced usage

### Topics:

By default we are measuring these topics: time_real, time_compile, size, size_code, size_data, minor_words, major_words, promoted_words, top_heap_words, minor_collections, major_collections, compactions, heap_words, heap_chunks, cycles, instructions, task_clock

### Objective functions

Objective functions are of the form 

```
(metric1_current_run / metric1_status_quo) ^ weight1 * ... * (metricn_current_run / metricn_status_quo) ^ weightn
```

You can define a metric in a file containing a sexp expression of the following type `t`:

```ocaml
type topic = string
type weight = float
type t = (topic * weight) list
```



### Writing benchmarks

You can specify a benchmark using a file containing an sexp of type `yt`:

```ocaml
type custom = {
  build : string list;
  exec : string list;
  dependency : string list [@default []];
  return_value : int [@default 1];
  env : string list option [@default None];
  topics: TSet.t [@default TSet.empty];
} 

type cmd =
  | Opam
  | Custom of custom

type t = {
  name : string;
  iter : int [@default 1];
  cmd : cmd;
}

```

- `Opam` which means that the benchmark is a bench defined as explained in *operf-macro* and
which is installed on opam. The name of the opam benchmark used is `name_of_the_benchmark`.
- `Custom (...)` represents a custom benchmark.
Dependency is the list of all files needed to run and build the benchmark, including source files.


#### Exemple:
```
(name kb)
(iter 4)
(cmd (Custom (
(dependency (kb.ml))
(build ("ocamlopt kb.ml -o kb"))
(exec (./kb))
))))
```
