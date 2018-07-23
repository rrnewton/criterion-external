# criterion-external [![Build](https://img.shields.io/travis/rrnewton/criterion-external.svg)](https://travis-ci.org/rrnewton/criterion-external)

A small wrapper around [criterion](https://github.com/bos/criterion) to benchmark executables.


## Usage


* criterion-external

```

% criterion-external -h

criterion-external usage:

  criterion-external <cmdname> <cmdArg>^N -- <criterionArgs>

 Benchmark an external program that takes a repetition count.
 Above, <cmdname> expects N+1 args, with the first argument being
 an iteration count.  Each measurement is the complete time of the
 spawned sub-process.

```


* criterion-interactive

```

% criterion-interactive -h

criterion-interactive usage:
  criterion-interactive <cmdname> <cmdArg> ... -- <criterionArg> ...

 Benchmark an external program that supports an interactive protocol
 on stdin.  Here, <cmdname> expects N args provided on the command line.
 After <cmdname> has started, it blocks on its stdin, waiting for input
 that begins a protocol:

  < READY
  > START_BENCH <repetition-count>
  < END_BENCH
  ...
  > EXIT

 After the child process indicates that it is ready, it goes into a loop
 running batches.  On each batch, it runs the benchmark for the given number
 of repititions, responding with the END_BENCH tag on the child processes' stdout.

 The benchmark harness times the interval between sending the start
 message and receiving the end message.  The benchmark harness issues
 many such 'rounds' of timing, before finally closing the subprocess
 with the EXIT message.


```
