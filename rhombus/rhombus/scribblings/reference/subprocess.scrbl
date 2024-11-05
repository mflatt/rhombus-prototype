#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/subprocess open)

@title(~style: #'toc, ~tag: "subprocess"){Subprocesses}

@docmodule(rhombus/subprocess)

@doc(
  class Subprocess():
    implements Closeable
    constructor (
      exe :: PathString,
      ~in: in :: Port.Input || Subprocess.Pipe = #'pipe,
      ~out: out :: Port.Output || Subprocess.Pipe = #'pipe,
      ~err: err :: Port.Output || Subprocess.ErrorPipe = #'pipe,
      ~group: group :: Subprocess.Group || Subprocess.NewGroup
                = (if current_subprocess_group_new() | #'new | #'same),
      arg :: PathString || ReadableString,
      ...
    )
  fun run(
    exe :: PathString,
    ~in: in :: Port.Input || Subprocess.Pipe = Port.Input.current(),
    ~out: out :: Port.Output || Subprocess.Pipe = Port.Output.current(),
    ~err: err :: Port.Output || Subprocess.ErrorPipe = Port.Output.current(),
    ~group: group :: Subprocess.Group || Subprocess.NewGroup
              = (if current_subprocess_group_new() | #'new | #'same),
    arg :: PathString || ReadableString,
    ...
  ) :: Subprocess
){

 Runs another program as a subprocess in the host operating system. The
 executable to run in the process is named by @rhombus(exe), and it
 receives the @rhombus(arg)s.

 The new process's input, output, and error output can be set to one end
 of a pipe using @rhombus(#'pipe) for @rhombus(in), @rhombus(out), and
 @rhombus(err); the other end of the pipe is then accessible using
 @rhombus(Subprocess.to_in), @rhombus(Subprocess.from_out), or
 @rhombus(Subprocess.from_err), respectively. Otherwise, the new
 process's input and output use the given ports. In the case of
 @rhombus(err), @rhombus(#'out) indicates that error output should use
 the same pipe or port as non-error output.

 The @rhombus(Subprocess, ~class) constructor and @rhombus(run) function
 are almost the same, but with different defaults for ports: the
 @rhombus(Subprocess, ~class) constructor uses @rhombus(#'pipe), while
 @rhombus(run) uses the current input and output ports of the current
 process.

 If @rhombus(group) is @rhombus(#'same), the process group of the
 current process is used for the new subprocess. If @rhombus(group) is
 @rhombus(#'new), the new subprocess is created in a fresh process group;
 the resulting @rhombus(Subprocess, ~annot) object then satisfies
 @rhombus(Subprocess.NewGroup, ~annot). If @rhombus(group) is a
 @rhombus(Subprocess.NewGroup, ~annot), then the new subprocess is in the
 same group as that previously created subprocess.

 When pipes are created for a subprocess, the local end of the pipe must
 be closed explicitly, perhaps using @rhombus(Subprocess.close). See also
 @rhombus(Closeable.let, ~defn).

}


@doc(
  fun run_shell(
    command :: String,
    ~in: in :: Port.Input || Subprocess.Pipe = Port.Input.current(),
    ~out: out :: Port.Output || Subprocess.Pipe = Port.Output.current(),
    ~err: err :: Port.Output || Subprocess.ErrorPipe = Port.Output.current(),
    ~group: group :: Subprocess.Group || Subprocess.NewGroup
              = (if current_subprocess_group_new() | #'new | #'same)
  ) :: Subprocess
  fun shell(
    command :: String,
    ~in: in :: Port.Input || Subprocess.Pipe = Port.Input.current(),
    ~out: out :: Port.Output || Subprocess.Pipe = Port.Output.current(),
    ~err: err :: Port.Output || Subprocess.ErrorPipe = Port.Output.current(),
    ~group: group :: Subprocess.Group || Subprocess.NewGroup
              = (if current_subprocess_group_new() | #'new | #'same)
  ) :: Boolean
){

 The @rhombus(run_shell) function runs a shell as a subprocess and
 provides @rhombus(command) to the shell. On Unix and Mac OS variants,
 @rhombus("/bin/sh") is run as the shell. On Windows, @rhombus("cmd.com")
 or @rhombus("command.exe") is used.

}

@doc(
  property (subp :: Subprocess).to_in :: Port.Output
  property (subp :: Subprocess).from_out :: Port.Input
  property (subp :: Subprocess).from_err :: Port.Input
  property (subp :: Subprocess).maybe_to_in :: maybe(Port.Output)
  property (subp :: Subprocess).maybe_from_out :: maybe(Port.Input)
  property (subp :: Subprocess).maybe_from_err :: maybe(Port.Input)
){

 Accesses pipe ends created for subprocess.

 Accessing the @rhombus(Subprocess.to_in),
 @rhombus(Subprocess.from_out), or @rhombus(Subprocess.from_err),
 property raises an exception if the subprocess does not have a pipe for
 the corresponding subprocess stream. Accessing the
 @rhombus(Subprocess.maybe_to_in), @rhombus(Subprocess.maybe_from_out),
 or @rhombus(Subprocess.maybe_from_err) property either produces the same
 result as @rhombus(Subprocess.to_in), @rhombus(Subprocess.from_out), or
 @rhombus(Subprocess.from_err), or it returns @rhombus(#false).

}


@doc(
  property (subp :: Subprocess).close() :: Void
){

 Closes any pipes created for the subprocess that are still open.

}


@doc(
  property (subp :: Subprocess).pid :: NonnegInt
){

 Returns the operating system's process ID for a subprocess.

}


@doc(
  method (subp :: Subprocess).wait() :: Int
  method (subp :: Subprocess).wait_ok() :: Boolean
  method (subp :: Subprocess).poll() :: maybe(Int)
){

 Waits for a subprocess to complete or checks whether it has completed.

 The @rhombus(Subprocess.wait) method waits for the subprocess and it
 returns its exit code. An exit code of @rhombus(0) typically indicates
 success, and @rhombus(Subprocess.wait_ok(subp)) is equivalent to
 @rhombus(Subprocess.wait(subp) == 0).

 The @rhombus(Subprocess.poll) method immediately returns
 @rhombus(#false) if the subprocess has not completed, or it returns the
 same result as @rhombus(Subprocess.wait).

}


@doc(
  method (subp :: Subprocess).interrupt() :: Void
  method (subp :: Subprocess).kill() :: Void
){

 Send a process an interrupt signal or a kill signal, respectively. The
 latter cannot be ignored by a process.

}

@doc(
  Parameter.def current_subprocess_group_new :: Any.to_boolean
){

 A @tech{context parameter} that determines the default group for a
 subprocess. See @rhombus(Subprocess), @rhombus(run), and other
 subprocess-creation functions.

}


@doc(
  Parameter.def current_subprocess_custodian_mode
    :: False || matching(#'kill) || matching(#'interrupt)
){

 A @tech{context parameter} that determines whether and how subprocesses
 are managed by a custodian.

}

@doc(
  enum Subprocess.Pipe:
    pipe
  enum Subprocess.ErrorPipe:
    ~is_a Subprocess.Pipe
    out
  enum Subprocess.Group:
    same
    new
  annot.macro 'Subprocess.NewGroup'
){

 Port and group modes for use with @rhombus(Subprocess, ~class), @rhombus(run),
 @rhombus(shell), and @rhombus(run_shell).

}
