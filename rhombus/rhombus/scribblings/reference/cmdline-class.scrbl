#lang rhombus/scribble/manual
@(import:
    "common.rhm" open:
      except: def
    "nonterminal.rhm" open
    meta_label:
      rhombus/cmdline
      rhombus/cmdline!class open)

@title(~tag: "cmdline-class"){Command Line Parsing Classes}

@docmodule(rhombus/cmdline!class)

@doc(
  class Parser():
    constructor (
      ~flags: flags :: List.of(Content),
      ~add_builtin: add_builtin = #true,
      ~args: args :: Handler,
      ~init: init :: Map = {},
      ~who: who :: maybe(error.Who) = #false
    )
  method (p :: Parser).parse(
    ~program: program :: String || Path = cmdline.current_program(),
    ~line: line :: List.of(String) = cmdline.current_command_line()
  ) :: Map
  method (p :: Parser).print_help(
    ~program: program :: String || Path = cmdline.current_program()
  ) :: Void
  property (p :: Parser).flags :: List.of(Content)
  property (p :: Parser).args :: Handler
  property (p :: Parser).init :: Map
){

 Represents a parser as normally created by @rhombus(cmdline.parser).

}

@doc(
  class Handler(init :: Map,
                args :: List.of(String),
                repeat_last :: Boolean,
                handler :: (~any) -> Map):
    nonfinal
){

 Represents a handler, either for a flag or for arguments after all
 flags. A handler that represents a flag is more specifically an instance
 of @rhombus(Flag).

}

@doc(
  interface Content
  class Flag(flag_strs :: List.of(String),
             help :: String):
    nonfinal
    extends Handler
    implements Content
  class FinalFlag():
    extends Flag
  class Multi(choices :: List.of(Flag || Multi)):
    implements Content
  class OnceEach(choices :: List.of(Flag || OnceEach)):
    implements Content
  class OnceAny(choices :: List.of(Flag || OnceAny)):
    implements Content
  class Text():
    nonfinal
    implements Content
  class EndText():
    extends Text
){

 Flags, flag sets, and help text that can be part of a command-line
 parser constructed with @rhombus(cmdline.parser) or @rhombus(Parser).

}


@doc(
  fun make_builtin_flags(help :: Map -> Map) :~ List.of(Content)
){

 Returns a list of @rhombus(Content) corresponding to flags that are
 normally added to a command-line parser by default, unless
 @rhombus(~no_builtin) is used with @rhombus(cmdline.parser) or
 @rhombus(~add_builtin) is supplied as @rhombus(#false) to
 @rhombus(Parser).

}
