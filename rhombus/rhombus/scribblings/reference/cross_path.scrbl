#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "cross-path"){Cross-Patform Paths}

A @deftech{cross-platform path} value represents a filesystem path
combined with @rhombus(#'unix) or @rhombus(#'windows) to indicate the
filesystem's path convention. A cross-platform path whose convention
matches the current host system is also a @tech{path}.

@dispatch_table(
  "cross_path"
  Path
  cross_path.bytes()
  cross_path.string()
  cross_path.add(part, ...)
  cross_path.split()
  cross_path.name()
  cross_path.parent()
  cross_path.directory_only(...)
  cross_path.to_directory_path(...)
  cross_path.to_absolute_path(...)
  cross_path.suffix(...)
  cross_path.replace_suffix(...)
  cross_path.add_suffix(...)
  cross_path.cleanse(...)
  cross_path.simplify(...)
  cross_path.normal_case(...)
  cross_path.as_relative_to(...)
)

@doc(
  enum CrossPath.Convention:
    unix
    windows
  annot.macro 'CrossPath'
  annot.macro 'CrossPath.Absolute'
  annot.macro 'CrossPath.Relative'
  annot.macro 'CrossPath.DriveRelative'
  annot.macro 'CrossPath.Directory'
  annot.macro 'CrossPath.Element'
){

 The @rhombus(CrossPath.Convention) enumeration represents the two
 supported path conventions. The other annotations are analogous to
 @rhombus(Path), @rhombus(Path.Absolute), @rhombus(Path.Relative),
 @rhombus(Path.DriveRelative), @rhombus(Path.Directory),
 @rhombus(Path.Element).

 Every @rhombus(Path) is also a @rhombus(CrossPath), and
 @rhombus(CrossPath.convention) for a @rhombus(Path) will produce the
 same symbol as @rhombus(CrossPath.Convention.current()). Operations on
 @rhombus(CrossPath)s also work on @rhombus(Path), but they typically do
 not accept strings, instead requiring @rhombus(Path) or
 @rhombus(CrossPath) values that have an associated convention.

}


@doc(
  fun CrossPath(
    cross_path :: Bytes || Path.Dot || CrossPath,
    convention :: CrossPath.Convention
      = CrossPath.Convention.current()
  ) :: CrossPath
){

 Constructs a @tech{cross-platform path} given a byte string and path
 convention. When a cross-platform path is provided as @rhombus(cross_path),
 then the result is @rhombus(cross_path).

@examples(
  def p = CrossPath(#"/home/rhombus/shape.txt", #'unix)
  p.string()
  p.convention()
)

}

@doc(
  ~nonterminal:
    bytes_bind: def bind ~defn
    convention_bind: def bind ~defn
  bind.macro 'CrossPath($bytes_bind, $convention_bind)'
){

 Matches a cross-platform path where the byte-string form of the path
 matches @rhombus(bytes_bind) and the convention matches
 @rhombus(convention_bind).

@examples(
  def CrossPath(bstr, _) = Path("/home/rhombus/shape.txt")
  bstr
)

}

@doc(
  fun CrossPath.bytes(cross_path :: CrossPath) :: Bytes
){

 Converts a cross-platform path to a byte-string form, which does not
 lose any information about the path.

@examples(
  def p = Path("/home/rhombus/shape.txt")
  CrossPath.bytes(p)
)

}


@doc(
  fun Path.string(cross_path :: CrossPath) :: String
){

 Converts a path to a human-readable form, but the conversion may lose
 information if the path cannot be expressed using a string (e.g., due to
 a byte-string form that is not a UTF-8 encoding).

@examples(
  def p = Path(#"/home/rhombus/shape.txt")
  CrossPath.string(p)
)

}


@doc(
  fun CrossPath.add(cross_path :: CrossPath || Path.dot,
                    cross_part :: CrossPath || Path.dot,
                    ...) :: CrossPath
){

 Like @rhombus(Path.add), but for cross paths.

 The @rhombus(cross_path) and @rhombus(cross_part)s that are
 @rhombus(CrossPath)s must all have the same convention, otherwise the
 @rhombus(Exn.Fail.Annot, ~annot) exception is raised.

@examples(
  def p = CrossPath(#"C:/Users", #'windows)
  p.add(CrossPath(#"Rhombus", #'windows)).string()
)

}


@doc(
  fun CrossPath.split(path :: CrossPath)
    :: List.of(CrossPath.Element || Path.Dot)
){

 Like @rhombus(Path.split), but for cross-platform paths.

@examples(
  def p = Path("/home/rhombus/shape.txt")
  Path.split(p).map(to_string)
)

}

@doc(
  fun CrossPath.name(cross_path :: CrossPath) :: CrossPath.Element || Path.Dot
  fun CrossPath.parent(cross_path :: CrossPath)
    :: (CrossPath.Directory || matching(#'relative) || False)
  fun CrossPath.directory_only(cross_path :: CrossPath) :: CrossPath.Directory
  fun CrossPath.to_directory_path(cross_path :: CrossPath) :: CrossPath.Directory
){

 Like to @rhombus(Path.name), @rhombus(Path.parent),
 @rhombus(Path.directory_only), and @rhombus(Path.to_directory_path), but
 for cross-platform paths.

@examples(
  def p = Path("/home/rhombus/shape.txt")
  CrossPath.name(p).string()
  CrossPath.parent(p).string()
  CrossPath.directory_only(p).string()
  CrossPath.directory_only(CrossPath.parent(p)).string()
  CrossPath.to_directory_path(p).string()
)

}

@doc(
  fun CrossPath.to_absolute_path(
    cross_path :: CrossPathString,
    ~relative_to: crosss_base :: CrossPath.Absolute
  ) :: CrossPath
){

 Like to @rhombus(Path.to_absolute_path), but for cross-platform paths.
 The @rhombus(~relative_to) argument is mandatory.

}


@doc(
  fun CrossPath.suffix(cross_path :: CrossPath) :: maybe(Bytes)
  fun CrossPath.replace_suffix(
    cross_path :: CrossPath,
    suffix :: Bytes || ReadableString
  ) :: CrossPath
  fun CrossPath.add_suffix(
    cross_path :: PathString,
    suffix :: Bytes || ReadableString,
    ~sep: sep :: Bytes || ReadableString = "_"
  ) :: CrossPath                                           
){

 Like to @rhombus(Path.suffix), @rhombus(Path.replace_suffix), and
 @rhombus(Path.add_suffix), but for cross-platform paths.

@examples(
  def p = Path("/home/rhombus/shape.txt")
  CrossPath.suffix(p)
  CrossPath.replace_suffix(p, ".rhm").string()
  CrossPath.add_suffix(p, ".rhm").string()
)

}

@doc(
  fun CrossPath.cleanse(cross_path :: CrossPathS) :: CrossPath
  fun CrossPath.simplify(cross_path :: CrossPath) :: CrossPath
  fun CrossPath.normal_case(cross_path :: CrossPath) :: CrossPath
){

 Like @rhombus(Path.cleanse), @rhombus(Path.simplify), and
 @rhombus(Path.normal_case), but for class-platform paths. Windows.

@examples(
  Path.cleanse(CrossPath(#"a/..//b")).string()
  Path.simplify(CrossPath(#"a/..//b")).string()
)

}


@doc(
  fun CrossPath.as_relative_to(
    cross_path :: CrossPath,
    rel_to_cross_path :: CrossPath,
    ~more_than_root: more_than_root = #false,
    ~more_than_same: more_than_same = #true,
    ~normal_case: normal_case = #true
  ) :: CrossPath
){

 

@examples(
  def p = Path("/home/rhombus/shape.txt")
  p.as_relative_to("/home")
  p.as_relative_to("/home/racket")
)

}


@// ------------------------------------------------------------

@include_section("runtime-path.scrbl")
