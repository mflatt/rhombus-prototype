#lang scribble/rhombus/manual

@(import:
    meta_label:
      pict open
      draw)

@title(~tag: "shape"){Constructors}

@doc(
  fun blank() :~ StaticPict
  fun blank(size :: Real) :~ StaticPict
  fun blank(width :: Real, height :: Real) :~ StaticPict
){

 Creates a blank static picture.

}

@doc(
  fun rectangle(
    ~around: around :: maybe(Pict) = #false,
    ~width: width :: Real || Pict = around || 32,
    ~height: height :: Real || Pict = around || 32,
    ~line: line :: maybe(Color || String || matching(#'inherit)) = #'inherit,
    ~fill: fill :: maybe(Color || String || matching(#'inherit)) = #false,
    ~line_width: line_width :: Real || matching(#'inherit) = #'inherit,
    ~rounded: rounded :: maybe(Real || matching(#'default)) = #false,
    ~epoch: epoch :: EpochAlignment = #'center,
    ~duration: duration :: DurationAlignment = #'sustain,
    ~refocus: refocus :: maybe(Pict || matching(#'around)) = #false
  ) :: Pict
){

 Creates a @tech{pict} to draw a rectangle. The rectangle's
 @rhombus(width) and @rhombus(height) can be supplied as numbers, or they
 can be supplied as a @tech{pict}, in which case the given picts' width
 and height are used, respectively. If an @rhombus(around) pict is
 provided, then it both supplies default @rhombus(width) and
 @rhombus(height) values an is @rhombus(overlay)ed on top of the rectangle
 image,

 The rectangle has an outline if @rhombus(line) is not @rhombus(#false),
 and it is filled in if @rhombus(fill) is not @rhombus(#false). If the
 rectangle has an outline, @rhombus(line_width) is used for the outline. A
 @rhombus(line), @rhombus(fill), or @rhombus(line_width) can be
 @rhombus(#'inherit) to indicate that a context-supplied color and line
 width should be used. See also @rhombus(Pict.colorize) and
 @rhombus(Pict.colorize) @rhombus(Pict.line_width).

 If @rhombus(rounded) is a non-negative number, it is used as the radius
 of an arc to use for the rectangle's corners. If @rhombus(rounded) is a
 negative number, it is negated and multipled by the rectangle's width
 and height to get a radius (in each direction) for the rounded corner.

 The @rhombus(epoch) and @rhombus(duration) arguments are used only when
 @rhombus(around) is supplied, and they are passed on to
 @rhombus(overlay) to combine a static rectangle pict with
 @rhombus(around). If @rhombus(around) is @rhombus(#false), the resulting
 pict is always a @tech{static pict}.

 When the @rhombus(refocus) argument is not @rhombus(#false), then
 @rhombus(Pict.refocus) is used on the resulting pict. If
 @rhombus(refocus) is @rhombus(#'around), then the pict is refocused on
 @rhombus(around), otherwise it is refocused on @rhombus(refocus).

}


@doc(
  fun square(
    ~around: around :: maybe(Pict) = #false,
    ~size: size :: Real || Pict = around || 32,
    ~line: line :: maybe(Color || String || matching(#'inherit)) = #'inherit,
    ~fill: fill :: maybe(Color || String || matching(#'inherit)) = #false,
    ~line_width: line_width :: Real || matching(#'inherit) = #'inherit,
    ~epoch: epoch :: EpochAlignment = #'center,
    ~duration: duration :: DurationAlignment = #'sustain,
    ~refocus: refocus :: maybe(Pict || matching(#'around)) = #false              
  ) :: Pict
){

 A shorthand for @rhombus(rectangle) where the width and height are
 specified as @rhombus(size) or, if @rhombus(size) is a pict, as the
 maximum of the pict's width and height.

}

@doc(
  fun beside(
    ~sep: sep :: Real = 0,
    ~vert: vert :: VertAlignment = #'center,
    ~epoch: epoch :: EpochAlignment = #'center,
    ~duration: duration :: DurationAlignment = #'sustain,
    pict :: Pict, ...
  ) :: Pict
){

 Creates a pict that combines the given @rhombus(pict)s vertically.

}

@doc(
  fun above(
    ~sep: sep :: Real = 0,
    ~horiz: horiz :: HorizAlignment = #'center,
    ~epoch: epoch :: EpochAlignment = #'center,
    ~duration: duration :: DurationAlignment = #'sustain,
    pict :: Pict, ...
  ) :: Pict
){

 Creates a pict that combines the given @rhombus(pict)s vertically.

}

@doc(
  fun overlay(
    ~horiz: horiz :: HorizAlignment = #'center,
    ~vert: vert :: VertAlignment = #'center,
    ~epoch: epoch :: EpochAlignment = #'center,
    ~duration: duration :: DurationAlignment = #'sustain,
    pict :: Pict, ...
  ) :: Pict
){

 Creates a pict that combines the given @rhombus(pict)s.

}


@doc(
  fun Pict.from_handle(handle) :: Pict
){

 Converts a static pict value compatible with the Racket
 @racketmodname(pict) library into a @rhombus(Pict, ~annot) value.

}
