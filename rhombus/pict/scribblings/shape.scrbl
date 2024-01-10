#lang scribble/rhombus/manual

@(import:
    meta_label:
      rhombus open
      pict open
      draw)

@title(~tag: "shape"){Pict Constructors}

@doc(
  fun blank(
    size :: Real = 0,
    ~width: width :: Real = size,
    ~height: height :: Real = size,
    ~ascent: ascent :: Real = height,
    ~descent: descent :: Real = 0
  ) :: StaticPict
){

 Creates a blank @tech{static pict} with the specified bounding box.

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
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain,
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

 The @rhombus(epoch_align) and @rhombus(duration_align) arguments are
 used only when @rhombus(around) is supplied, and they are passed on to
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
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain,
    ~refocus: refocus :: maybe(Pict || matching(#'around)) = #false              
  ) :: Pict
){

 A shorthand for @rhombus(rectangle) where the width and height are
 specified as @rhombus(size) or, if @rhombus(size) is a pict, as the
 maximum of the pict's width and height.

}

@doc(
  fun ellipse(
    ~around: around :: maybe(Pict) = #false,
    ~width: width :: Real || Pict = around || 32,
    ~height: height :: Real || Pict = around || 32,
    ~arc: arc :: maybe(matching(#'cw || #'ccw)) = #false,
    ~start: start :: Real = 0,
    ~end: end :: Real = 2 * math.pi,
    ~line: line :: maybe(Color || String || matching(#'inherit)) = #'inherit,
    ~fill: fill :: maybe(Color || String || matching(#'inherit)) = #false,
    ~line_width: line_width :: Real || matching(#'inherit) = #'inherit,
    ~rounded: rounded :: maybe(Real || matching(#'default)) = #false,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain,
  ) :: Pict
){

 Like @rhombus(rectangle), but for an ellipse or arc/wedge. The pict
 draws an arc or widge if @rhombus(arc) is @rhombus(#'cw) (clockwise) or
 @rhombus(#'ccw) (counterclockwise).

}

@doc(
  fun circle(
    ~around: around :: maybe(Pict) = #false,
    ~size: size :: Real || Pict = around || 32,
    ~arc: arc :: maybe(matching(#'cw || #'ccw)) = #false,
    ~start: start :: Real = 0,
    ~end: end :: Real = 2 * math.pi,
    ~line: line :: maybe(Color || String || matching(#'inherit)) = #'inherit,
    ~fill: fill :: maybe(Color || String || matching(#'inherit)) = #false,
    ~line_width: line_width :: Real || matching(#'inherit) = #'inherit,
    ~rounded: rounded :: maybe(Real || matching(#'default)) = #false,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain,
  ) :: Pict
){

 Like @rhombus(square), but a shorthand for @rhombus(ellipse).
 
}

@doc(
  fun polygon(
    [pt :: draw.PointLike.to_point, ...],
    ~line: line :: maybe(Color || String || matching(#'inherit)) = #'inherit,
    ~fill: fill :: maybe(Color || String || matching(#'inherit)) = #false,
    ~line_width: line_width :: Real || matching(#'inherit) = #'inherit
  ) :: Pict
){

 Creates a @tech{pict} that draws a polygon.

}

@doc(
  fun line(
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~color: color :: Color || String || matching(#'inherit) = #'inherit,
    ~width: width :: Real || matching(#'inherit) = #'inherit
  ) :: Pict
){

 Creates a @tech{pict} that draws a line from the top-left of the pict.
 The @rhombus(dx) and @rhombus(dy) arguments determine both the shape of
 the line and the width and height of the pict.

}

@doc(
  fun text(content :: String,
           ~font: font :: draw.Font = draw.Font()) :: Pict
){

 Creates a @tech{pict} that draws text using @rhombus(font)

}

@doc(
  fun bitmap(path :: Path || String) :: Pict
){

 Creates a @tech{pict} that draws a bitmap as loaded from @rhombus(path).

}

@doc(
  fun dc(draw :: Function.of_arity(3),
         ~width: width :: Real,
         ~height: height :: Real,
         ~ascent: ascent :: Real = height,
         ~descent: descent :: Real = 0) :: Pict
){

 Creates a @tech{pict} with an arbitrary drawing context. The
 @rhombus(draw) function receives a s @rhombus(draw.DC), an x-offset, and
 a y-offset.

}


@doc(
  fun animate(
    proc :: Function.of_arity(1),
    ~extent: extent :: NonnegReal = 0.5,
    ~bend: bend = bend.fast_middle,
    ~sustain_edge: sustain_edge :: matching(#'before || #'after) = #'before
  ) :: Pict
){

 Creates an @tech{animated pict}. The @rhombus(proc) should accept a
 @rhombus(Real.in(), ~annot) and produce a @rhombus(StaticPict, ~annot).

}

@doc(
  fun Pict.from_handle(handle) :: Pict
){

 Converts a static pict value compatible with the Racket
 @racketmodname(pict) library into a @rhombus(Pict, ~annot) value.

}
