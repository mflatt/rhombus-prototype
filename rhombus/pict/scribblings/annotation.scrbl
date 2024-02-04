#lang scribble/rhombus/manual

@(import:
    meta_label:
      pict open
      draw)

@title(~tag: "annot"){Annotations and Constants}

@doc(
  annot.macro 'Pict'
  annot.macro 'StaticPict'
){

 An annotation representing a @tech{pict} value or a pict value that is
 specifically a @tech{static pict}.

}

@doc(
  def nothing :: NothingPict
  annot.macro 'NothingPict'
){

 The @rhombus(nothing) pict is a special @tech{static pict} that acts as
 if it is not supplied at all. The @rhombus(Nothing, ~annot) annotation
 is satisfied by only @rhombus(nothing).

}

@doc(
  annot.macro 'HorizAlignment'
){

 Recognizes an option for horizontal alignment, which is either
 @rhombus(#'left), @rhombus(#'center), or @rhombus(#'right).

}

@doc(
  annot.macro 'VertAlignment'
){

 Recognizes an option for vertical alignment, which is either
 @rhombus(#'top), @rhombus(#'topline), @rhombus(#'center),
 @rhombus(#'baseline), or @rhombus(#'bottom).

}

@doc(
  annot.macro 'DurationAlignment'
){

 Recognizes an option for duration alignment, which is either
 @rhombus(#'sustain) or @rhombus(#'pad).

}

@doc(
  annot.macro 'EpochAlignment'
){

 Recognizes an option for epoch alignment, which is either
 @rhombus(#'early), @rhombus(#'center), @rhombus(#'stretch), or
 @rhombus(#'late).

}
