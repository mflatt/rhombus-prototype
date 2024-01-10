#lang scribble/rhombus/manual

@(import:
    meta_label:
      pict open
      draw)

@title{Rhombus Pict: Functional Pictures}

@docmodule(pict)

The @rhombusmodname(pict) Rhombus library starts with the same
@deftech{pict} concept as the Racket @racketmodname(pict) library, but
the Rhombus pict abstraction includes direct support for slideshow
animations. That is, the @rhombus(Pict) datatype from the
@rhombusmodname(pict) library covers both static pictures, as useful in
a paper, and dynamic pictures, as useful in a slide presentation. In
particular, a @rhombus(Pict, ~annot) can represent an entire Slideshow talk.
Concretely, you can pass an animated pict to a function like
@rhombus(above), and the result will be an animated pict that animates
and steps concurrent to other animated picts provided in the same
@rhombus(above) combination.

@table_of_contents()

@include_section("overview.scrbl")
@include_section("api.scrbl")
