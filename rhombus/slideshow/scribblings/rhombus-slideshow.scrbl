#lang scribble/rhombus/manual

@(import:
    meta_label:
      rhombus open
      slideshow open      
      draw.Font)

@title{Rhombus Slideshow: Figure and Presentation Tools}

@docmodule(slideshow)

The @rhombusmodname(slideshow) Rhombus library is built on the Racket
@racketmodname(slideshow) library, but uses the Rhombus
@rhombusmodname(pict) library's picts and @rhombusmodname(pict/text)
utilities for text layout. The @rhombusmodname(slideshow) module
re-exports @rhombusmodname(pict) and @rhombusmodname(pict/text).

@table_of_contents()

@doc(
  fun slide(~title: title :: maybe(String || Pict) = #false,
            ~name: name = title,
            ~layout: layout :: SlideLayout = #'auto,
            ~sep: sep :: Real = slide.gap,
            ~horiz: horiz :: HorizAlignment = #'center,
            ~lead_in: lead_in = #false,
            content, ...) :: Void
){

 Registers one or more slides.

}

@doc(
  def current_title_font :: Parameter
  fun current_title_font() :: Font
  fun current_title_font(font :: Font) :: Void
){

 A parameter for the font used by @rhombus(titlet).

}

@doc(
 fun titlet(content, ...) :: Pict
){

 Like @rhombus(t) from @rhombusmodname(pict/text), but using
 @rhombus(current_title_font()).

}

@doc(
 expr.macro 'titlely($content_expr, ...)'
 expr.macro 'titlely: $body; ...'
){

 Like @rhombus(boldly), etc., but for the slide title font configured
 via @rhombus(current_title_font).

}
