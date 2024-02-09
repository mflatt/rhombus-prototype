#lang scribble/rhombus/manual

@(import:
    meta_label:
      rhombus open
      pict open
      pict/text open
      draw)

@title(~tag: "text"){Pict Text Layout}

@docmodule(pict/text)

The @rhombus(text) function from @rhombusmodname(pict) can draw text,
but only in a fixed, given font. The @rhombusmodname(pict/text) library
provides support for font configuration, nestable styles, and basic
paragraph typesetting.

@doc(
  def current_font :: Parameter
  fun current_font() :: Font
  fun current_font(font :: Font) :: Void
){

 The default font used by functions from @rhombusmodname(pict/text) for
 formatting text, including by @rhombus(t) and @rhombus(para).

}

@doc(
  def current_para_width :: Parameter
  fun current_para_width() :: Real
  fun current_para_width(width :: Real) :: Void
){

 The default line width for @rhombus(para) and related functions.

}

@doc(
  def current_line_sep :: Parameter
  fun current_line_sep() :: Real
  fun current_line_sep(sep :: Real) :: Void
){

 The default amount of separate used between lines by @rhombus(lines),
 @rhombus(para) and related functions.

}


@doc(
 fun t(content, ...,
       ~font: font :: draw.Font = current_font()) :: Pict
){

 Accepts @rhombus(content) as either a string, @tech{pict}, or list of
 such content (including, potentially, nested lists) and combines it into
 into a single @tech{pict}. Strings are converted to text using
 @rhombus(text) with @rhombus(font), and lists are flattened; the result
 listing of picts is the combined with @rhombus(beside) and
 @rhombus(#'topline) mode.

 Support for lists makes @rhombus(t) suitable for use with @litchar("@")
 notation, as in @litchar|{@t{Hello}}|.

}

@doc(
 fun bold(content, ...) :: Pict
 fun italic(content, ...) :: Pict
 fun tt(content, ...) :: Pict
 fun roman(content, ...) :: Pict
){

 Similar to @rhombus(t), but starts with @rhombus(current_font()) and
 adds boldness, italicness, fixed-widthness, or serifness to obstain the
 font for converting strings.

}

@doc(
  fun subscript(content, ...) :: Pict
  fun superscript(content, ...) :: Pict
){

 Like @rhombus(t), but the resulting @tech{pict}'s scale and baseline
 are adjusted to form subscript or superscript text.
 
}

@doc(
 expr.macro 'boldly($content_expr, ...)'
 expr.macro 'boldly: $body; ...'
 expr.macro 'italicly($content_expr, ...)'
 expr.macro 'italicly: $body; ...'
 expr.macro 'ttly($content_expr, ...)'
 expr.macro 'ttly: $body; ...'
 expr.macro 'romanly($content_expr, ...)'
 expr.macro 'romanly: $body; ...'
){

 Macros that @rhombus(parameterize) @rhombus(content_expr)s or
 @rhombus(body)s to adjust the @rhombus(current_font) parameter in the
 sameway as @rhombus(bold), @rhombus(italic), @rhombus(tt), or
 @rhombus(romanly). For the forms with @rhombus(content_expr)s, the
 results of the expressions are passed on to @rhombus(t). For the forms
 with @rhombus(body)s, the result is the result of the @rhombus(body)
 sequence.

}

@doc(
  fun lines(p :: Pict, ...,
            ~horiz: align :: HorizAlignment = #'left) :: Pict
){

 Like @rhombus(stack), but using @rhombus(current_line_sep()) as the
 separation amount and with a default horiztonal alignment of
 @rhombus(#'left).

}

@doc(
  fun para(content, ...,
           ~width: width = current_para_width(),
           ~horiz: align :: pict.HorizAlignment = #'left,
           ~full: full = #false) :: Pict
){

 Accepts @rhombus(content) similar to @rhombus(t), but combining into
 multiple lines instead of a single @rhombus(beside) combination, and a
 blank space is added between picts on the same line. Also, strings are
 treated more specially:

@itemlist(

 @item{all strings are split on space characters to obtain a list of
  strings to convert separately;}

 @item{strings are decoded to convert @litchar{``} to @litchar{“}, etc.; and}

 @item{when a string does not start or end with a space, then no space
  or line break is added between the string's pict (or last pict after
  splitting) and the pevious or subsequent pict.}
 
)

 The @rhombus(width) argument determines the maximum width of a line,
 and if @rhombus(full) is true, then the resulting pict is padded to that
 width. The @rhombus(align) argument constrolds both how multiple lines
 are aligned relative to one another and how padding is added when
 @rhombus(full) is true. Lines are separated by
 @rhombus(current_line_sep()) space.

}

@doc(
  fun item(content, ...,
           ~bullet: bullet :: maybe(Pict) = #false,
           ~width: width = current_para_width(),
           ~horiz: align :: pict.HorizAlignment = #'left,
           ~full: full = #false) :: Pict
  fun subitem(content, ...,
              ~bullet: bullet :: maybe(Pict) = #false,
              ~width: width = current_para_width(),
              ~horiz: align :: pict.HorizAlignment = #'left,
              ~full: full = #false) :: Pict
){

 Like @rhombus(para), but the paragraph is narrowed to leave room for a
 bullet and space after the bullet. When @rhombus(bullet) is
 @rhombus(#false), then @litchar("\u2022") is used for @rhombus(item) or
 @litchar("\u25E6") is used for @rhombus(subitem).

}
