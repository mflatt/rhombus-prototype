#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "annot"){Building Blocks}

@doc(
  annot.macro 'Element'
){

 Elements are components of a paragraph. The can be individual words or
 whole sentences, and they can have styling such as font or size changes.

 Element constructors include @rhombus(bold) and @rhombus(larger). Most
 elements are constructed implicitly, however, from content or
 pre-content in the form of a plain string.

}

@doc(
  annot.macro 'Content'
  annot.macro 'PreContent'
){

 Content is either a @rhombus(ReadableString, ~annot), @rhombus(Symbol, ~annot),
 @rhombus(Element, ~annot), a list of @rhombus(Content, ~annot).

 Pre-content is the same as content, but with the intent that strings
 will be decoded to convert, for example, @litchar{``} and @litchar{''} into
 @litchar{“} and @litchar{”}.

 Symbol (pre-)content is either @rhombus(#'mdash), @rhombus(#'ndash),
 @rhombus(#'ldquo), @rhombus(#'lsquo), @rhombus(#'rdquo),
 @rhombus(#'rsquo), @rhombus(#'larr), @rhombus(#'rarr), or
 @rhombus(#'prime). The symbol is rendered like the corresponding HTML
 entity (even for output forms other than HTML).

}

annot.macro 'PreFlow':
  'converting(fun (v): convert_pre_flow(v)) && True'


@doc(  
  annot.macro 'FlowBlock'
){

 A flow block is a generalization of a paragraph.

}

@doc(
  annot.macro 'Part'
){

 A part is a section, perhaps with subsection, or even a whole document.

}

@doc(
  annot.macro 'PartDecl'
){

 A part declaration is created by functions such as @rhombus(section),
 and they are used by document decoding to construct a tree of parts.

}

@doc(
  annot.macro 'Style'
){

 A style is metadata that is associated with an element or flow block.
 It affects rendering in a backend-specific way.

 For example, @rhombus(bold) creates an element with a style that makes
 the text bold.

}

@doc(
  annot.macro 'StyleLike'
){

 A @rhombus(StyleLike, ~annot) value can be coerced to a @rhombus(Style, ~annot).
 It can be a @rhombus(Style, ~annot) already, a @rhombus(String, ~annot),
 a @rhombus(Symbol, ~annot), or a @rhombus(List.of(Symbol), ~annot).

}
