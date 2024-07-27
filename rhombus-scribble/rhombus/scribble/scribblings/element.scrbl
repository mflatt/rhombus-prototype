#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "element"){Elements}

@include_doc(rhombus/scribble/private/element: elem){

 Constructs an @rhombus(Element, ~annot) with a specific style.

}

@include_doc(rhombus/scribble/private/element:
               italic
               bold
               emph
               tt
               subscript
               superscript
               smaller
               larger){

 Element constructors that adjust the given pre-content to change its
 rendered style.

}

@include_doc(rhombus/scribble/private/element: literal){

 Constructs an element with a string to be used literally, as opposed to
 decoding as usual for @rhombus(PreContent).

}
