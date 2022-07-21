#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(val dots: @rhombus[..., ~bind])
@(val dots_expr: @rhombus[...])

@title{Lists}

Lists can be constructed using the syntax
@rhombus[[$$(@rhombus[expr, ~var]), ...]], which creates list containing
the values of the @rhombus[expr, ~var]s as elements.

A list works with map-referencing square brackets to access a list
element by position (in time proportional to the position), and it works
with the @rhombus[++] operator to append lists.

@doc[
  fun List(v :: Any, ...) :: List,
  fun List(v :: Any, ..., repetition, dots) :: List,
  grammar dots:
    $$(dots_expr)
]{

 Constructs a list of the given arguments, equivalent to using
 @rhombus[[v, ...]]. When @dots_expr appears after all arguments,
 the preceding positions is a @tech{repetition} position, and
 all elements of the repetition are included in order at the end of the
 list.

@examples[
  val lst: List(1, 2, 3),
  lst,
  lst[0],
  lst ++ [4, 5]
]

}

@doc[
  bind.macro 'List($binding, ...)',
  bind.macro 'List($binding, ..., $dots)',
  grammar dots:
    $$(dots)
]{

 Matches a list with as many elements as @rhombus[binding]s, or if
 @rhombus[dots] is included, at least as many elements as
 @rhombus[binding]s before the last one, and then them last one is
 matched against the rest of the list.

@examples[
  val List(1, x, y): [1, 2, 3],
  y,
  val List(1, x, ...): [1, 2, 3],
  [x, ...]
]

}

@doc[
  annotation.macro 'List',
  annotation.macro 'List.of($annotation)',
]{

 Matches any list in the form without @rhombus[of]. The @rhombus[of]
 variant matches a list whose elements satisfy @rhombus[annotation].

}

@doc[
  folder.macro 'List'
]{

 A @tech{folder} used with @rhombus[for], accumulates each result of a
 @rhombus[for] body into a result list.

}
