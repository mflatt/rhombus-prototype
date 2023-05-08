#lang scribble/rhombus/manual
@(import: "common.rhm" open
          "nonterminal.rhm" open)

@(def dots: @rhombus(..., ~bind))
@(def dots_expr: @rhombus(...))

@title{Sequences}

A @deftech{sequence} supplies elements to a @rhombus(for) iteration.
Lists, maps, sets, and arrays are sequences, and new kinds of sequences
can be defined by implementing @rhombus(Sequenceable, ~class).

@doc(
  annot.macro 'Sequence'
){

 Matches any @tech{sequence}.

}

@doc(
  expr.macro '$n_expr .. $m_expr'
  expr.macro '$n_expr ..'
){

 If @rhombus(n_expr) produces an integer @rhombus(n, ~var) and
 @rhombus(m_expr) (when supplied) produces an integer @rhombus(m, ~var),
 returns a @tech{sequence} containing the integers from @rhombus(n, ~var)
 (inclusive) to @rhombus(m, ~var) (exclusive). If @rhombus(m_expr) is not
 specified, the result is an infinite sequence that contains all integers
 starting from @rhombus(n, ~var).

 The @rhombus(..)'s precedence is lower than the arithmetic operators
 @rhombus(+), @rhombus(-), @rhombus(*), and @rhombus(/). In particular,
 @rhombus(n_expr..1+m_expr) creates a sequence that includes
 @rhombus(m, ~var) for many forms @rhombus(m_expr).

}

