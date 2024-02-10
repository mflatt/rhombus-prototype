#lang scribble/rhombus/manual
@(import:
    "common.rhm" open:
      except: def
    "nonterminal.rhm" open
    meta_label:
      rhombus/random open)

@title{Random Number Generation}

@docmodule(rhombus/random)

@doc(
  annot.macro 'PRNG'
  fun PRNG() :: PRNG
  fun PRNG(state :: PRNGState) :: PRNG
){

 Annotation and constructors for a pseudo-random number generator.

}

@doc(
  method (prng :: PRNG).random() :: Real.in(0 ~exclusive, 1 ~exclusive)
  method (prng :: PRNG).random(n :: PosInt) :: Int.in(0, n ~exclusive)
  method (prng :: PRNG).random(start :: Int, end :: Int)
    :: Int.in(start, end ~exclusive)
){

 Steps @rhombus(prng) to obstain a number.

 Using @rhombus(math.random) is the same as using
 @rhombus(PRNG.current())'s @rhombus(PRNG.random).

}

@doc(
  method (prng :: PRNG).state :: PRNGState
  method (prng :: PRNG).state := (s :: PRNGState)
){

 A property for the state of @rhombus(prng).

}

@doc(
  def PRNG.current :: Parameter
  fun PRNG.current() :: PRNG
  fun PRNG.current(prng :: PRNG) :: Void
){

 A parameter for the pseudo-random number generator that is used by
 @rhombus(math.random).

}

@doc(
  annot.macro 'PRNGState'
){

 Satisfied by an array of 6 values where the first three values are
 integers in the range @rhombus(0) to @rhombus(4294967086), inclusive,
 and the last three integers are in the range @rhombus(0) to
 @rhombus(4294944442),inclusive.

}
