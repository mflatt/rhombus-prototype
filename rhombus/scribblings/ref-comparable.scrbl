#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Comparables}

A @deftech{comparable} value is one that supports @rhombus(<),
@rhombus(<=), @rhombus(>=),@rhombus(>=), @rhombus(compares_equal), and
@rhombus(compares_unequal). Real numbers, @tech{characters},
@tech{strings}, @tech{byte strings}, @tech{symbols}, and @tech{keywords}
are all comparable, as are instances of classes that implement
@rhombus(Comparable, ~class).

@doc(
  operator ((v1 :: Comparable) < (v2 :: Comparable)) :: Boolean
  operator ((v1 :: Comparable) > (v2 :: Comparable)) :: Boolean
  operator ((v1 :: Comparable) <= (v2 :: Comparable)) :: Boolean
  operator ((v1 :: Comparable) >= (v2 :: Comparable)) :: Boolean
  operator ((v1 :: Comparable) compares_equal (v2 :: Comparable))
    :: Boolean
  operator ((v1 :: Comparable) compares_unequal (v2 :: Comparable))
    :: Boolean
){

 Compares @rhombus(v1) and @rhombus(v2), which uses a primitive
 comparison operation for real numbers, characters, strings, byte string,
 symbols, and keywords, or it calls the @rhombus(compare_to, ~datum)
 method for a @rhombus(Comparable, ~class) instance.

 See also @rhombus(.<), @rhombus(.>), @rhombus(.<=), @rhombus(.>=),
 @rhombus(.=), and @rhombus(.!=), which are specific to numbers.

 The difference between @rhombus(compares_equal) and @rhombus(==) is
 that the former uses @rhombus(Comparable, ~class) while the latter uses
 @rhombus(Equatable, ~class). The results tend to be the same, but they
 are different in the case of numbers: two numbers are @rhombus(==) only
 when they have the same exactness, but @rhombus(compares_equal)
 corresponds to @rhombus(.=).

 The @rhombus(use_static) declaration constrains @rhombus(<), etc., to
 work only when the left-hand argument or right-hand argument has static
 information indicating that it satisfies @rhombus(Comparable, ~annot).
 If both expressions provide static information, the
 @rhombus(Comparable, ~annot) specifications must be compatible: both
 identifying the same operation, or one specifying the generic
 @rhombus(Comparable, ~class) operation.

@examples(
  ~repl:
    1 < 2
    "apple" <= "banana"
    #'banana >= #'apple
    #'~banana > #'~apple
  ~repl:
    use_static
    ~error:
      1 < "apple"
)

}



@doc(
  interface Comparable
){

@provided_interface_and_other_annotation_only()

 An interface that a class can implement (publicly or privately) to make
 instances of the class work with @rhombus(<), @rhombus(>), etc. As an
 annotation, @rhombus(Comparable, ~annot) matches all @tech{comparable}
 objects, not just instances of classes that publicly implement the
 @rhombus(Comparable, ~class) interface.

 The interface has a single abstract method:

@itemlist(

 @item{@rhombus(#,(@rhombus(compare_to, ~datum))(#,(@rhombus(other, ~var))))
  --- the @rhombus(other, ~var) value is the right-hand argument to a
  comparison, and it is always an instance of the same class or a subclass
  that inherits the same @rhombus(compare_to, ~datum) implementation. The
  result must be an integer: negative if the object is less than the
  @rhombus(other, ~var), positive if the object is greater than
  @rhombus(other, ~var), and zero if they are equal.}

)


@examples(
  ~defn:
    class Posn(x, y):
      private implements Comparable
      private override method compare_to(other :: Posn):
        let delta = x - other.x
        if delta == 0
        | y - other.y
        | delta
  ~repl:
    Posn(1, 2) < Posn(2, 1)
    Posn(1, 2) < Posn(1, 3)
    Posn(1, 2) < Posn(1, 0)
)

}
