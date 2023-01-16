#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@(def dots: @rhombus(..., ~bind))
@(fun list(x, ...): [x, ...])

@title{Syntax Classes}

@doc(
  ~literal: kind pattern description field error_mode
            matching_also matching_when matching_unless
  defn.macro 'syntax.class $name $maybe_args
              | $pattern_case
              | ...'
  defn.macro 'syntax.class $name $maybe_args:
                $class_clause'

  grammar maybe_args:
    ($identifier_binding, ...)
    ($identifier_binding, ..., & $rest_identifier)
    #,(epsilon)

  grammar class_clause:
    pattern | $pattern_case | ...
    description: $body; ...
    error_mode $error_mode_keyword
    error_mode: $error_mode_keyword
    kind $kind_keyword
    kind: $kind_keyword
    fields: $identifier ...; ...
                 
  grammar pattern_case:
    $syntax_pattern
    $syntax_pattern: $pattern_body; ...

  grammar kind_keyword:
    ~term
    ~sequence
    ~group
    ~multi
    ~block

  grammar error_mode_keyword:
    ~opaque
    ~transparent

  grammar pattern_body:
    field $identifier_maybe_rep: $body; ...
    field $identifier_maybe_rep = $expr
    matching_also '$pattern': $body; ...
    matching_also '$pattern' = $expr
    matching_when $expr
    matching_when: $body; ...
    matching_unless $expr
    matching_unless $body; ...
    $body

  grammar identifier_maybe_rep:
    $identifier
    [$identifier_maybe_rep, $ellipsis]

  grammar ellipsis:
    #,(dots)
){

 Defines a @deftech{syntax class} that can be used in syntax patterns with
 @rhombus(::, ~syntax_binding). A syntax class can optionally have arguments, in which
 case every use of the syntax class with @rhombus(::, ~syntax_binding) must supply
 arguments; an @rhombus(identifier_binding) is like a @rhombus(kwopt_binding, ~var) for
 @rhombus(fun), but each binding must be a plain @rhombus(identifier) (i.e., annotations
 and general pattern matching are not supported). Identifiers bound as arguments
 are visible in @rhombus(clause) bodies.

 Syntax forms matched by the syntax class are described by
 @rhombus(pattern_case) alternatives.
 The @rhombus(pattern) clause is optional in the sense
 that pattern alternatives can be inlined directly in the
 @rhombus(syntax.class) form, but the @rhombus(pattern) subform makes
 room for additional options as clauses. Each kind of @rhombus(class_clause)
 alternative can be supplied at most once, and @rhombus(pattern) is
 required.

 An optional @rhombus(description) clause provides a description of the
 syntax class which is used to produce clearer error messages when a term
 is rejected by the syntax class. The result of the @rhombus(block) block
 must be a string or @rhombus(#false), where @rhombus(#false) is
 equivalent to not specifying a @rhombus(description). When
 @rhombus(error_mode) is declared as @rhombus(~opaque), then parsing
 error messages will not refer to the interior details of the pattern
 cases; insteda, messages will use the decsription string.

 An optional @rhombus(kind) declaration where the context within a
 pattern where a syntax class can be used, and it determines the kind
 of match that each pattern specifies. Declaring @rhombus(~term) means
 that each pattern represents a single term, and the syntax
 class can be used in the same way at the @rhombus(Term, ~stxclass)
 syntax class. Declaring @rhombus(~sequence) means that each pattern
 represents a sequence of terms that is spliced within a group;
 @rhombus(~sequence) is the default mode of a syntax class when no
 @rhombus(kind) is specified. Declaring @rhombus(~group) means that
 each pattern represents a @tech{group}, and the syntax class can be
 used in the same places as @rhombus(Group, ~stxclass) (i.e., alone
 within its group). Declaring @rhombus(~multi) means that the pattern
 represents multiple groups, and the syntax class can be used in the
 same way at the @rhombus(Multi, ~stxclass) syntax class. Declaring
 @rhombus(~multi) means that the pattern represents a block, and the
 syntax class can be used in the same way at the @rhombus(Block, ~stxclass)
 syntax class. With @rhombus(~term), each pattern must
 match only a single term, and with @rhombus(~block), each pattern
 must be a block pattern.

 A @rhombus(fields) declaration limits the set of pattern variables that
 are accessible from the class, where variables used in all
 @rhombus(pattern_case)s are otherwise available (as described next).
 Each identifier in @rhombus(fields) must be a field name that would be
 made available.

 The @rhombus(pattern_case) alternatives are the main content
 of a syntax class.
 After the class @rhombus(name) is defined, then when a
 variable @rhombus(id, ~var) is bound through a
 @seclink("stxobj"){syntax pattern} with
 @rhombus($(#,(@rhombus(id, ~var)) :: #,(@rhombus(name, ~var)))),
 it matches a syntax object that matches any of the
 @rhombus(syntax_case)s in the definition of
 @rhombus(stx_class_id ,~var), where the @rhombus(syntax_case)s are tried
 first to last. A pattern variable that is included in all of the
 @rhombus(syntax_pattern)s is a field of the syntax class, which is
 accessed from a binding @rhombus(id, ~var) using dot notation. For
 example, if the pattern variable is @rhombus(var, ~var), its value is
 accessed from @rhombus(id, ~var) using
 @list(@rhombus(id, ~var), @rhombus(.), @rhombus(var, ~var)).

 A @rhombus(pattern_case) matches when

@itemlist(

 @item{the @rhombus(syntax_pattern) at the start of the
  @rhombus(pattern_case) matches;}

 @item{every @rhombus(matching_also) match within the
  @rhombus(pattern_case) body also matches;}

 @item{every @rhombus(matching_when) clause within the
  @rhombus(pattern_case) body has a true value for its right-hand side;
  and}

 @item{every @rhombus(matching_unless) clause within the
  @rhombus(pattern_case) body has a false value for its right-hand side.}

)

 Every pattern variable in the initial @rhombus(syntax_pattern) of a
 @rhombus(pattern_case) as well as evey variable in every
 @rhombus(matching_when) is a candiate field name, as long as it is also
 a candiate in all other @rhombus(syntax_pattern)s within the syntax
 class. In addition, names declared with @rhombus(field) are also
 candidates, where @rhombus(field) is similar to @rhombus(def), but
 constrained to defining a plain identifier or a simple list repetition.

 The body of a @rhombus(syntax_case) can include other definitions and
 expressions. Those definitions and expressions can use pattern variables
 bound in the main @rhombus(syntax_pattern) of the case as well as any
 preceding @rhombus(matching_when) of an attribite declared by a
 preceding @rhombus(field). Consecutive definitions and expressions
 within a @rhombus(syntax_case) are form a definition context, but
 separated sets of definitions and expressions can refer only to
 definitions in earlier sets.

 A variable bound with a syntax class (within a syntax pattern) can be
 used without dot notation. In that case, the result for
 @rhombus(~sequence) mode is a sequence of syntax objects
 corresponding to the entire match of a @rhombus(syntax_pattern); use
 @rhombus(...) after a @rhombus($)-escaped reference to the variable
 in a syntax template. For other modes, the variable represents a
 single syntax object representing matched syntax.

@examples(
  ~eval: macro.make_for_meta_eval()
  meta:
    syntax.class Arithmetic
    | '$x + $y'
    | '$x - $y'
  expr.macro 'doubled_operands $(e :: Arithmetic)':
    values('$(e.x) * 2 + $(e.y) * 2', '')
  doubled_operands 3 + 5
  expr.macro 'add_one_to_expression $(e :: Arithmetic)':
    values('$e ... + 1', '')
  add_one_to_expression 2 + 2
  meta:
    syntax.class NTerms
    | '~one $a':
        field b = '0'
        field average = '$(Syntax.unwrap(a) / 2)'
    | '~two $a $b':
        def sum:
          Syntax.unwrap(a) + Syntax.unwrap(b)
        field average = '$(sum / 2)'
  expr.macro 'second_term $(e :: NTerms)':
    values(e.b, '')
  second_term ~two 1 2
  second_term ~one 3
  expr.macro 'average $(e :: NTerms)':
    values(e.average, '')
  average ~two 24 42
)

}
