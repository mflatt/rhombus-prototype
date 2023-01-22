Internal Implementation Notes
=============================

Binding
-------

Never export a Rhombus identifier in the default space. Always use
`rhombus/expr` to bind in the expresion space, for example. Avoiding
the default space means that Rhombus programmers can take fine control
over binding and extension using `only_space`, etc.

Most spaces are distinct, but there are some key interactions:

 * Definition macros, declaration macros, and expressions all use the
   same space, `rhombus/expr`. That's because a definition context
   also allowes expressions, etc.

 * Repetition bindings work only when the expression binding has the
   same compile-time value. Otherwise, we'd have to remember to bind
   in the repetition space every time we bind in the expresion space.

   The same value works as an expression and as a repetition because
   it can be a structure implements both the expression and repetition
   properties.
