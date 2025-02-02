#lang scribble/rhombus/manual
@(
  import:
    "common.rhm" open
    meta_label:
      lib("racket/draw.rkt"):
        expose:
          #{color-database<%>}
)

@title{Color}

@doc(
  class Color():  
    constructor
    | (name :: String)
    | (red :: Byte, green :: Byte, blue :: Byte)
    | (red :: Byte, green :: Byte, blue :: Byte, alpha :: Real.in(0.0, 1.0))
){

 When @rhombus(name) is given, it must be one of the predefined names
 listed with @rhombus(#{color-database<%>}).

 When @rhombus(alpha) is not supplied, @rhombus(1.0) is used.

}

@doc(
  property (col :: Color).red :: Byte
  property (col :: Color).green :: Byte
  property (col :: Color).blue :: Byte
  property (col :: Color).alpha :: Real.in(0, 1)
){

 Properties to access color components.

}
