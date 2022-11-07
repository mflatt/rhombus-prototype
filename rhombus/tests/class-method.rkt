#lang rhombus
import: "check.rhm" open

use_static

check:
  ~eval_exn
  class Posn(x, y):
    method x(): 0
    method x(): 1
  "duplicate method name"

check:
  ~eval_exn
  class Posn(x, y):
    method x(): 0
  "identifier used as both a field name and method name"

check:
  ~eval_exn
  class Posn(x, y):
    private method x(): 0
  "identifier used as both a field name and method name"

check:
  ~eval_exn
  class Posn(x, y):
    field q: 1
    method q(): 0
  "identifier used as both a field name and method name"

check:
  ~eval_exn
  class Posn(x, y):
    private field q: 1
    method q(): 0
  "identifier used as both a field name and method name"

check:
  ~eval_exn
  class Posn(x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
    method x(): 0
  "identifier used as both a field name and method name"    

check:
  ~eval_exn
  class Posn(x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
    private method x(): 0
  "identifier used as both a field name and method name"    

check:
  ~eval_exn
  class Posn(x, y):
    nonfinal
    field q: 1
  class Posn3D(z):
    extends Posn
    method q(): 0
  "identifier used as both a field name and method name"    

check:
  ~eval_exn
  class Posn(x, y):
    nonfinal
    method z(): 0
  class Posn3D(z):
    extends Posn
  "identifier used as both a field name and method name"    

check:
  ~eval_exn
  class Posn(x, y):
    nonfinal
    method q(): 0
  class Posn3D(z):
    extends Posn
    field q: 1
  "identifier used as both a field name and method name"    

check:
  ~eval_exn
  class Posn(x, y):
    nonfinal
    method q(): 0
  class Posn3D(z):
    extends Posn
    private field q: 1
  "identifier used as both a field name and method name"    

check:
  ~eval_exn
  class Posn(x, y):
    nonfinal
    method m(): 0
  class Posn3D(z):
    extends Posn
    method m(): 0
  "method is already in superclass"

check:
  ~eval_exn
  class Posn(x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
    override m(): 0
  "method is not in superclass"

check:
  ~eval_exn
  class Posn(x, y):
    nonfinal
    final method m(): 0
  class Posn3D(z):
    extends Posn
    override m(): 0
  "cannot override superclass's final method"

check:
  class Posn(x, y):
    method m0(): [y, x]
    method m1(z): [y, x, z]
    method m2(z): m1(-z)
  val p: Posn(1, 2)
  [p.m0(),
   p.m1(3),
   p.m2(3)]
  [[2, 1],
   [2, 1, 3],
   [2, 1, -3]]

check:
  class Posn(x, y):
    nonfinal
    method m0(): [y, x]
    method m1(z): [y, x, z]
    method m2(z): m1(-z)
  class Posn3D(z):
    extends Posn
    override m1(w): [y, x, z, w]
    method m3(a, b, c): [a, m0(), m1(5), m2(6)]
    method m4(x): x
  val p: Posn(1, 2)
  val p3: Posn3D(1, 2, 3)
  [p.m0(), p.m1(3), p.m2(3),
   p3.m0(),
   p3.m1(4),
   p3.m2(4),
   p3.m3(10, 11, 12),
   p3.m4("x")]
  [[2, 1], [2, 1, 3], [2, 1, -3],
   [2, 1],
   [2, 1, 3, 4],
   [2, 1, 3, -4],
   [10, [2, 1], [2, 1, 3, 5], [2, 1, 3, -6]],
   "x"]

check:
  ~eval_exn
  use_static
  begin:
    class Posn(x, y):
      method m0(): [y, x]
    val p: Posn(1, 2)
    p.m0
  "method must be called for static mode"

check:
  ~eval_exn
  use_static
  begin:
    class Posn(x, y):
      method m0(): m0
    "ok"
  "method must be called"

check:
  use_dynamic
  class Posn(x, y):
    method m0(): [y, x]
  val p: Posn(1, 2)
  [dynamic(p).x,
   dynamic(p).m0 +& "",
   dynamic(p).m0()]
  [1,
   "#<function:m0>",
   [2, 1]]

check:
  class Posn(x, y):
    private field q: 1
    method m0(): [q, this.q]
  val p: Posn(1, 2)
  [p.m0()]
  [[1, 1]]

check:
  ~eval_exn
  use_static
  class Posn(x, y):
    private field q: 1
  (Posn(1, 2)).q
  "no such public field or method"

check:
  ~eval_exn
  use_static
  class Posn(x, y):
    private field q: 1
  Posn.q
  "identifier not provided"

check:
  ~exn
  use_dynamic
  class Posn(x, y):
    private field q: 1
  (Posn(1, 2)).q
  "no such field"

check:
  class Posn(x, y):
    nonfinal
    private field q: 1
    method lookup(p -: Posn): p.q
  class Posn3D(z):
    extends Posn
    private field q: "other"
    method get(): q
  val p: Posn3D(0, 2, 3)
  [p.lookup(p), p.get()]
  [1, "other"]

check:
  ~eval_exn
  use_static
  begin:
    class Posn(x, y):
      nonfinal
      private field q: 1
      method lookup(p :: Posn3D):
        p.q
    class Posn3D(z):
      extends Posn
    "ok"
  "no such public field or method"

check:
  class Posn(x, y):
    private field q: 1
    method get():
      class Helper():
        private field q: 1000
        method get(p :: Posn): [p.q, q]
      Helper().get(this)
  Posn(0, 10).get()
  [1, 1000]