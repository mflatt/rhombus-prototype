#lang racket/base

(provide (struct-out spacer-binding))

(struct spacer-binding (datum
                        mpi sym nom-mpi nom-sym
                        ns-mpi ns-sym ns-nom-mpi ns-nom-sym)
  #:prefab)
