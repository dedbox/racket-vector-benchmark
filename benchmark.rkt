#lang racket/base

(require glm/vec
         math/array
         math/flonum
         racket/math
         racket/unsafe/ops
         racket/vector
         syntax/parse/define
         (for-syntax racket/base))

(random-seed 1)

(define-syntax-rule (benchmark M N for/vector-type op arg ...)
  (let ()
    (define (random-vector)
      (for/vector-type ([_ (in-range M)]) (random)))
    (define vs (build-list (- N 1) (λ _ (random-vector))))
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define t0 (current-inexact-milliseconds))
    (define (loop count v)
      (define Δt (- (current-inexact-milliseconds) t0))
      (if (>= Δt 1000)
          (exact-round (/ count (/ Δt 1000)))
          (loop (+ count 1) (for/fold ([v1 v]) ([v2 vs]) (op arg ... v1 v2)))))
    (printf "\t~a" (loop 0 (random-vector)))
    (flush-output)))

(module benchmark-typed-array typed/racket/base
  (require math/array
           racket/math)
  (provide (all-defined-out))

  (: typed-array* (-> (Array Flonum) (Array Flonum) (Array Flonum)))
  (define (typed-array* v1 v2)
    (array* v1 v2))

  (define-syntax-rule (typed-array-benchmark M N for/vector-type op arg ...)
    (let ()
      (: random-vector (-> (Array Flonum)))
      (define (random-vector)
        (for/array: ([_ (in-range M)]) : Flonum (random)))
      (define vs : (Listof (Array Flonum))
        (build-list (- N 1) (λ _ (random-vector))))
      (collect-garbage)
      (collect-garbage)
      (collect-garbage)
      (define t0 (current-inexact-milliseconds))
      (: loop (-> Nonnegative-Integer (Array Flonum) Integer))
      (define (loop count v)
        (define Δt (- (current-inexact-milliseconds) t0))
        (if (>= Δt 1000)
            (exact-round (/ count (/ Δt 1000)))
            (loop (+ count 1) (for/fold ([v1 v]) ([v2 vs]) (op arg ... v1 v2)))))
      (printf "\t~a" (loop 0 (random-vector)))
      (flush-output)))

  (: do-typed-array-benchmark (-> Positive-Integer Positive-Integer Void))
  (define (do-typed-array-benchmark M N)
    (typed-array-benchmark M N for/array array*)))

(require 'benchmark-typed-array)

(module benchmark-typed-math-flonum typed/racket/base
  (require math/array
           racket/math
           math/flonum)
  (provide (all-defined-out))

  (define-syntax-rule (typed-flonum-benchmark M N)
    (let ()
      (: random-vector (-> FlVector))
      (define (random-vector)
        (for/flvector: ([_ (in-range M)]) (random)))
      (define vs : (Listof FlVector)
        (build-list (- N 1) (λ _ (random-vector))))
      (collect-garbage)
      (collect-garbage)
      (collect-garbage)
      (define t0 (current-inexact-milliseconds))
      (: loop (-> Nonnegative-Integer FlVector Integer))
      (define (loop count v)
        (define Δt (- (current-inexact-milliseconds) t0))
        (if (>= Δt 1000)
            (exact-round (/ count (/ Δt 1000)))
            (loop (+ count 1) (for/fold ([v1 v]) ([v2 vs]) (flvector* v1 v2)))))
      (printf "\t~a" (loop 0 (random-vector)))
      (flush-output)))

  (: do-typed-flonum-benchmark (-> Positive-Integer Positive-Integer Void))
  (define (do-typed-flonum-benchmark M N)
    (typed-flonum-benchmark M N)))

(require 'benchmark-typed-math-flonum)

(define (flvector-* v1 v2)
  (for/flvector ([x1 (in-flvector v1)]
                 [x2 (in-flvector v2)])
    (* x1 x2)))

(define (flvector-fl* v1 v2)
  (for/flvector ([x1 (in-flvector v1)]
                 [x2 (in-flvector v2)])
    (fl* x1 x2)))

(define (flvector-unsafe-fl* v1 v2)
  (for/flvector ([x1 (in-flvector v1)]
                 [x2 (in-flvector v2)])
    (unsafe-fl* x1 x2)))

(require glm/private/new-vector)

(module+ main
  (require racket/cmdline)

  (define-values (M N) (values #f #f))

  (command-line
   #:program "benchmark"
   #:args (num-components num-vectors)
   (set! M (string->number num-components))
   (set! N (string->number num-vectors)))

  ;;; math/array untyped
  (benchmark M N for/array array*)

  ;;; math/array typed->untyped
  (benchmark M N for/array typed-array*)

  ;;; math/array fully typed
  (do-typed-array-benchmark M N)

  ;;; ffi/unsafe array, dynamic dispatch
  (benchmark M N for/vec vec*)

  ;;; ffi/unsafe array, static dispatch
  (benchmark M N for/vec vec*vec)

  ;;; vector
  (benchmark M N for/vector vector-map *)

  ;;; vector + racket/flonum
  (benchmark M N for/vector vector-map fl*)

  ;;; vector + racket/unsafe/ops
  (benchmark M N for/vector vector-map unsafe-fl*)

  ;;; flvector
  (benchmark M N for/flvector flvector-*)

  ;;; flvector + racket/flonum
  (benchmark M N for/flvector flvector-fl*)

  ;;; flvector + racket/unsafe/ops
  (benchmark M N for/flvector flvector-unsafe-fl*)

  ;;; flvector + math/flonum
  (benchmark M N for/flvector flvector*)

  ;;; math/flonum fully typed
  (do-typed-flonum-benchmark M N)

  ;;; new-vector
  (benchmark M N for/dvec dvec*dvec)

  (newline))
