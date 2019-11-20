#lang typed/racket/base

(require (except-in math/array array?)
         math/flonum
         racket/format
         racket/function
         racket/math
         racket/string
         racket/unsafe/ops
         racket/vector
         syntax/parse/define
         (for-syntax racket/syntax
                     typed/racket/base))

(define K 1)
(define T 1000)

(define Mmax 4)
(define Nmax 16)

(random-seed 1)

;;; ----------------------------------------------------------------------------

(: random-vector (All (S V) (S * -> V) Positive-Integer (Flonum -> S) -> V))

(define (random-vector make M scalar)
  ((inst apply S V) make ((inst build-list S) M (λ _ (scalar (random))))))

(: random-vectors
   (All (S V) (S * -> V) Positive-Integer Nonnegative-Integer (Flonum -> S)
        -> (Listof V)))

(define (random-vectors make M N scalar)
  ((inst build-list V) N (λ _ ((inst random-vector S V) make M scalar))))

(: benchmark
   (All (S V) (S * -> V) Positive-Integer Positive-Integer (Flonum -> S) (V V -> V)
        -> Nonnegative-Integer))

(define (benchmark make M N scalar op)
  (define vs ((inst random-vectors S V) make M (sub1 N) scalar))
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define t0 (current-inexact-milliseconds))
  (let loop ([v : V ((inst random-vector S V) make M scalar)]
             [count : Nonnegative-Integer 0]
             [Δt : Flonum 0.0])
    (if (< Δt T)
        (loop (for/fold ([v1 v]) ([v2 (in-list vs)]) (op v1 v2))
              (add1 count)
              (- (current-inexact-milliseconds) t0))
        (abs (exact-round (/ count (/ Δt T)))))))

(define-simple-macro (define-benchmark-cases [name make S V scalar op] ...)
  #:with (type ...) (for/list ([_ (in-list (attribute name))]
                               [i (in-naturals)])
                      #`#,i)
  #:with print-typed-benchmark-index (datum->syntax this-syntax 'print-typed-benchmark-index)
  #:with run-typed-benchmarks (datum->syntax this-syntax 'run-typed-benchmarks)
  (begin
    (provide print-typed-benchmark-index run-typed-benchmarks)
    (: print-typed-benchmark-index (Index -> Void))
    (define (print-typed-benchmark-index offset)
      (for ([t (in-list (list type ...))]
            [n (in-list (list name ...))])
        (printf "~a\t~a\n" (+ t (cast offset Number) 1) n)))
    (: run-typed-benchmarks (Index -> Void))
    (define (run-typed-benchmarks offset)
      (for* ([M : Positive-Integer (in-range 2 (add1 Mmax))]
             [N : Positive-Integer (in-range 2 (add1 Nmax))])
        (begin
          (printf "~a\t~a\t~a\t~a\n" M N (+ type (cast offset Number) 1)
                  ((inst benchmark S V) make M N scalar op)))
        ...))))

;;; ----------------------------------------------------------------------------

(require/typed ffi/vector
  [#:opaque F64Vector f64vector?]
  [f64vector (-> Flonum * F64Vector)]
  [f64vector->list (F64Vector -> (Listof Flonum))]
  [list->f64vector ((Listof Flonum) -> F64Vector)])

(define-benchmark-cases
  ["typed racket/base (Listof Flonum) for/list *"
   list Flonum (Listof Flonum) fl (λ (v1 v2)
                                    (for/list ([x1 (in-list v1)]
                                               [x2 (in-list v2)])
                                      (* x1 x2)))]

  ["typed racket/base (Listof Flonum) for/list fl*"
   list Flonum (Listof Flonum) fl (λ (v1 v2)
                                    (for/list ([x1 (in-list v1)]
                                               [x2 (in-list v2)])
                                      (fl* x1 x2)))]

  ["typed racket/base (Listof Flonum) for/list unsafe-fl*"
   list Flonum (Listof Flonum) fl (λ (v1 v2)
                                    (for/list ([x1 (in-list v1)]
                                               [x2 (in-list v2)])
                                      (unsafe-fl* x1 x2)))]

  ["typed racket/vector (Mutable-Vectorof Flonum) for/vector *"
   vector Flonum (Mutable-Vectorof Flonum) fl
   (λ (v1 v2)
     (for/vector : (Mutable-Vectorof Flonum)
         ([x1 (in-vector v1)]
          [x2 (in-vector v2)])
       (* x1 x2)))]

  ["typed racket/vector (Mutable-Vectorof Flonum) for/vector fl*"
   vector Flonum (Mutable-Vectorof Flonum) fl
   (λ (v1 v2)
     (for/vector : (Mutable-Vectorof Flonum)
         ([x1 (in-vector v1)]
          [x2 (in-vector v2)])
       (fl* x1 x2)))]

  ["typed racket/vector (Mutable-Vectorof Flonum) for/vector unsafe-fl*"
   vector Flonum (Mutable-Vectorof Flonum) fl
   (λ (v1 v2)
     (for/vector : (Mutable-Vectorof Flonum)
         ([x1 (in-vector v1)]
          [x2 (in-vector v2)])
       (unsafe-fl* x1 x2)))]

  ["typed racket/vector (Mutable-Vectorof Flonum) vector-map *"
   vector Flonum (Mutable-Vectorof Flonum) fl (λ (v1 v2) (vector-map * v1 v2))]

  ["typed racket/vector (Mutable-Vectorof Flonum) vector-map fl*"
   vector Flonum (Mutable-Vectorof Flonum) fl (λ (v1 v2) (vector-map fl* v1 v2))]

  ["typed racket/vector (Mutable-Vectorof Flonum) vector-map unsafe-fl*"
   vector Flonum (Mutable-Vectorof Flonum) fl (λ (v1 v2) (vector-map unsafe-fl* v1 v2))]

  ["typed math/array (Array Flonum) *"
   (λ xs ((inst list->array Flonum) xs)) Flonum (Array Flonum) fl
   (λ (v1 v2) ((inst array-map Flonum Flonum Flonum) * v1 v2))]

  ["typed math/array (Array Flonum) fl*"
   (λ xs ((inst list->array Flonum) xs)) Flonum (Array Flonum) fl
   (λ (v1 v2) ((inst array-map Flonum Flonum Flonum) fl* v1 v2))]

  ["typed math/array (Array Flonum) unsafe-fl*"
   (λ xs ((inst list->array Flonum) xs)) Flonum (Array Flonum) fl
   (λ (v1 v2) ((inst array-map Flonum Flonum Flonum) unsafe-fl* v1 v2))]

  ["typed math/array (Array Flonum) array*"
   (λ xs ((inst list->array Flonum) xs)) Flonum (Array Flonum) fl
   (λ (v1 v2) (array* v1 v2))]

  ;; ["ffi/unsafe array *"
  ;;  f64array Flonum FFIArray fl
  ;;  (λ (v1 v2)
  ;;    (apply f64array (for/list : (Listof Flonum)
  ;;                        ([i : Nonnegative-Integer (in-range (ffi-array-length v1))])
  ;;                      (* (ffi-array-ref v1 i) (ffi-array-ref v2 i)))))]

  ;; ["ffi/unsafe array fl*"
  ;;  ffi-array
  ;;  (λ (v1 v2)
  ;;    (apply ffi-array (for/list ([i (in-range (array-length v1))])
  ;;                       (fl* (array-ref v1 i) (array-ref v2 i)))))]

  ;; ["ffi/unsafe array unsafe-fl*"
  ;;  ffi-array
  ;;  (λ (v1 v2)
  ;;    (apply ffi-array (for/list ([i (in-range (array-length v1))])
  ;;                       (unsafe-fl* (array-ref v1 i) (array-ref v2 i)))))]

  ["typed ffi/vector f64vector *"
   f64vector Flonum F64Vector fl
   (λ (v1 v2)
     (list->f64vector
      (for/list ([x1 (in-list (f64vector->list v1))]
                 [x2 (in-list (f64vector->list v2))])
        (* x1 x2))))]

  ["typed ffi/vector f64vector fl*"
   f64vector Flonum F64Vector fl
   (λ (v1 v2)
     (list->f64vector
      (for/list ([x1 (in-list (f64vector->list v1))]
                 [x2 (in-list (f64vector->list v2))])
        (fl* x1 x2))))]

  ["typed ffi/vector f64vector unsafe-fl*"
   f64vector Flonum F64Vector fl
   (λ (v1 v2)
     (list->f64vector
      (for/list ([x1 (in-list (f64vector->list v1))]
                 [x2 (in-list (f64vector->list v2))])
        (unsafe-fl* x1 x2))))]

  ;; ["ffi/cvector _double *"
  ;;  ffi-cvector
  ;;  (λ (v1 v2)
  ;;    (list->cvector
  ;;     (for/list ([x1 (in-list (cvector->list v1))]
  ;;                [x2 (in-list (cvector->list v2))])
  ;;       (* x1 x2))
  ;;     _double))]

  ;; ["ffi/cvector _double fl*"
  ;;  ffi-cvector
  ;;  (λ (v1 v2)
  ;;    (list->cvector
  ;;     (for/list ([x1 (in-list (cvector->list v1))]
  ;;                [x2 (in-list (cvector->list v2))])
  ;;       (fl* x1 x2))
  ;;     _double))]

  ;; ["ffi/cvector _double unsafe-fl*"
  ;;  ffi-cvector
  ;;  (λ (v1 v2)
  ;;    (list->cvector
  ;;     (for/list ([x1 (in-list (cvector->list v1))]
  ;;                [x2 (in-list (cvector->list v2))])
  ;;       (unsafe-fl* x1 x2))
  ;;     _double))]

  ["typed math/flonum flvector for/flvector *"
   flvector Flonum FlVector fl
   (λ (v1 v2)
     (for/flvector: ([x1 (in-flvector v1)]
                     [x2 (in-flvector v2)])
       (* x1 x2)))]

  ;; ["math/flonum flvector for/flvector fl*"
  ;;  flvector Flonum FlVector fl
  ;;  (λ (v1 v2)
  ;;    (for/flvector: ([x1 (in-flvector v1)]
  ;;                    [x2 (in-flvector v2)])
  ;;      (fl* x1 x2)))]

  ;; ["math/flonum flvector for/flvector unsafe-fl*"
  ;;  flvector Flonum FlVector fl
  ;;  (λ (v1 v2)
  ;;    (for/flvector: ([x1 (in-flvector v1)]
  ;;                    [x2 (in-flvector v2)])
  ;;      (unsafe-fl* x1 x2)))]

  ["typed math/flonum flvector flvector-map *"
   flvector Flonum FlVector fl (λ (v1 v2) (flvector-map * v1 v2))]

  ;; ["math/flonum flvector flvector-map fl*"
  ;;  flvector Flonum FlVector fl
  ;;  (λ (v1 v2)
  ;;    ((cast flvector-map ((Flonum Flonum -> Flonum) FlVector FlVector -> FlVector))
  ;;     (ann fl* (-> Flonum Flonum Flonum))
  ;;     v1 v2))]

  ;; ["math/flonum flvector flvector-map unsafe-fl*"
  ;;  flvector Flonum FlVector fl
  ;;  (λ (v1 v2)
  ;;    ((cast flvector-map ((Flonum Flonum -> Flonum) FlVector FlVector -> FlVector))
  ;;     (ann unsafe-fl* (-> Flonum Flonum Flonum))
  ;;     v1 v2))]

  ["typed math/flonum flvector inline-flvector-map *"
   flvector Flonum FlVector fl (λ (v1 v2) (inline-flvector-map * v1 v2))]

  ["typed math/flonum flvector inline-flvector-map fl*"
   flvector Flonum FlVector fl (λ (v1 v2) (inline-flvector-map fl* v1 v2))]

  ["typed math/flonum flvector inline-flvector-map unsafe-fl*"
   flvector Flonum FlVector fl (λ (v1 v2) (inline-flvector-map unsafe-fl* v1 v2))]

  ["typed math/flonum flvector flvector*" flvector Flonum FlVector fl flvector*])
