#lang racket/base

(require ffi/cvector
         (only-in ffi/unsafe
                  _array _double ptr-ref malloc array-length array-ref array-set!)
         ffi/vector
         (except-in math/array array-ref array-set!)
         math/flonum
         racket/format
         racket/function
         racket/math
         racket/string
         racket/unsafe/ops
         racket/vector
         syntax/parse/define
         "./typed.rkt"
         (prefix-in unsafe- "./glm-shim.rkt")
         (for-syntax racket/base))

(define K 1)
(define T 1000)

(define Mmax 4)
(define Nmax 16)

(random-seed 1)

;;; ----------------------------------------------------------------------------

(define (benchmark M N make op)
  (define (make-random)
    (apply make (for/list ([_ (in-range M)]) (random))))
  (define vs (build-list (- N 1) (λ _ (make-random))))
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define t0 (current-inexact-milliseconds))
  (let loop ([v (make-random)]
             [count 0]
             [Δt 0])
    (if (< Δt T)
        (loop (for/fold ([v1 v]) ([v2 (in-list vs)]) (op v1 v2))
              (add1 count)
              (- (current-inexact-milliseconds) t0))
        (exact-round (/ count (/ Δt T))))))

(define-simple-macro (define-benchmark-cases [name make op] ...)
  #:with (type ...) (for/list ([_ (in-list (attribute name))]
                               [i (in-naturals)])
                      #`#,(add1 i))
  #:with print-benchmark-index (datum->syntax this-syntax 'print-benchmark-index)
  #:with run-benchmarks (datum->syntax this-syntax 'run-benchmarks)
  (begin
    (define (print-benchmark-index)
      (printf "type\tname\n")
      (for ([t (in-list '(type ...))]
            [n (in-list '(name ...))])
        (printf "~a\t~a\n" t n))
      (print-typed-benchmark-index (length '(type ...))))
    (define (run-benchmarks)
      (printf (string-join '("# components" "# vectors" "type" "count") "\t"))
      (newline)
      (flush-output)
      (for* ([M (in-range 4 (add1 Mmax))]
             [N (in-range 2 (add1 Nmax))])
        (begin
          (benchmark M N make op)
          (printf (string-join (map ~a (list M N type (benchmark M N make op))) "\t"))
          (newline)
          (flush-output))
        ...
        (run-typed-benchmark (length '(type ...)) M N)))))

;;; ----------------------------------------------------------------------------

(define (ffi-array . xs)
  (define t (_array _double (length xs)))
  (define v (ptr-ref (malloc t 'atomic) t 0))
  (for ([i (in-range (length xs))]
        [x (in-list xs)])
    (array-set! v i x))
  v)

(define (ffi-cvector . xs)
  (apply cvector _double xs))

(define (dvec . xs)
  (case (length xs)
    [(2) (apply unsafe-dvec2 xs)]
    [(3) (apply unsafe-dvec3 xs)]
    [(4) (apply unsafe-dvec4 xs)]))

(define-benchmark-cases

  ;; ["racket/base list for/list *"
  ;;  list (λ (v1 v2)
  ;;         (for/list ([x1 (in-list v1)]
  ;;                    [x2 (in-list v2)])
  ;;           (* x1 x2)))]

  ;; ["racket/base list for/list fl*"
  ;;  list (λ (v1 v2)
  ;;         (for/list ([x1 (in-list v1)]
  ;;                    [x2 (in-list v2)])
  ;;           (fl* x1 x2)))]

  ;; ["racket/base list for/list unsafe-fl*"
  ;;  list (λ (v1 v2)
  ;;         (for/list ([x1 (in-list v1)]
  ;;                    [x2 (in-list v2)])
  ;;           (unsafe-fl* x1 x2)))]

  ;; ["racket/vector vector for/vector *" vector (λ (v1 v2)
  ;;                                               (for/vector ([x1 (in-vector v1)]
  ;;                                                            [x2 (in-vector v2)])
  ;;                                                 (* x1 x2)))]

  ;; ["racket/vector vector for/vector fl*"
  ;;  vector (λ (v1 v2)
  ;;           (for/vector ([x1 (in-vector v1)]
  ;;                        [x2 (in-vector v2)])
  ;;             (fl* x1 x2)))]

  ;; ["racket/vector vector for/vector unsafe-fl*"
  ;;  vector (λ (v1 v2)
  ;;           (for/vector ([x1 (in-vector v1)]
  ;;                        [x2 (in-vector v2)])
  ;;             (unsafe-fl* x1 x2)))]

  ;; ["racket/vector vector vector-map *" vector (curry vector-map *)]

  ;; ["racket/vector vector vector-map fl*" vector (curry vector-map fl*)]

  ;; ["racket/vector vector vector-map unsafe-fl*" vector (curry vector-map unsafe-fl*)]

  ["math/array *" (compose list->array list) (λ (v1 v2)
                                               (for/array ([x1 (in-array v1)]
                                                           [x2 (in-array v2)])
                                                 (* x1 x2)))]

  ["math/array fl*" (compose list->array list)
                    (λ (v1 v2)
                      (for/array ([x1 (in-array v1)]
                                  [x2 (in-array v2)])
                                 (fl* x1 x2)))]

  ["math/array unsafe-fl*" (compose list->array list)
                    (λ (v1 v2)
                      (for/array ([x1 (in-array v1)]
                                  [x2 (in-array v2)])
                                 (unsafe-fl* x1 x2)))]

  ["math/array array*" (compose list->array list) (λ (a b) (array* a b))]

  ;; ["ffi/unsafe array *"
  ;;  ffi-array
  ;;  (λ (v1 v2)
  ;;    (apply ffi-array (for/list ([i (in-range (array-length v1))])
  ;;                       (* (array-ref v1 i) (array-ref v2 i)))))]

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

  ;; ["ffi/vector f64vector *"
  ;;  f64vector (λ (v1 v2)
  ;;              (list->f64vector
  ;;               (for/list ([x1 (in-list (f64vector->list v1))]
  ;;                          [x2 (in-list (f64vector->list v2))])
  ;;                 (* x1 x2))))]

  ;; ["ffi/vector f64vector fl*"
  ;;  f64vector (λ (v1 v2)
  ;;              (list->f64vector
  ;;               (for/list ([x1 (in-list (f64vector->list v1))]
  ;;                          [x2 (in-list (f64vector->list v2))])
  ;;                 (fl* x1 x2))))]

  ;; ["ffi/vector f64vector unsafe-fl*"
  ;;  f64vector (λ (v1 v2)
  ;;              (list->f64vector
  ;;               (for/list ([x1 (in-list (f64vector->list v1))]
  ;;                          [x2 (in-list (f64vector->list v2))])
  ;;                 (unsafe-fl* x1 x2))))]

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

  ;; ["math/flonum flvector for/flvector *"
  ;;  flvector (λ (v1 v2)
  ;;             (for/flvector ([x1 (in-flvector v1)]
  ;;                            [x2 (in-flvector v2)])
  ;;               (* x1 x2)))]

  ;; ["math/flonum flvector for/flvector fl*"
  ;;  flvector (λ (v1 v2)
  ;;             (for/flvector ([x1 (in-flvector v1)]
  ;;                            [x2 (in-flvector v2)])
  ;;               (fl* x1 x2)))]

  ;; ["math/flonum flvector for/flvector unsafe-fl*"
  ;;  flvector (λ (v1 v2)
  ;;             (for/flvector ([x1 (in-flvector v1)]
  ;;                            [x2 (in-flvector v2)])
  ;;               (unsafe-fl* x1 x2)))]

  ;; ["math/flonum flvector flvector-map *" flvector (curry flvector-map *)]

  ;; ["math/flonum flvector flvector-map fl*" flvector (curry flvector-map fl*)]

  ;; ["math/flonum flvector flvector-map unsafe-fl*" flvector (curry flvector-map unsafe-fl*)]

  ;; ["math/flonum flvector inline-flvector-map *"
  ;;  flvector (λ (v1 v2) (inline-flvector-map * v1 v2))]

  ;; ["math/flonum flvector inline-flvector-map fl*"
  ;;  flvector (λ (v1 v2) (inline-flvector-map fl* v1 v2))]

  ;; ["math/flonum flvector inline-flvector-map unsafe-fl*"
  ;;  flvector (λ (v1 v2) (inline-flvector-map unsafe-fl* v1 v2))]

  ;; ["math/flonum flvector flvector*" flvector flvector*]

  ;; ["glm-shim unsafe-dvec*" dvec unsafe-dvec*]

  )

(print-benchmark-index)
(newline)
(run-benchmarks)
