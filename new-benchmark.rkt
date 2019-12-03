#lang racket/base

(require math/flonum
         racket/math
         racket/system
         racket/unsafe/ops)

(require (prefix-in unsafe- "./glm-shim.rkt")
         (prefix-in optimized- "./glm-opt.rkt"))

(define-syntax-rule (benchmark make-v1 make-v2 op ...)
  (let ()
    (define t0 (current-inexact-milliseconds))
    (define v2 make-v2)
    (let loop ([v1 make-v1]
               [count 0]
               [Δt 0])
      (if (< Δt 1000.0)
          (loop (op ... v1 v2)
                (add1 count)
                (- (current-inexact-milliseconds) t0))
          (printf "\t~a" (exact-round (/ count (/ Δt 1000.0))))))
    (flush-output)))

(module typed typed/racket/base
  (require racket/math
           math/flonum 
           racket/unsafe/ops)

  (provide do-typed-benchmarks)

  (define-syntax-rule (typed-benchmark T make-v1 make-v2 op ...)
    (let ()
      (define t0 (current-inexact-milliseconds))
      (define v2 make-v2)
      (let loop ([v1 : T make-v1]
                 [count : Float 0.0]
                 [Δt : Float 0.0])
        (if (< Δt 1000.0)
            (loop (op ... v1 v2)
                  (add1 count)
                  (- (current-inexact-milliseconds) t0))
            (printf "\t~a" (abs (exact-round (/ count (/ Δt 1000.0)))))))
      (flush-output)))

  (: do-typed-benchmarks (-> Void))
  (define (do-typed-benchmarks)

    (typed-benchmark FlVector
                     (flvector 1.0000001 1.0000001 1.0000001 1.0000001)
                     (flvector 1.0000001 1.0000001 1.0000001 1.0000001)
                     inline-flvector-map fl*)

    (typed-benchmark FlVector
                     (flvector 1.0000001 1.0000001 1.0000001 1.0000001)
                     (flvector 1.0000001 1.0000001 1.0000001 1.0000001)
                     inline-flvector-map unsafe-fl*)

    ))

;; (require 'typed)
;; (do-typed-benchmarks)

(require glm/dvec
         (prefix-in uu- glm/unsafe))

(benchmark (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001)
           (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001) dvec*)

(benchmark (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001)
           (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001) dvec4*)

(benchmark (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001)
           (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001) dvec4*dvec4)

(benchmark (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001)
           (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001) uu-dvec*)

(benchmark (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001)
           (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001) uu-dvec4*)

(benchmark (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001)
           (uu-dvec4 1.0000001 1.0000001 1.0000001 1.0000001) uu-dvec4*dvec4)

;; (benchmark (unsafe-dvec4 1.0000001 1.0000001 1.0000001 1.0000001)
;;            (unsafe-dvec4 1.0000001 1.0000001 1.0000001 1.0000001) unsafe-dvec*)
(benchmark (unsafe-dvec4 1.0000001 1.0000001 1.0000001 1.0000001)
           (unsafe-dvec4 1.0000001 1.0000001 1.0000001 1.0000001) unsafe-dvec4*)

;; (benchmark (optimized-dvec4 1.0000001 1.0000001 1.0000001 1.0000001)
;;            (optimized-dvec4 1.0000001 1.0000001 1.0000001 1.0000001) optimized-dvec*)
(benchmark (optimized-dvec4 1.0000001 1.0000001 1.0000001 1.0000001)
           (optimized-dvec4 1.0000001 1.0000001 1.0000001 1.0000001) optimized-dvec4*)

(void (system "./baseline"))
;; (void (system "./optimized"))
(newline)
