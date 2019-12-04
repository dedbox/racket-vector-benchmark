#lang racket/base

(require racket/file
         racket/system)

(provide (all-defined-out))

(define (pre-installer _)
  (define native-dir (build-path "compiled" "native" (system-library-subpath)))
  (make-directory* native-dir)
  (unless (system "raco ctool --cc --compiler /usr/bin/g++ --ccf -O2 ++ccf -O0 glm-shim.cc")
    (error "failed compiling glm-shim"))
  (unless (system (format "raco ctool --ld ~a/glm-shim_rkt.so ++ldf -O0 glm-shim.o"
                          native-dir))
    (error "failed linking glm-shim"))
  (unless (system "raco ctool --cc --compiler /usr/bin/g++ --ccf -O2 ++ccf -Os glm-opt.cc")
    (error "failed compiling glm-opt"))
  (unless (system (format "raco ctool --ld ~a/glm-opt_rkt.so ++ldf -Os glm-opt.o"
                          native-dir))
    (error "failed linking glm-opt"))
  (unless (system (format "/usr/bin/g++ -O0 -o baseline baseline.cc"))
    (error "failed compiling baseline C++"))
  (unless (system (format "/usr/bin/g++ -Ofast -march=native -o optimized baseline.cc"))
    (error "failed compiling optimized C++"))
  )

(module+ main (pre-installer #t))
