#lang racket/base

(require data/gvector
         racket/list
         racket/match
         racket/string
         racket/vector
         "types.rkt")

(provide load-obj)

(define (load-obj obj-path)
  (define mtls (make-gvector))
  (define positions (make-gvector))
  (define normals (make-gvector))
  (define tex-coords (make-gvector))
  (define groups (make-gvector))

  (define curr-group #f)

    ; Given a vertex, vertex normal, or face line, return the integer or float values from the line.
  (define (get-values line)
    (rest (string-split (string-normalize-spaces line))))

  (define (obj-val->float val)
    (+ 0.0 (string->number val)))

  (define (get-float3 line)
    (match (map obj-val->float (get-values line))
      [(list x y z) (float3 x y z)]))

    (define (get-uint3 line)
    (match (map string->number (get-values line))
      [(list x y z) (uint3 x y z)]))

  (define (get-v-vt-vn f0 f1 f2)
    (define vertices #())
    (define tex-coords #())
    (define normals #())

    (define (split-face f idx)
      (match (string-split f "/")
        [(list v) (set! vertices (vector-append vertices (vector (string->number v))))]
        [(list v vt vn) (set! vertices (vector-append vertices (vector (string->number v))))
                        (set! tex-coords (vector-append tex-coords (vector (string->number vt))))
                        (set! normals (vector-append normals (vector (string->number vn))))]))

    (split-face f0 0)
    (split-face f1 1)
    (split-face f2 2)

    (values
     (if (> (vector-length vertices) 0) vertices #f)
     (if (> (vector-length tex-coords) 0) tex-coords #f)
     (if (> (vector-length normals) 0) normals #f)))

  (define (get-tri-faces line faces)
    (define line-values (get-values line))
    (match line-values
      [(list f0 f1 f2) (let-values ([(vertices tex-coords normals) (get-v-vt-vn f0 f1 f2)])
                         (gvector-add! faces (face vertices tex-coords normals)))]
      ; Triangulate quads
      [(list f0 f1 f2 f3) (let-values ([(vertices-0 tex-coords-0 normals-0) (get-v-vt-vn f0 f1 f3)]
                                       [(vertices-1 tex-coords-1 normals-1) (get-v-vt-vn f1 f2 f3)])
                            (gvector-add! faces (face vertices-0 tex-coords-0 normals-0))
                            (gvector-add! faces (face vertices-1 tex-coords-1 normals-1)))]))

  (define (get-mtls path mtls)
    (define curr-name #f)
    (define ka #f)
    (define kd #f)
    (define ks #f)

    (define-values (base-path file-path is-dir) (split-path obj-path))

    (define mtl-path (build-path base-path path))

    (with-input-from-file mtl-path
      (λ ()
        (for ([curr-line-untrimmed (in-lines)])
          (let ([curr-line (string-trim curr-line-untrimmed)])
            (cond [(string-prefix? curr-line "newmtl ")
                   (cond [curr-name (gvector-add! mtls (mtl curr-name ka kd ks))
                                    (set! curr-name #f)
                                    (set! ka #f)
                                    (set! kd #f)
                                    (set! ks #f)])
                   (set! curr-name (list-ref (string-split (string-normalize-spaces curr-line)) 1))]
                  [(string-prefix? curr-line "map_Ka ") (set! ka (list-ref (string-split (string-normalize-spaces curr-line)) 1))]
                  [(string-prefix? curr-line "map_Kd ") (set! kd (list-ref (string-split (string-normalize-spaces curr-line)) 1))]
                  [(string-prefix? curr-line "map_Ks ") (set! ks (list-ref (string-split (string-normalize-spaces curr-line)) 1))])))))

    (cond [curr-name
           (gvector-add! mtls (mtl curr-name ka kd ks))]))            

  (with-input-from-file obj-path
    (λ ()
      (for ([curr-line (in-lines)])
        (cond [(string-prefix? curr-line "mtllib ")
               (get-mtls (list-ref (string-split (string-normalize-spaces curr-line)) 1) mtls)]
              [(string-prefix? curr-line "g ")
               (cond [curr-group (gvector-add! groups curr-group)])
               (set! curr-group (group (make-gvector) #f))]
              [(string-prefix? curr-line "usemtl")
               (set-group-mtl! curr-group (list-ref (string-split (string-normalize-spaces curr-line)) 1))]
              [(string-prefix? curr-line "v ")
               (gvector-add! positions (get-float3 curr-line))]
              [(string-prefix? curr-line "vn ")
               (gvector-add! normals (get-float3 curr-line))]
              [(string-prefix? curr-line "vt ")
               (gvector-add! tex-coords (get-float3 curr-line))]
              [(string-prefix? curr-line "f ")
               (get-tri-faces curr-line (group-faces curr-group))]))))

  (gvector-add! groups curr-group)

  (obj-model mtls positions normals tex-coords groups))
