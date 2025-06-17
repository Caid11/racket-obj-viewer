#lang racket

(require
  data/applicative
  data/gvector
  data/either
  data/monad
  megaparsack
  megaparsack/text

  "types.rkt")

(provide (struct-out vertex)
         (struct-out vertex-normal)
         (struct-out face)
         (struct-out mtl)
         (struct-out group)
         (struct-out obj-model)

         load-obj)

(struct vertex (loc) #:transparent)
(struct vertex-normal (loc) #:transparent)

(struct face (vertices tex-coords normals) #:transparent)
(struct mtl (name ka kd ks) #:transparent)
(struct group (faces mtl) #:transparent)

(struct obj-model (vertices normals tex-coords groups) #:transparent)

(struct comment (msg))

; Parser parameters

(define curr-group (make-parser-parameter #f))

(define vertices (make-parser-parameter (list)))
(define normals (make-parser-parameter (list)))
(define tex-coords (make-parser-parameter (list)))
(define faces (make-parser-parameter (list)))
(define groups (make-parser-parameter (list)))

(define comment/p
  (do (char-ci/p #\#)
      (many/p space/p)
      [c <- (many-until/p any-char/p
                          #:end (or/p (char-ci/p #\newline)
                                      eof/p))]
      (pure (comment (list->string (first c))))))

(define float/p
  (do [ns <- (many/p (or/p digit/p
                           (char-in/p "-.e")))]
      (pure (string->number (list->string ns)))))

(define int/p
  (do [ns <- integer/p]
      (pure ns)))

(define float3/p
  (do
    (many/p space/p)
    [v0 <- float/p]
    (many/p space/p)
    [v1 <- float/p]
    (many/p space/p)
    [v2 <- float/p]
    (pure (vec3 v0 v1 v2))))

(define uint3/p
  (do
    (many/p space/p)
    [v0 <- int/p]
    (many/p space/p)
    [v1 <- int/p]
    (many/p space/p)
    [v2 <- int/p]
    (pure (vec3 v0 v1 v2))))

(define vertex/p
  (do 
    (many/p space/p)
    (string/p "v ")
    [v <- float3/p]
    (char-ci/p #\newline)
    [vs <- (vertices)]
    (vertices (append vs (list v)))
    (pure (vertex v))))

(define normal/p
  (do
    (many/p space/p)
    (string/p "vn ")
    [v <- float3/p]
    (char-ci/p #\newline)
    (pure (vertex-normal v))))

(define face/p
  (do
    (many/p space/p)
    (string/p "f ")
    [v <- uint3/p] ; TODO: Make sure to parse normals and tex coords
    (char-ci/p #\newline)
    [fs <- (faces)]
    [f <- (pure (face v #f #f))]
    (faces (append fs (list f)))
    (pure f)))

(define group/p
  (do
    (string/p "g ")
    (many/p space/p)
    [name <- (many-until/p any-char/p
                           #:end space/p)]
    [curr-name <- (curr-group)]

    (cond
      [curr-name
       (do
         [gs <- (groups)]
         [fs <- (faces)]
         (groups (append gs (list (group (list->gvector fs) #f))))
         (faces (list))
         (curr-group name))]
      [else (curr-group name)])))

(define obj/p
  (do
    (many-until/p
      (or/p (try/p group/p)
            (try/p comment/p)
            (try/p normal/p)
            (try/p vertex/p)
            face/p)
      #:end eof/p)
    [v <- (vertices)]
    [n <- (normals)]
    [t <- (tex-coords)]
    [g <- (groups)]

    [curr-name <- (curr-group)]
    (cond
      [curr-name
        (do
          [fs <- (faces)]
          [gn <- (pure (group fs #f))]
          (pure (obj-model (list->gvector v) (list->gvector n) (list->gvector t) (list->gvector (append g (list gn))))))]
      [else (pure (obj-model (list->gvector v) (list->gvector n) (list->gvector t) (list->gvector g)))])))

(define (load-obj obj-path)
  (define obj-str (port->string (open-input-file obj-path) #:close? #t))
  (match (parse-string obj/p obj-str obj-path)
    [(success model)  model]
    [(failure msg)
     (printf "~a\n" (parse-error->string msg))
     #f]))

;(load-obj "C:\\src\\racket-obj-viewer\\racket-obj-viewer\\teapot.obj")
