#lang racket/base

(provide (struct-out vec3))

(struct vec3 (x y z) #:transparent)
