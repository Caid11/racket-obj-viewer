#lang racket/base

(provide (struct-out float3)
         (struct-out uint3)
         (struct-out face)
         (struct-out mtl)
         (struct-out group)
         (struct-out obj-model))

(struct float3 (x y z))
(struct uint3 (x y z))

(struct face (vertices tex-coords normals))
(struct mtl (name ka kd ks))
(struct group ([faces #:mutable] [mtl #:mutable]))

(struct obj-model (mtls positions normals tex-coords groups))
