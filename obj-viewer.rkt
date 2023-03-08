#lang racket/base

(require ffi/unsafe
         ffi/vector
         opengl
         racket/class
         racket/flonum
         racket/gui/base
         racket/list
         racket/match
         racket/string
         racket/vector)

; Linear math
; Use right-handed, y-up conventions
;===============================================================================

(define (nsin x)
  (* -1 (sin x)))

(define (ncos x)
  (* -1 (cos x)))

(define (mul-mat2f-vec2f mat v)
  (let ([m-0-0 (f32vector-ref mat 0)]
        [m-1-0 (f32vector-ref mat 1)]
        [m-0-1 (f32vector-ref mat 2)]
        [m-1-1 (f32vector-ref mat 3)]
        [v-0 (f32vector-ref v 0)]
        [v-1 (f32vector-ref v 1)])
  (f32vector (+ (* v-0 m-0-0) (* v-1 m-1-0))
             (+ (* v-0 m-0-1) (* v-1 m-1-1)))))

(define (mat4f-mat4f-* b a)
  (define result (make-f32vector 16))

  ; Take the dot product of every row from a with every column of b
  (for ([row 4])
    (for ([col 4])
      (let ([dest-idx (+ (* row 4) col)]
            [a-idx (* row 4)]
            [b-idx col])
        (f32vector-set! result dest-idx
                        (+ (* (f32vector-ref a a-idx) (f32vector-ref b b-idx))
                           (* (f32vector-ref a (+ a-idx 1)) (f32vector-ref b (+ b-idx 4)))
                           (* (f32vector-ref a (+ a-idx 2)) (f32vector-ref b (+ b-idx 8)))
                           (* (f32vector-ref a (+ a-idx 3)) (f32vector-ref b (+ b-idx 12))))))))
  result)
  
(define (mat4f-rotate-x theta)
  (f32vector 1.0 0.0          0.0         0.0
             0.0 (cos theta)  (sin theta) 0.0
             0.0 (nsin theta) (cos theta) 0.0
             0.0 0.0           0.0        1.0))

(define (mat4f-rotate-y theta)
  (f32vector (cos theta) 0.0  (nsin theta) 0.0
             0.0         1.0  0.0          0.0
             (sin theta) 0.0  (cos theta)  0.0
             0.0         0.0  0.0          1.0))

(define (mat4f-rotate-z theta)
  (f32vector (cos theta)  (sin theta) 0.0 0.0
             (nsin theta) (cos theta) 0.0 0.0
             0.0          0.0         1.0 0.0
             0.0          0.0         0.0 1.0))

(define (mat4f-translate x y z)
  (f32vector 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             x   y   z   1.0))

(define (mat4f-perspective fov aspect-ratio z-near z-far)
  (letrec ([y-scale (/ 1.0 (tan (* fov 0.5)))]
           [x-scale (/ y-scale aspect-ratio)]
           [z-diff (- z-near z-far)])
  (f32vector x-scale 0.0     0.0                         0.0
             0.0     y-scale 0.0                         0.0
             0.0     0.0     (/ (+ z-far z-near) z-diff) (/ (* (* z-far z-near) 2.0) z-diff)
             0.0     0.0     -1.0                        1.0)))

(define (normalize x y z)
  (define length (sqrt (+ (* x x) (* y y) (* z z))))
  (values (/ x length) (/ y length) (/ z length)))

(define (cross x0 x1 x2 y0 y1 y2)
  (values
   (- (* x1 y2) (* x2 y1))
   (- (* x2 y0) (* x0 y2))
   (- (* x0 y1) (* x1 y0))))

(define (dot x0 x1 x2 y0 y1 y2)
  (+ (* x0 y0)
     (* x1 y1)
     (* x2 y2)))

(define (mat4f-lookat pos-x pos-y pos-z target-x target-y target-z up-x up-y up-z)
  (letrec-values ([(f0u) (- target-x pos-x)]
                  [(f1u) (- target-y pos-y)]
                  [(f2u) (- target-z pos-z)]
                  [(f0 f1 f2) (normalize f0u f1u f2u)]

                  [(s0u s1u s2u) (cross f0 f1 f2 up-x up-y up-z)]
                  [(s0 s1 s2) (normalize s0u s1u s2u)]
                  
                  [(u0 u1 u2) (cross s0 s1 s2 f0 f1 f2)])
    (f32vector s0 u0 (* -1 f0) 0.0
               s1 u1 (* -1 f1) 0.0
               s2 u2 (* -1 f2) 0.0
               (dot (* -1 s0) (* -1 s1) (* -1 s2) pos-x pos-y pos-z) (dot (* -1 u0) (* -1 u1) (* -1 u2) pos-x pos-y pos-z) (dot f0 f1 f2 pos-x pos-y pos-z) 1.0)))

(define (write-mat4f mat)
  (for ([row 4])
    (for ([col 4])
      (write (f32vector-ref mat (+ (* 4 row) col)))
      (display "\t"))
    (display "\n")))

; OBJ loading
;===============================================================================

(struct float3 (x y z))
(struct uint3 (x y z))

(struct face (vertices tex-coords normals))
(struct mtl (name ka kd ks))
(struct group ([faces #:mutable] [mtl #:mutable]))

(struct obj-model (mtls positions normals tex-coords groups))

(define (load-obj obj-path)
  (define mtls #())
  (define positions #())
  (define normals #())
  (define tex-coords #())
  (define groups #())

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

  (define (get-tri-faces line)
    (define line-values (get-values line))
    (match line-values
      [(list f0 f1 f2) (let-values ([(vertices tex-coords normals) (get-v-vt-vn f0 f1 f2)])
                         (vector (face vertices tex-coords normals)))]
      ; Triangulate quads
      [(list f0 f1 f2 f3) (let-values ([(vertices-0 tex-coords-0 normals-0) (get-v-vt-vn f0 f1 f3)]
                                       [(vertices-1 tex-coords-1 normals-1) (get-v-vt-vn f1 f2 f3)])
                            (vector (face vertices-0 tex-coords-0 normals-0) (face vertices-1 tex-coords-1 normals-1)))]))

  (define (get-mtls path)
    (define mtls #())

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
                   (cond [curr-name (set! mtls (vector-append mtls (vector (mtl curr-name ka kd ks))))
                                    (set! curr-name #f)
                                    (set! ka #f)
                                    (set! kd #f)
                                    (set! ks #f)])
                   (set! curr-name (list-ref (string-split (string-normalize-spaces curr-line)) 1))]
                  [(string-prefix? curr-line "map_Ka ") (set! ka (list-ref (string-split (string-normalize-spaces curr-line)) 1))]
                  [(string-prefix? curr-line "map_Kd ") (set! kd (list-ref (string-split (string-normalize-spaces curr-line)) 1))]
                  [(string-prefix? curr-line "map_Ks ") (set! ks (list-ref (string-split (string-normalize-spaces curr-line)) 1))])))))

    (cond [curr-name
           (set! mtls (vector-append mtls (vector (mtl curr-name ka kd ks))))])

    mtls)            

  (with-input-from-file obj-path
    (λ ()
      (for ([curr-line (in-lines)])
        (cond [(string-prefix? curr-line "mtllib ")
               (set! mtls (vector-append mtls (get-mtls (list-ref (string-split (string-normalize-spaces curr-line)) 1))))]
              [(string-prefix? curr-line "g ")
               (cond [curr-group
                      (vector-append groups #(curr-group))])
               (set! curr-group (group #() #f))]
              [(string-prefix? curr-line "usemtl")
               (set-group-mtl! curr-group (list-ref (string-split (string-normalize-spaces curr-line)) 1))]
              [(string-prefix? curr-line "v ")
               (set! positions (vector-append positions (vector (get-float3 curr-line))))]
              [(string-prefix? curr-line "vn ")
               (set! normals (vector-append normals (vector (get-float3 curr-line))))]
              [(string-prefix? curr-line "vt ")
               (set! tex-coords (vector-append tex-coords (vector (get-float3 curr-line))))]
              [(string-prefix? curr-line "f ")
               (set-group-faces! curr-group (vector-append (group-faces curr-group) (get-tri-faces curr-line)))]))))

  (set! groups (vector-append groups (vector curr-group)))

  (obj-model mtls positions normals tex-coords groups))

; Shaders
;================================================================================

(define vertex-shader-source #<<GLSL
#version 330 core

layout(location=0) in vec3 pos;
layout(location=1) in vec3 texCoord;

uniform mat4 mvp;

out vec2 vTexCoord;

void main() {
    gl_Position = mvp * vec4(pos, 1.0f);
    vTexCoord = texCoord.xy;
}
GLSL
)

(define fragment-shader-source #<<GLSL
#version 330 core

layout(location=0) out vec4 color;

in vec2 vTexCoord;

uniform bool hasTexture;
uniform sampler2D tex;

void main() {
    if( hasTexture )
        color = texture(tex, vTexCoord);
   else
        color = vec4(1.0, 0.0f, 0.0f, 1.0f);
}
GLSL
)

(define (compile-shader source shader-type)
  (define shader (glCreateShader shader-type))
  (define vertex-source-bytes (string->bytes/utf-8 source))
  (glShaderSource shader 1 (vector vertex-source-bytes) (s32vector (bytes-length vertex-source-bytes)))
  (glCompileShader shader)

  ; Check for errors
  (if (= (glGetShaderiv shader GL_COMPILE_STATUS) 0)
      (begin
        (let ([log-length (glGetShaderiv shader GL_INFO_LOG_LENGTH)])
          (let-values (([actual-length log-buf] (glGetShaderInfoLog shader log-length)))
            (writeln (bytes->string/utf-8 log-buf #\? 0 actual-length))))
        (raise 'shader-comp-failed))
      #t)
  shader)



(define opengl-canvas%
  (class canvas%
    (inherit with-gl-context
             swap-gl-buffers
             refresh
             accept-drop-files)

    (define gl-config (new gl-config%))

    (super-new [style '(gl no-autoclear)]
               [gl-config gl-config])

    (define program
      (with-gl-context
          (λ ()
            (define vertex-shader (compile-shader vertex-shader-source GL_VERTEX_SHADER))
            (define fragment-shader (compile-shader fragment-shader-source GL_FRAGMENT_SHADER))

            (define shader-program (glCreateProgram))
            (glAttachShader shader-program vertex-shader)
            (glAttachShader shader-program fragment-shader)
            (glLinkProgram shader-program)

            ; Check for errors
            (if (= (glGetProgramiv shader-program GL_LINK_STATUS) 0)
                (begin
                  (let ([log-length (glGetProgramiv shader-program GL_INFO_LOG_LENGTH)])
                    (let-values (([actual-length log-buf] (glGetProgramInfoLog shader-program log-length)))
                      (writeln (bytes->string/utf-8 log-buf #\? 0 actual-length))))
                  (raise 'shader-link-failed))
                #t)

            shader-program)))

    (struct renderable-mesh (vao num-vertices texture color))
    (define meshes-to-render '())

    (define (upload-obj-to-gpu obj-path)
      (with-gl-context
          (λ ()
            (set! meshes-to-render '())

            (define obj-to-upload (load-obj obj-path))

            (for ([g (obj-model-groups obj-to-upload)])
              (define vao (u32vector-ref (glGenVertexArrays 1) 0))
              (glBindVertexArray vao)
              
              (define positions '())
              (define tex-coords '())

              (define (float3->list x)
                (list (float3-x x) (float3-y x) (float3-z x)))

              (for ([f (group-faces g)])
                (let* ([v (face-vertices f)]
                       [t (face-tex-coords f)])
                  (match (face-vertices f)
                    [(vector v0 v1 v2)
                     (set! positions (append positions (float3->list (vector-ref (obj-model-positions obj-to-upload) (- v0 1)))))
                     (set! positions (append positions (float3->list (vector-ref (obj-model-positions obj-to-upload) (- v1 1)))))
                     (set! positions (append positions (float3->list (vector-ref (obj-model-positions obj-to-upload) (- v2 1)))))])
                  (match (face-tex-coords f)
                    [(vector t0 t1 t2)
                     (set! tex-coords (append tex-coords (float3->list (vector-ref (obj-model-tex-coords obj-to-upload) (- t0 1)))))
                     (set! tex-coords (append tex-coords (float3->list (vector-ref (obj-model-tex-coords obj-to-upload) (- t1 1)))))
                     (set! tex-coords (append tex-coords (float3->list (vector-ref (obj-model-tex-coords obj-to-upload) (- t2 1)))))]
                    [else #f])))

              (define pos-buffer (list->f32vector positions))
              (define pbo (u32vector-ref (glGenBuffers 1) 0))
              (glBindBuffer GL_ARRAY_BUFFER pbo)
              (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof pos-buffer) pos-buffer GL_STATIC_DRAW)

              (glVertexAttribPointer 0 3 GL_FLOAT #f (* 3 (ctype-sizeof _float)) 0)
              (glEnableVertexAttribArray 0)

              (cond [(not (empty? tex-coords))
                     (define tex-buffer (list->f32vector tex-coords))
                     (define tbo (u32vector-ref (glGenBuffers 1) 0))
                     (glBindBuffer GL_ARRAY_BUFFER tbo)
                     (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof tex-buffer) tex-buffer GL_STATIC_DRAW)
                     
                     (glVertexAttribPointer 1 3 GL_FLOAT #f (* 3 (ctype-sizeof _float)) 0)
                     (glEnableVertexAttribArray 1)])

              (define (argb->rgba pixels)
                (define new-pixels (make-bytes (bytes-length pixels)))
                (for ([i (in-range 0 (- (bytes-length pixels) 4))])
                  (let ([a (bytes-ref pixels i)]
                        [r (bytes-ref pixels (+ i 1))]
                        [g (bytes-ref pixels (+ i 2))]
                        [b (bytes-ref pixels (+ i 3))])
                    (bytes-set! new-pixels i r)
                    (bytes-set! new-pixels (+ i 1) g)
                    (bytes-set! new-pixels (+ i 2) b)
                    (bytes-set! new-pixels (+ i 3) a)))
                new-pixels)

              (define (get-mtl m-name)
                (define mtl #f)
                (for ([m (obj-model-mtls obj-to-upload)])
                  (if (string=? (mtl-name m) m-name)
                      (set! mtl m) #f))
                mtl)

              (define-values (obj-dir obj-filename -obj-is-dir) (split-path obj-path))
              
              (define tex
                (if (group-mtl g)
                    (let* ([m (get-mtl (group-mtl g))]
                           [tex-filename (mtl-kd m)]
                           [tex-path (build-path obj-dir tex-filename)]
                          [diffuse-bitmap (read-bitmap tex-path)]
                          [texture (u32vector-ref (glGenTextures 1) 0)])
                      (glBindTexture GL_TEXTURE_2D texture)
                      
                      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
                      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)

                      (let ([w (send diffuse-bitmap get-width)]
                            [h (send diffuse-bitmap get-height)])
                        (define pixel-bytes (make-bytes (* 4 w h)))
                        (send diffuse-bitmap get-argb-pixels 0 0 w h pixel-bytes)
                        (define pixel-vec (list->u8vector (bytes->list (argb->rgba pixel-bytes))))
                
                        (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE pixel-vec)
                        (glGenerateMipmap GL_TEXTURE_2D))

                      texture)
                    #f))

              (set! meshes-to-render (append meshes-to-render (list (renderable-mesh vao (length positions) tex #f))))))))
    
    (define/override (on-drop-file pathname)
      (upload-obj-to-gpu pathname)
      (super on-drop-file pathname))

    (accept-drop-files #t)

    (define last-frame-time (current-inexact-monotonic-milliseconds))

    (define last-mouse-x #f)
    (define last-mouse-y #f)
    (define last-mouse-motion #f)

    (define camera-phi 0.0)
    (define camera-theta 0.0)
    (define camera-distance 100.0)
    (define camera-rotate-sensitivity 0.001)
    (define camera-zoom-sensitivity 0.001)

    (define is-left-mouse-down #f)
    (define is-right-mouse-down #f)
    
    (define/override (on-event event)
      (cond
        [(is-a? event mouse-event%)
         (case (send event get-event-type)
           [(motion) (let ([mouse-x (send event get-x)]
                           [mouse-y (send event get-y)]
                           [curr-time (current-inexact-monotonic-milliseconds)])
                       (cond
                         [last-mouse-motion
                          (let ([delta-x (- mouse-x last-mouse-x)]
                                [delta-y (- mouse-y last-mouse-y)]
                                [delta-t (- curr-time last-mouse-motion)])
                            (if is-left-mouse-down
                                (begin
                                  (set! camera-theta (- camera-theta (* delta-x camera-rotate-sensitivity delta-t)))
                                  (set! camera-phi (- camera-phi (* delta-y camera-rotate-sensitivity delta-t)))
                                  (cond [(> camera-phi (/ 3.14 2)) (set! camera-phi (/ 3.14 2))]
                                        [(< camera-phi (/ -3.14 2)) (set! camera-phi (/ -3.14 2))]))
                                  #f)
                            (if is-right-mouse-down
                                (set! camera-distance (+ camera-distance (* delta-y camera-zoom-sensitivity delta-t))) #f))])
                       
                       (set! last-mouse-x mouse-x)
                       (set! last-mouse-y mouse-y)
                       (set! last-mouse-motion curr-time))]
           [(left-down) (set! is-left-mouse-down #t)]
           [(left-up) (set! is-left-mouse-down #f)]
           [(right-down) (set! is-right-mouse-down #t)]
           [(right-up) (set! is-right-mouse-down #f)])]))

    (define proj-matrix (mat4f-perspective 70.0 (/ 800.0 600.0) 1.0 100.0))
    
    (define/override (on-paint)
      (with-gl-context
          (λ ()
            (define curr-frame-time (current-inexact-monotonic-milliseconds))
            (define frame-delta (- curr-frame-time last-frame-time))
            (set! last-frame-time curr-frame-time)

            (glEnable GL_DEPTH_TEST)
            (glClearColor 0.3 0.3 0.4 0.0)
            (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

            (glUseProgram program)

            (define camera-x (* camera-distance (cos camera-phi) (sin camera-theta)))
            (define camera-y (* (* -1.0 camera-distance) (sin camera-phi)))
            (define camera-z (* camera-distance (cos camera-phi) (cos camera-theta)))
            (define view-matrix (mat4f-lookat camera-x camera-y camera-z 0.0 0.0 0.0 0.0 1.0 0.0))
                        
            (define mvp-mat (mat4f-mat4f-* proj-matrix view-matrix))
            (define mvp-mat-loc (glGetUniformLocation program "mvp"))
            (glUniformMatrix4fv mvp-mat-loc 1 #f mvp-mat)

            (define has-tex-loc (glGetUniformLocation program "hasTexture"))

            (for ([m meshes-to-render])
              (cond [(renderable-mesh-texture m)
                     (glUniform1i has-tex-loc 1)
                     (glActiveTexture GL_TEXTURE0)
                     (glBindTexture GL_TEXTURE_2D (renderable-mesh-texture m))]
                    [else (glUniform1i has-tex-loc 0)])
              
              (glBindVertexArray (renderable-mesh-vao m))
              (glDrawArrays GL_TRIANGLES 0 (renderable-mesh-num-vertices m)))
            
            (swap-gl-buffers)))
      (queue-callback (λ () (refresh)) #f)
      )))

(define top-level-window (new frame% [label "Model Viewer"]))
(send top-level-window show #t)

(define canvas (new opengl-canvas% [parent top-level-window]
                    [min-width 800]
                    [min-height 600]))

