#lang typed/racket

;; from http://lists.racket-lang.org/users/archive/2015-March/066259.html

(provide parametric-cylinder)

(require pict3d
         math/flonum)

(: parametric-cylinder (-> (-> Flonum (Values Pos Dir Dir Real))
                           Real
                           Real
                           [#:segments Nonnegative-Integer]
                           [#:samples Nonnegative-Integer]
                           [#:inside? Boolean]
                           Pict3D))
(define (parametric-cylinder f tmin tmax #:segments [m 32] #:samples [n 32] #:inside? [inside? #f])
  (define tstep (/ (- (fl tmax) (fl tmin)) (fl n)))
  (define θstep (/ (* 2 pi) (fl m)))
  
  ;; Compute all the verices first
  (define vss
    ;; For each sample...
    (for/list : (Listof (Listof Vertex)) ([i  (in-range (+ n 1))])
      (define t (+ tmin (* tstep (fl i))))
      (define-values (v dv ddv r) (f t))
      ;; Derive local coordinate axes from derivative and second derivative
      (define dz (dir-normalize dv))
      (cond
        [(not dz)  (error 'bent-cylinder "zero derivative at t = ~v~n" t)]
        [else
         (define dy (dir-normalize (dir-cross dz ddv)))
         (cond
           [(not dy)  (error 'bent-cylinder "zero or coincident second derivative at t = ~v~n" t)]
           [else
            (define dx (dir-cross dy dz))
            ;; Assemble the transformation to local coordinates
            (define tr (cols->affine dx dy dz origin))
            ;; For each angle...
            (for/list : (Listof Vertex) ([j  (in-range (+ m 1))])
              (define θ (* θstep (fl j)))
              (let* (;; Compute the direction to the vertex on the unit circle
                     [d  (dir (* (cos θ) r) (* (sin θ) r) 0)]
                     ;; Transform into local coordinates
                     [d  (transform-dir d tr)])
                (vertex (pos+ v d) #:normal d)))])])))
  
  (freeze
   (combine
    ;; Cylinder part
    (for/list : (Listof (Listof Pict3D)) ([vs0  (in-list vss)]
                                          [vs1  (in-list (rest vss))])
      (for/list : (Listof Pict3D) ([v00  (in-list vs0)]
                                   [v01  (in-list (rest vs0))]
                                   [v10  (in-list vs1)]
                                   [v11  (in-list (rest vs1))])
        (quad v00 v01 v11 v10 #:back? inside?)))
    ;; "Bottom" (tmin) cap
    (for/list : (Listof Pict3D) ([v0  (in-list (first vss))]
                                 [v1  (in-list (rest (first vss)))])
      (define-values (v2 dv _2 _3) (f (fl tmin)))
      (triangle (vertex v2 #:normal dv)
                (vertex (vertex-pos v1) #:normal dv)
                (vertex (vertex-pos v0) #:normal dv)
                #:back? inside?))
    ;; "Top" (tmax) cap
    (for/list : (Listof Pict3D) ([v0  (in-list (last vss))]
                                 [v1  (in-list (rest (last vss)))])
      (define-values (v2 dv _2 _3) (f (fl tmax)))
      (triangle (vertex (vertex-pos v0) #:normal dv)
                (vertex (vertex-pos v1) #:normal dv)
                (vertex v2 #:normal dv)
                #:back? inside?)))))

(module* test typed/racket
  (require (submod "..")
           pict3d)
  
  (: square (-> Flonum (Values Pos Dir Dir Real)))
  (define (square t)
    (values (pos t (sqr t) 0)
            (dir 1 (* 2 t) 0)
            (dir 0 2 0)
            (+ 0.25 (* 0.25 (abs (sin (* t 4)))))))
  
  (: helix (-> Flonum (Values Pos Dir Dir Real)))
  (define (helix t)
    (values (pos (cos t) (sin t) (/ t (* 2 pi)))
            (dir (- (sin t)) (cos t) (/ 1 (* 2 pi)))
            (dir (- (cos t)) (- (sin t)) 0)
            1/2))
  
  (parametric-cylinder square -1 1 #:inside? #t)
  (with-color (rgba "lightblue")
              (parametric-cylinder helix 0 (* 8 pi) #:segments 32 #:samples 128))
  )
