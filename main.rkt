#lang racket/base

(require pict3d
         (only-in pict3d/universe big-bang3d)
         racket/stream
         racket/match
         racket/local
         racket/list
         "real-modulo.rkt"
         "parametric-cylinder.rkt"
         "my-point-at.rkt"
         )
(module+ test
  (require "testing.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; a World-State is a (world-state Dir Angle (Streamof Piece))
;; interp. a struct with a dir, an angle, and an infinite lazy stream of pieces
;; the dir represents the direction the tube and camera are pointing reletive to the earth.
;; the angle represents the angle of the camera relative to the tube
;; the stream records the shape of the tube and positions of the obstacles

;; a Piece is (piece ∆θDir (Listof Obstacle))
;; a ∆θDir is a (∆θdir Angle Angle) ; angles in degrees
;; an Obstacle is an Angle

(struct world-state (dir angle stream) #:transparent)
(struct piece (∆θdir obstacles) #:transparent)
(struct ∆θdir (∆yθ ∆zθ) #:transparent)

(define make-pos pos)
(define make-dir dir)

;; dir+∆θdir : [Dir ∆θDir -> Dir]
(define (dir+∆θdir d ∆θd)
  (define-values (yθ zθ) (dir->angles d))
  (match-define (∆θdir ∆yθ ∆zθ) ∆θd)
  (angles->dir (+ yθ ∆yθ) (+ zθ ∆zθ)))

(module+ test
  (check-within (dir+∆θdir +x (∆θdir   0   0)) +x 1e-10)
  (check-within (dir+∆θdir +x (∆θdir  90   0)) +y 1e-10)
  (check-within (dir+∆θdir +x (∆θdir 180   0)) -x 1e-10)
  (check-within (dir+∆θdir +x (∆θdir -90   0)) -y 1e-10)
  (check-within (dir+∆θdir +x (∆θdir   0  90)) +z 1e-10)
  (check-within (dir+∆θdir +x (∆θdir   0 -90)) -z 1e-10)
  )

(define (normalize-angle a)
  (cond [(<= 0 a) (real-modulo a 360)]
        [else (- (real-modulo (- a) 360))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants

;; make-initial-world : [-> World-State]
(define (make-initial-world)
  (world-state +x 0 (make-world-stream)))

(define max-∆θdir-component 8) ; angle in degrees
(define max-∆∆θdir-component 4) ; angle in degrees

(define world-stream-counter-start 20)
(define world-stream-counter-reset 10)

(define pieces-to-render-at-a-time 40)

(define tube-segments 16)

(define earth
  (with-material
   (material #:ambient 0.1
             #:diffuse 0.8
             #:specular 0.1
             #:roughness 0.5)
   (with-color
    (rgba "darkgreen")
    (ellipsoid
     (pos -100 -100 -10)
     (pos  100  100  0)))))

(define sky
  (with-emitted
   (emitted "lightblue" 1)
   (sphere
    (pos 0 0 -5)
    100
    #:inside? #t)))

(define sun
  (combine
   (sunlight -z (emitted "oldlace" 1))
   (light (pos 0 0 50) (emitted "oldlace" 1000))
   (with-emitted
    (emitted "oldlace" 1)
    (sphere (pos 0 0 52) 1))))

(define earth-sky-sun
  (combine earth sky sun))

;; with-earth-etc : [Pict3d #:pos Pos #:dir Dir -> Pict3d]
(define (with-earth-etc pict3d #:pos pos #:dir dir)
  (combine
   earth-sky-sun
   (transform
    pict3d
    (my-point-at origin +x pos dir))))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions

;; main : [-> World-State]
(define (main)
  (big-bang3d (make-initial-world)
              #:on-frame tick
              #:stop-state? stop-state?
              #:on-draw render-world
              #:on-key handle-key
              ))

;; tick : [World-State N T -> World-State]
(define (tick ws n t)
  (match-define (world-state dir a s) ws)
  (define ∆θdir (piece-∆θdir (stream-first s)))
  (define new-dir (dir+∆θdir dir ∆θdir))
  (struct-copy world-state ws
               [dir new-dir]
               [stream (stream-rest s)]))

;; stop-state? : [World-State N T -> Boolean]
(define (stop-state? ws n t)
  (match-define (world-state _ a s) ws)
  (cond [(trace (combine (get-tube+obstacles s +x 1))
                (transform-pos (make-pos -1 0 1) (rotate-x a))
                +x)
         #t]
        [else
         #f]))

;; render-world : [World-State N T -> Image]
(define (render-world ws n t)
  (match-define (world-state dir a s) ws)
  (with-earth-etc
   #:pos (pos 0 0 30) #:dir dir
   (combine (rotate-x (basis 'camera (point-at (make-pos 0 0 1) +x))
                      a)
            (get-tube+obstacles s))))

(define (handle-key ws n t k)
  (match k
    ["left" (rotate-world-camera ws -10)]
    ["right" (rotate-world-camera ws 10)]
    [_ ws]))

(define (rotate-world-camera ws ∆a)
  (struct-copy world-state ws
               [angle (normalize-angle (+ (world-state-angle ws) ∆a))]))
  

;; get-tube+obstacles : Stream [Dir] [Natural] -> (Treeof Pict3d)
(define (get-tube+obstacles s [dir +x] [n pieces-to-render-at-a-time])
  (define (loop #:n n
                #:s s
                #:pict3ds pict3ds
                #:pos pos
                #:dir dir)
    (cond
      [(zero? n) pict3ds]
      [else
       (match-define (piece ∆θdir obstacles) (stream-first s))
       (define new-dir (dir+∆θdir dir ∆θdir))
       (define new-pos (pos+ pos new-dir))
       (define cylinder
         (with-color
          (rgba "blue")
          (parametric-cylinder
           #:samples 1 #:segments tube-segments
           (match-lambda
             [0.0 (values pos dir (dir- new-dir dir) 1/2)]
             [1.0 (values new-pos new-dir (dir- new-dir dir) 1/2)])
           0.0 1.0)))
       (define unpositioned-obstacles
         (with-color
          (rgba "gray")
          (for/list ([angle (in-list obstacles)])
            (rotate-x (rectangle (make-pos 1/5 -1/5 0)
                                 (make-pos 3/5  1/5 3))
                      angle))))
       (define new-pict3d
         (combine cylinder
                  (transform (combine unpositioned-obstacles)
                             (my-point-at origin +x pos dir))))
       (loop #:n (sub1 n)
             #:s (stream-rest s)
             #:pict3ds (cons new-pict3d pict3ds)
             #:pos new-pos
             #:dir new-dir)]))
  (loop #:n n
        #:s s
        #:pict3ds '()
        #:pos (pos 0 0 0)
        #:dir dir))



(define (make-world-stream #:counter [i world-stream-counter-start])
  (if (zero? i)
      (stream-cons (piece (random-∆θdir) (random-obstacles))
                   (make-world-stream #:counter world-stream-counter-reset))
      (stream-cons (piece (random-∆θdir) '())
                   (make-world-stream #:counter (sub1 i)))))




;; random-∆θdir : [-> ∆θDir]
(define random-∆θdir
  (local [(define (random-∆θdir)
            (∆θdir (random-∆yθ) (random-∆zθ)))
          (define ∆yθ 0)
          (define ∆zθ 0)
          (define (random-∆yθ)
            (define new-∆yθ
              (constrain
               #:min (- max-∆θdir-component) #:max max-∆θdir-component
               (+ ∆yθ (* max-∆∆θdir-component (+/-random)))))
            (set! ∆yθ new-∆yθ)
            new-∆yθ)
          (define (random-∆zθ)
            (define new-∆zθ
              (constrain
               #:min (- max-∆θdir-component) #:max max-∆θdir-component
               (+ ∆zθ (* max-∆∆θdir-component (+/-random)))))
            (set! ∆zθ new-∆zθ)
            new-∆zθ)
          (define (+/-)
            (sub1 (* 2 (random 2))))
          (define (+/-random)
            (* (+/-) (random)))]
    random-∆θdir))

;; constrain : Real #:min Real #:max Real -> Real
(define (constrain x #:min mn #:max mx)
  (min (max x mn) mx))


;; random-obstacles : [-> (Listof Obstacle)]
(define (random-obstacles)
  (define o (random 90))
  (for/list ([i (in-range 4)])
    (+ o (* 90 i)))
  ;(for/list ([i (in-range 5)])
  ;  (random 360))
  )



(main)
