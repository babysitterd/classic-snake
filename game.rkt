#lang racket

(require 2htdp/image 2htdp/universe)

;; sprite size
(define N 40)
;; scale factor
(define X 1)

;; board size in squares
(define width 17)
(define height 15)

(define tick-rate 1/7)

;; loading assets, thank you @Clear_code from opengameart.org
;; head
(define sprite-head-up (bitmap/file "assets/head_up.png"))
(define sprite-head-left (bitmap/file "assets/head_left.png"))
(define sprite-head-down (bitmap/file "assets/head_down.png"))
(define sprite-head-right (bitmap/file "assets/head_right.png"))
;; body
(define sprite-body-vertical (bitmap/file "assets/body_vertical.png"))
(define sprite-body-horizontal (bitmap/file "assets/body_horizontal.png"))
(define sprite-body-bottomleft (bitmap/file "assets/body_bottomleft.png"))
(define sprite-body-bottomright (bitmap/file "assets/body_bottomright.png"))
(define sprite-body-topleft (bitmap/file "assets/body_topleft.png"))
(define sprite-body-topright (bitmap/file "assets/body_topright.png"))
;; tail
(define sprite-tail-up (bitmap/file "assets/tail_up.png"))
(define sprite-tail-left (bitmap/file "assets/tail_left.png"))
(define sprite-tail-down (bitmap/file "assets/tail_down.png"))
(define sprite-tail-right (bitmap/file "assets/tail_right.png"))
;; other
(define sprite-apple (bitmap/file "assets/apple.png"))

;; square number to pixel position
(define (to-pos square)
  (+ (* square N) (/ N 2)))

;; generating chess board style grass field
(define (flip-grass-factory init)
  (let ([light init])
    (λ (x)
      (set! light (not light))
      (if light
          (rectangle N N 'solid (make-color #xaa #xd7 #x51))
          (rectangle N N 'solid (make-color #xa2 #xd1 #x49))))))

(define light-grass-line (apply beside (build-list width (flip-grass-factory #f))))
(define dark-grass-line (apply beside (build-list width (flip-grass-factory #t))))

(define empty-board (apply above
       (for/list ([j height])
         (if (even? j)
             light-grass-line
             dark-grass-line))))

(struct posn (x y) #:transparent)

(define (posn=? lhs rhs)
  (and (= (posn-x lhs) (posn-x rhs)) (= (posn-y lhs) (posn-y rhs))))

(struct snake (dir turn sections) #:transparent)

(struct world (snake apple score) #:transparent)

(define (draw-head s scene)
  (define dir (snake-dir s))
  (define sprite-head
    (cond [(eq? dir 'up)    sprite-head-up]
          [(eq? dir 'down)  sprite-head-down]
          [(eq? dir 'left)  sprite-head-left]
          [(eq? dir 'right) sprite-head-right]))
  (define head-posn (first (snake-sections s)))
  (place-image sprite-head
               (to-pos (posn-x head-posn))
               (to-pos (posn-y head-posn))
               scene))

(define (draw-segments segs scene)
  (define len (length segs))
  (cond [(>= len 3) (draw-segments (rest segs) (draw-body (first segs) (second segs) (third segs) scene))]
        [(= len 2) (draw-tail (first segs) (second segs) scene)]
        [else scene]))

(define (draw-body prev cur next scene)
  (define sprite-body-dir
    (cond [(and (= (posn-x prev) (posn-x cur)) (= (posn-x cur) (posn-x next))) sprite-body-vertical]
          [(and (= (posn-y prev) (posn-y cur)) (= (posn-y cur) (posn-y next))) sprite-body-horizontal]

          [(and (< (posn-x prev) (posn-x cur)) (< (posn-y cur) (posn-y next))) sprite-body-bottomleft]
          [(and (> (posn-y prev) (posn-y cur)) (> (posn-x cur) (posn-x next))) sprite-body-bottomleft]
          
          [(and (> (posn-y prev) (posn-y cur)) (< (posn-x cur) (posn-x next))) sprite-body-bottomright]
          [(and (> (posn-x prev) (posn-x cur)) (< (posn-y cur) (posn-y next))) sprite-body-bottomright]

          [(and (< (posn-y prev) (posn-y cur)) (< (posn-x cur) (posn-x next))) sprite-body-topright]
          [(and (> (posn-x prev) (posn-x cur)) (> (posn-y cur) (posn-y next))) sprite-body-topright]

          [(and (< (posn-y prev) (posn-y cur)) (> (posn-x cur) (posn-x next))) sprite-body-topleft]
          [(and (< (posn-x prev) (posn-x cur)) (> (posn-y cur) (posn-y next))) sprite-body-topleft]))
  (place-image sprite-body-dir
               (to-pos (posn-x cur))
               (to-pos (posn-y cur))
               scene))

(define (draw-tail prev tail scene)
  (define sprite-tail
    (cond [(and (= (posn-x tail) (posn-x prev)) (> (posn-y tail) (posn-y prev))) sprite-tail-down]
          [(and (= (posn-x tail) (posn-x prev)) (< (posn-y tail) (posn-y prev))) sprite-tail-up]
          [(and (> (posn-x tail) (posn-x prev)) (= (posn-y tail) (posn-y prev))) sprite-tail-right]
          [(and (< (posn-x tail) (posn-x prev)) (= (posn-y tail) (posn-y prev))) sprite-tail-left]))
  (place-image sprite-tail
               (to-pos (posn-x tail))
               (to-pos (posn-y tail))
               scene))

(define (draw-snake s scene)
  (draw-segments (snake-sections s) (draw-head s scene)))

(define (draw-apple pos scene)
  (place-image sprite-apple
               (to-pos (posn-x pos))
               (to-pos (posn-y pos))
               scene))

(define (overlay-score w scene)
  (overlay/align "left" "top" (text (format "Score: ~a" (world-score w)) (quotient N 2) "black") scene))

(define (move pos dir)
  (cond [(eq? dir 'up)    (posn (posn-x pos) (sub1 (posn-y pos)))]
        [(eq? dir 'down)  (posn (posn-x pos) (add1 (posn-y pos)))]
        [(eq? dir 'left)  (posn (sub1 (posn-x pos)) (posn-y pos))]
        [(eq? dir 'right) (posn (add1 (posn-x pos)) (posn-y pos))]))

(define (next-step w)
  (define s (world-snake w))
  (define a (world-apple w))
  (define score (world-score w))
  (define turn (snake-turn s))
  (define dir (if turn turn (snake-dir s)))
  (define sec (snake-sections s))
  (define head-posn (first sec))
  (define new-pos (move head-posn (if turn turn dir)))
  (let ([new-world
    ; collision detection, eat and grow or just move
    (if (posn=? new-pos a)
        (let ([new-snake (snake dir #f (cons new-pos sec))])
          (world new-snake (new-apple-pos new-snake) (add1 score)))
        (world (snake dir #f (cons new-pos (remove (last sec) sec))) a score))])
    ; return the world state right before the disaster
    (if (dead? new-world) (stop-with w) new-world)))

;; keyboard event handler
(define (direct-snake w e)
  (define s (world-snake w))
  (define dir (snake-dir s))
  (define head-posn (first (snake-sections s)))
  (define sec (snake-sections s))
  ; allow only one direction change per square to prevent 180 turns
  (if (snake-turn s)
      w
      (cond
        [(and (key=? e "left") (not (eq? dir 'right))) (struct-copy world w [snake (snake dir 'left sec)])]
        [(and (key=? e "right") (not (eq? dir 'left))) (struct-copy world w [snake (snake dir 'right sec)])]
        [(and (key=? e "up") (not (eq? dir 'down)))    (struct-copy world w [snake (snake dir 'up sec)])]
        [(and (key=? e "down") (not (eq? dir 'up)))    (struct-copy world w [snake (snake dir 'down sec)])]
        [else w])))

(define (render-game w)
  (scale X (overlay-score w (draw-apple (world-apple w) (draw-snake (world-snake w) empty-board)))))

;; game over if out of bounds or hit your own tail
(define (dead? w)
  (define lst (snake-sections (world-snake w)))
  (define head (first lst))
  (define tail (rest lst))
  (or (wall-colliding? head) (self-colliding? head tail)))

(define (wall-colliding? head)
  (define x (posn-x head))
  (define y (posn-y head))
  (or (negative? x) (negative? y)
      (>= x width) (>= y height)))

(define (self-colliding? head tail)
  (member head tail))

(define (game-over w)
  (overlay (text "Game Over" (* N 1.5) "black") (render-game w)))

(define (new-apple-pos s)
  (define candidate (posn (random width) (random height)))
  (if (member candidate (snake-sections s))
      (new-apple-pos s)
      candidate))

(define snake-start (snake 'right #f (list (posn 3 (quotient height 2)) (posn 2 (quotient height 2)))))

(big-bang (world snake-start (new-apple-pos snake-start) 0)
          (on-tick next-step tick-rate)
          (on-key direct-snake)
          (to-draw render-game)
          (stop-when dead? game-over)
          (name "classic snake"))
