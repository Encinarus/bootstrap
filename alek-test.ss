#lang scheme/gui

(require (lib "world.ss" "htdp")
         (lib "prim.ss" "lang")
         lang/prim
         htdp/world
         (except-in htdp/testing test)
         (for-syntax scheme/base))

;(provide circle triangle rectangle ellipse line text)

; A coord is a (make-coord Number Number)
(define-struct coord [x y])

; An Entity is a (make-entity Image Number)
; Broken out as a separate type so the kids can use that rather than the more complicated Being
(define-struct entity [image size])

; A Bullet is a (make-bullet Image Number Number String)
(define-struct bullet [image x y type])

; A being is a (make-being Entity Coord)
(define-struct being [entity coord])

; Utility functions for getting at the values nested in being
(define being-x (compose coord-x being-coord))
(define being-y (compose coord-y being-coord))
(define being-image (compose entity-image being-entity))
(define being-size (compose entity-size being-entity))

; A World is a (make-world Being* Being* Being Image Integer)
(define-struct world [objects targets player background score title timer])

; make-targets and make-objects are really the same as calling list
; just to simplify for the kids
(define make-targets list)
(define make-objects list)

(define (draw-being being background)
  (let* ((image (entity-image (being-entity being)))
         (x (coord-x (being-coord being)))
         (y (coord-y (being-coord being))))
    (place-image image x y background)))

(define (draw-all beings background)
    (if (empty? beings)
        background
        (draw-being (car beings) (draw-all (cdr beings) background))))

; draw-world : Image (list being*) (list being*) being -> Image
(define (draw-world bg objects targets player title score)
  (let* ((score-string (string-append title "    score:" (number->string score)))
         (target-layer (draw-all targets bg))
         (object-layer (draw-all objects target-layer))
         (player-layer (draw-being player object-layer)))
    (place-image (text score-string 20 "white") 10 0 player-layer)))

(define (stub-player player timer)
  (if (> timer 0)
      (make-being (make-entity (star 20 10 (+ timer 1) "solid" 
                                     (if (even? timer ) "black" "white"))
                               (being-size player))
                  (being-coord player))
      player))

(define (redraw-world w)
  (let* ((player (stub-player (world-player w) (world-timer w))))
    (draw-world (world-background w)
                (world-objects w)
                (world-targets w)
                player
                (world-title w)
                (world-score w))))
  
(define (bg image) (put-pinhole image 0 0))

(define (random-object objects)
  (car objects))

(define (move-all beings update-function)
  (let* ((update-being* (lambda (o) (let* ((loc (update-function (being-x o) (being-y o)))
                                           (ent (being-entity o)))
                                      (if (offscreen? (coord-x loc) (coord-y loc))
                                          (make-being ent (make-coord (+ 600 (object-spacing)) (random 480)))
                                          (make-being ent loc)))))
         (next (update-being* (car beings))))
    (map update-being* beings)))

(define (object-spacing) (+ (random 150) 40))

(define (convert-entities entities start-x)
  (if (empty? entities)
      entities
      (cons (make-being (car entities) (make-coord start-x (random 480))) 
            (convert-entities (cdr entities) (+ start-x (object-spacing))))))

(define (move-player w player-coord)
  (make-world (world-objects w)
              (world-targets w)
              (make-being (being-entity (world-player w)) player-coord)
              (world-background w)
              (world-score w)
              (world-title w)
              (world-timer w)))

;; char->string : char -> String
(define (char->string c)
  (cond [(not (char? c)) (raise-type-error 'char->string "character" c)]
        [(eq? c #\space) "space"]
        [else (string c)]))

(define (keypress w key update-player)
  (cond
    [(symbol? key) (keypress w (symbol->string key) update-player)]
    [(char? key)   (keypress w (char->string key)   update-player)]
    [(not (string? key)) w]
    [(or (string=? key "up") (string=? key "down") (string=? key "left") (string=? key "right"))
     (move-player w (update-player (being-x (world-player w)) (being-y (world-player w)) key))]
    [else w]))

; any-collide? : Being (list Being) -> Boolean
; Returns true if any of the objects collide with the being
(define (any-collide? collide? player objects)
  (if (empty? objects)
      false
      (or (collide? (being-x player) (being-y player) (being-size player)
                    (being-x (car objects)) (being-y (car objects)) (being-size (car objects)))
          (any-collide? collide? player (cdr objects)))))

(define (wrap-collide collide?)
  (if (= (procedure-arity collide?) 4)
      (lambda (px py psize cx cy csize)
        (collide? px py cx cy))
      collide?))

(define (window title objects targets player background
                collide? update-player update-object update-target)
  (let* ((world (make-world (convert-entities objects (+ 600 (object-spacing)))
                            (convert-entities targets (+ 600 (object-spacing)))
                            (make-being player (make-coord 320 300))
                            (bg background)
                            100
                            title
                            0))
         (collide* (wrap-collide collide?))
         (keypress* (lambda (w k) (keypress w k update-player)))
         (update-world (lambda (w) 
                         (let* ((objects (move-all (world-objects w) update-object))
                                (targets (move-all (world-targets w) update-target))
                                (score (world-score w))
                                (player (world-player w))
                                (bg (world-background w))
                                (title (world-title w))
                                (timer (world-timer w)))
                           (cond
                             [(> timer 0)
                              (make-world objects targets player bg score title (- timer 11))]
                             [(any-collide? collide* player objects)
                              (make-world objects targets player bg (- score 50) title 155)]
                             [(any-collide? collide* player targets)
                              (make-world objects targets player bg (+ score 20) title 155)]
                             [else (make-world objects targets player bg score title timer)])
                           ))))
    (begin
      (big-bang 640 480 .1 world true)
      (on-redraw redraw-world)
      (on-tick-event update-world)
      (on-key-event keypress*))))

; no-move: Number String -> Number
; Returns the same number, basically a placeholder
(define (no-move ord dir) ord)

; wrap-updateplayer : function(Number Number String) function(Number Number String) -> function(Number Number String)
; Wraps the update-player-x and update-player-y in a function that handles both, may be easier for the kids
(define (wrap-updateplayer update-player-x update-player-y)
  (lambda (x y dir)
    (cond
      [(or (string=? dir "up") (string=? dir "down"))
       (make-coord x (update-player-y y dir))]
      [(or (string=? dir "left") (string=? dir "right"))
       (make-coord (update-player-x x dir) y)]
      [else (make-coord x y)])))

(define (wrap-updater updater)
  (if (> 1 (procedure-arity updater))
      updater
      (lambda (x y)
        (make-coord (updater x) y))))

; Compatibility layer for the students to still run their old games
(define (start title background update-target update-player update-object collide? target player object off-the-edge?)
  (window title
          (make-objects (make-entity object 20))
          (make-objects (make-entity target 20))
          (make-entity player 20)
          background
          collide?
          (wrap-updateplayer no-move update-player-y)
          (wrap-updater update-object)
          (wrap-updater update-target)))

; Student functions
(define (update-bullet x y type)
  (cond
    [(string=? type "fast")
     (make-coord (+ x 10) y)]
    [(string=? type "slow")
     (make-coord (+ x 5) y)]
    ))

(define ammo (ellipse 10 20 "solid" "orange"))

(define (shoot x y button)
  (cond
    [(string=? button "a")
     (make-bullet ammo x y "slow")]
    [(string=? button "s")
     (make-bullet ammo x y "fast")]
    [else (make-bullet ammo 10000 10000 "none")]))

(define (update-player x y dir)
  (cond
    [(string=? dir "up") (make-coord x (- y 10))]
    [(string=? dir "down") (make-coord x (+ y 10))]
    [(string=? dir "left") (make-coord (- x 10) y)]
    [(string=? dir "right") (make-coord (+ x 10) y)]
    [else (make-coord x y)]))

(define (update-player2 x y dir)
  (cond
    [(or (string=? dir "up") (string=? dir "down"))
     (make-coord x (update-player-y y dir))]
    [(or (string=? dir "left") (string=? dir "right"))
     (make-coord (update-player-x x dir) y)]
    [else (make-coord x y)]))

(define (update-player-y y dir)
  (cond
    [(string=? dir "up") (- y 20)]
    [(string=? dir "down") (+ y 20)]
    [else y]))

(define (update-player-x x dir)
  (cond
    [(string=? dir "left") (- x 20)]
    [(string=? dir "right") (+ x 20)]
    [else x]))

(define (distance px py cx cy)
  (sqrt (+ (expt (- px cx) 2)
           (expt (- py cy) 2))))

(define (collide? px py psize cx cy csize)
  (> (+ psize csize) (distance px py cx cy)))

(define (old-collide? px py cx cy)
  (> 100 (distance px py cx cy)))

; update-target : Number Number -> Coord
; Moves the target left in a saw tooth wave... sine wave next?
(define (update-target x y)
  (make-coord (- x 10) 
              (if (= (modulo (floor (/ x 150)) 2) 0)
                  (- y 5)
                  (+ y 5))))

; update-object: Number Number -> Coord
; Moves the object left
(define (update-object x y)
  (make-coord (- x 25) y))

; offscreen? : Number Number -> Boolean
; Determines if the object is off the screen, ignores right (starting) edge
(define (offscreen? x y)
  (or (< x 0) (< y 0) (> y 480)))

(define objects (make-objects (make-entity (circle 20 "solid" "green") 20) (make-entity (circle 20 "solid" "purple") 20)))
(define targets (make-targets (make-entity (triangle 30 "solid" "red") 15) (make-entity (triangle 30 "solid" "blue") 15)))
(define player (make-entity (rectangle 30 30 "solid" "gray") (/ (sqrt 1800) 2)))
(define backdrop (rectangle 640 480 "solid" "black"))


(define (old-update-target x)
  (- x 50))

(define (old-update-object x)
  (- x 30))

;(window "Student Game" objects targets player backdrop collide? update-player update-object update-target)
(start "Student Game" backdrop old-update-target update-player old-update-object collide? (triangle 30 "solid" "red") (rectangle 30 30 "solid" "gray") (circle 20 "solid" "green") offscreen?)
