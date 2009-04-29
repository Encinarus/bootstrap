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
(define-struct world [objects targets player background score title timer bullet])

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
(define (draw-world bg objects targets player title score bullet)
  (let* ((score-string (string-append title "    score:" (number->string score)))
         (target-layer (draw-all targets bg))
         (object-layer (draw-all objects target-layer))
         (bullet-layer (place-image (bullet-image bullet) (bullet-x bullet) (bullet-y bullet) object-layer))
         (player-layer (draw-being player bullet-layer)))
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
                (world-score w)
                (world-bullet w))))
  
(define (bg image) (put-pinhole image 0 0))

(define (random-object objects)
  (car objects))

(define (move-all beings update-function offscreen?)
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
              (world-timer w)
              (world-bullet w)))

;; char->string : char -> String
(define (char->string c)
  (cond [(not (char? c)) (raise-type-error 'char->string "character" c)]
        [(eq? c #\space) "space"]
        [else (string c)]))

(define (keypress w key update-player shoot offscreen?)
  (let* ((player (world-player w))
         (bullet (world-bullet w)))
    (cond
      [(symbol? key) (keypress w (symbol->string key) update-player shoot offscreen?)]
      [(char? key)   
       (if (offscreen? (bullet-x bullet) (bullet-y bullet))
           (make-world (world-objects w)
                       (world-targets w)
                       (world-player w)
                       (world-background w)
                       (world-score w)
                       (world-title w)
                       (world-timer w)
                       (shoot (being-x player) (being-y player) (char->string key)))
           w)]
      [(not (string? key)) w]
      [(or (string=? key "up") (string=? key "down") (string=? key "left") (string=? key "right"))
       (move-player w (update-player (being-x (world-player w)) (being-y (world-player w)) key))]
      [else w])))

; any-collide? : Being (list Being) -> Boolean
; Returns true if any of the objects collide with the being
(define (any-collide? collide? player objects)
  (if (empty? objects)
      false
      (or (collide? (being-x player) (being-y player) (being-size player)
                    (being-x (car objects)) (being-y (car objects)) (being-size (car objects)))
          (any-collide? collide? player (cdr objects)))))

; Hide the objects on collision by moving them way off the screen
(define (remove-collisions collide? bullet objects)
  (let* ((move-on-hit (lambda (obj) (if (collide? (bullet-x bullet) (bullet-y bullet) 10
                                                  (being-x obj) (being-y obj) (being-size obj))
                                        (make-being (being-entity obj) (make-coord -1000 0))
                                        obj))))
    (map move-on-hit objects)))

(define no-bullet (make-bullet (circle 1 "solid" "black") -1000 -1000 "none"))

(define (move-bullet bullet offscreen? update-bullet)
  (let* ((pos (update-bullet (bullet-x bullet)
                             (bullet-y bullet)
                             (bullet-type bullet))))
    (if (offscreen? (bullet-x bullet) (bullet-y bullet))
        no-bullet
        (make-bullet (bullet-image bullet)
                     (coord-x pos)
                     (coord-y pos)
                     (bullet-type bullet)))))

(define (window title objects targets player background
                collide? update-player update-object update-target offscreen? update-bullet shoot)
  (let* ((world (make-world (convert-entities objects (+ 600 (object-spacing)))
                            (convert-entities targets (+ 600 (object-spacing)))
                            (make-being player (make-coord 320 300))
                            (bg background)
                            100
                            title
                            0
                            no-bullet))
         (collide* (wrap-collide collide?))
         (keypress* (lambda (w k) (keypress w k update-player shoot offscreen?)))
         (update-bullet* (lambda (bullet)
                           (if (= (bullet-x bullet) (bullet-x no-bullet))
                               no-bullet
                               (move-bullet bullet offscreen? update-bullet))))
         (update-world (lambda (w) 
                         (let* ((bullet (update-bullet* (world-bullet w)))
                                (objects (remove-collisions collide? bullet
                                                            (move-all (world-objects w) update-object offscreen?)))
                                (targets (move-all (world-targets w) update-target offscreen?))
                                (score (world-score w))
                                (player (world-player w))
                                (bg (world-background w))
                                (title (world-title w))
                                (timer (world-timer w)))
                           (cond
                             [(> timer 0)
                              (make-world objects targets player bg score title (- timer 11) bullet)]
                             [(any-collide? collide* player objects)
                              (make-world objects targets player bg (- score 50) title 155 bullet)]
                             [(any-collide? collide* player targets)
                              (make-world objects targets player bg (+ score 20) title 155 bullet)]
                             [else (make-world objects targets player bg score title timer bullet)])
                           ))))
    (begin
      (big-bang 640 480 .1 world true)
      (on-redraw redraw-world)
      (on-tick-event update-world)
      (on-key-event keypress*))))

; no-move: Number String -> Number
; Returns the same number, basically a placeholder
(define (no-move ord dir) ord)

; no-bullet: Number Number String -> Coord
; Returns the same coordinate, basically doesn't update
(define (no-bullet-update x y type)
  (make-coord x y))

; no-shoot: Number Number String -> Bullet
; Returns a bullet that is off the screen.
(define (no-shoot x y key) no-bullet)

; Wraps the collide function, detecting if it's a new or old style function
(define (wrap-collide collide?)
  (if (= (procedure-arity collide?) 4)
      (lambda (px py psize cx cy csize)
        (collide? px py cx cy))
      collide?))

; simple-updateplayer : function(Number Number String) function(Number Number String) -> function(Number Number String)
; Wraps the update-player-x and update-player-y in a function that handles both, may be easier for the kids to write
(define (simple-updateplayer update-player-x update-player-y)
  (lambda (x y dir)
    (cond
      [(or (string=? dir "up") (string=? dir "down"))
       (make-coord x (update-player-y y dir))]
      [(or (string=? dir "left") (string=? dir "right"))
       (make-coord (update-player-x x dir) y)]
      [else (make-coord x y)])))

; Wraps update-player if needed, converting from a simple 
(define (wrap-updateplayer update-player)
  (if (= (procedure-arity update-player) 2)
      (simple-updateplayer no-move update-player)
      update-player))

(define (wrap-updater updater)
  (if (> 1 (procedure-arity updater))
      updater
      (lambda (x y)
        (make-coord (updater x) y))))

; Compatibility layer for the students to still run their old games
(define (start title background update-target update-player update-object collide? target player object offscreen?)
  (window title
          (make-objects (make-entity object 20))
          (make-objects (make-entity target 20))
          (make-entity player 20)
          background
          collide?
          (wrap-updateplayer update-player-y)
          (wrap-updater update-object)
          (wrap-updater update-target)
          offscreen?
          no-bullet
          no-shoot))

; New Student functions
;   By using the wrapper functions they can mix and match what advanced features they want
;   They would start with the basic functions in the game and build to these features
;   It's unlikely that they'll get to implement all of the advanced features but these
;   seem like a good start for them. We offered them the chance to make a more advanced
;   game if they had time at the end.

(define objects (make-objects (make-entity (circle 20 "solid" "green") 20) (make-entity (circle 20 "solid" "purple") 20)))
(define targets (make-targets (make-entity (triangle 30 "solid" "red") 15) (make-entity (triangle 30 "solid" "blue") 15)))
(define player (make-entity (rectangle 30 30 "solid" "gray") (/ (sqrt 1800) 2)))
(define backdrop (rectangle 640 480 "solid" "black"))
(define ammo (circle 10 "solid" "orange"))

(define (update-bullet x y type)
  (cond
    [(string=? type "fast")
     (make-coord (+ x 45) y)]
    [(string=? type "slow")
     (make-coord (+ x 5) y)]
    [(string=? type "normal")
     (make-coord (+ x 30) y)]
    [(string=? type "up")
     (make-coord (+ x 30) (- y 20))]
    [(string=? type "down")
     (make-coord (+ x 30) (+ y 20))]
    ))

(define (shoot x y button)
  (cond
    [(string=? button "a")
     (make-bullet ammo (+ x 5) y "slow")]
    [(string=? button "s")
     (make-bullet ammo (+ x 5) y "fast")]
    [(string=? button "w")
     (make-bullet ammo (+ x 5) (- y 2) "up")]
    [(string=? button "x")
     (make-bullet ammo (+ x 5) (+ y 2) "down")]
    [else no-bullet]))

(define (update-player x y dir)
  (cond
    [(string=? dir "up") (make-coord x (- y 20))]
    [(string=? dir "down") (make-coord x (+ y 20))]
    [(string=? dir "left") (make-coord (- x 20) y)]
    [(string=? dir "right") (make-coord (+ x 20) y)]
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

(define (collide? px py psize cx cy csize)
  (> (+ psize csize) (distance px py cx cy)))

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


; Old student functions
; These functions are what the students are expected to write for the basic game

; offscreen? : Number Number -> Boolean
; Determines if the object is off the screen, ignores right (starting) edge
(define (offscreen? x y)
  (or (< x 0) (< y 0) (> y 480) (> x 640)))

(define (old-collide? px py cx cy)
  (> 100 (distance px py cx cy)))

(define (distance px py cx cy)
  (sqrt (+ (expt (- px cx) 2)
           (expt (- py cy) 2))))

(define (old-update-target x)
  (- x 50))

(define (old-update-object x)
  (- x 30))

(define (old-update-player y dir)
  (cond
    [(string=? dir "up") (- y 20)]
    [(string=? dir "down") (+ y 20)]
    [else y]))

; The below two lines show the difference between using the old version (start...)
; and using the new version (window...). Now start is just a wrapper for window,
; you can take a look in that function to see how they would wrap their functions
; if they wanted to use some but not all of the advanced features of the framework.
;(start "Student Game" backdrop old-update-target old-update-player old-update-object collide? (triangle 30 "solid" "red") (rectangle 30 30 "solid" "gray") (circle 20 "solid" "green") offscreen?)
(window "Student Game" objects targets player backdrop collide? update-player update-object update-target offscreen? update-bullet shoot)
