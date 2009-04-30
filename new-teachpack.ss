#lang scheme/gui

(require (lib "world.ss" "htdp")
         (lib "prim.ss" "lang")
         (lib "image.ss" "htdp")
         lang/prim
         lang/private/imageeq
         (except-in htdp/testing test)
         (for-syntax scheme/base))

(define-higher-order-primitive start wrap-window  (_ _ 
                                                     update-target 
                                                     update-player 
                                                     update-object 
                                                     collide?
                                                     _ _ _
                                                     offscreen?))

(define-higher-order-primitive advanced-start window (_ _
                                                        update-target
                                                        update-player
                                                        update-object
                                                        collide?
                                                        _ _ _
                                                        offscreen?
                                                        update-bullet
                                                        shoot))

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

; make-targets and make-objects wrap entity creation, can take either images or
; entities to simplify for the kids -- most often they'll just pass multiple images
; but if they want more accurate collision detection they can pass actual entities
(define (wrap-entity ent)
  (let* ((image->entity (lambda (i) (make-entity i 20))))
    (cond
      [(image? ent) (image->entity ent)]
      [(entity? ent) ent]
      [else (raise-type-error 'entity "Unknown type for wrapping" ent)])))

(define wrap-entities
  (lambda p (map wrap-entity p)))
  
(define make-targets wrap-entities)
(define make-objects wrap-entities)

(define (draw-being being background)
  (place-image (being-image being) (being-x being) (being-y being) background))

(define (draw-all beings background)
  (foldl draw-being background beings))

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
  (foldl (lambda (obj bool) (or (collide? (being-x player) (being-y player) (being-size player)
                                          (being-x obj) (being-y obj) (being-size obj)) bool))
         false
         objects))

; Hide the objects on collision by moving them way off the screen
(define (remove-collisions collide? bullet objects)
  (let* ((move-on-hit (lambda (obj) (if (collide? (bullet-x bullet) (bullet-y bullet) 15
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

(define (window title background update-target update-player update-object 
                collide? targets player objects offscreen? update-bullet shoot)
  (let* ((player-ent (wrap-entity player))
         (world (make-world (convert-entities objects (+ 600 (object-spacing)))
                            (convert-entities targets (+ 600 (object-spacing)))
                            (make-being player-ent (make-coord 320 300))
                            (bg background)
                            100
                            title
                            0
                            no-bullet))
         (update-target* (wrap-updater update-target))
         (update-player* (wrap-updateplayer update-player))
         (update-object* (wrap-updater update-object))
         (collide* (wrap-collide collide?))
         (keypress* (lambda (w k) (keypress w k update-player* shoot offscreen?)))
         (update-bullet* (lambda (bullet)
                           (if (= (bullet-x bullet) (bullet-x no-bullet))
                               no-bullet
                               (move-bullet bullet offscreen? update-bullet))))
         (update-world (lambda (w) 
                         (let* ((bullet (update-bullet* (world-bullet w)))
                                (objects (remove-collisions collide* bullet
                                                            (move-all (world-objects w) update-object* offscreen?)))
                                (targets (move-all (world-targets w) update-target* offscreen?))
                                (score (world-score w))
                                (player (world-player w))
                                (bg (world-background w))
                                (title (world-title w))
                                (timer (world-timer w)))
                           (cond
                             [(> timer 0)
                              (make-world objects targets player bg score title (- timer 11) bullet)]
                             [(any-collide? collide* player objects)
                              (begin (play-sound "sounds/crash.wav" true)
                                     (make-world objects targets player bg (- score 50) title 155 bullet))]
                             [(any-collide? collide* player targets)
                              (begin (play-sound "sounds/score.wav" true)
                                     (make-world objects targets player bg (+ score 20) title 155 bullet))]
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
  (if (= (procedure-arity updater) 2)
      updater
      (lambda (x y)
        (make-coord (updater x) y))))

; Compatibility layer for the students to still run their old games
(define (wrap-window title background update-target update-player-y update-object collide? target player object offscreen?)
  (window title
          background
          update-target
          update-player-y
          update-object
          collide?
          (make-targets target)
          (make-entity player 20)
          (make-objects object)
          offscreen?
          no-bullet
          no-shoot))

(provide start advanced-start
         make-coord make-entity make-bullet make-being
         coord-x coord-y entity-image entity-size
         bullet-image bullet-x bullet-y
         being-entity being-coord being-x being-y being-image being-size
         make-targets make-objects
         no-bullet no-bullet-update no-move no-shoot 
         simple-updateplayer
         circle triangle rectangle ellipse)

;; a `test' macro that is a synonym for `check-expect', catches expansion
;; errors and pretends that they come from `test'.
(require (for-syntax syntax/kerncase))
(define-syntax (TEST stx)
  (syntax-case stx ()
    [(_ x ...)
     (with-handlers ([exn? (lambda (e)
                             (raise (make-exn
                                     (regexp-replace*
                                      #rx"check-expect"
                                      (exn-message e)
                                      "test")
                                     (exn-continuation-marks e))))])
       (local-expand (syntax/loc stx (check-expect x ...))
                     (syntax-local-context)
                     (kernel-form-identifier-list)))]))

