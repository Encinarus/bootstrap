;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname game) (read-case-sensitive #t) (teachpacks ((lib "new-teachpack.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "new-teachpack.ss" "installed-teachpacks")))))
; New Student functions
;   By using the wrapper functions they can mix and match what advanced features they want
;   They would start with the basic functions in the game and build to these features
;   It's unlikely that they'll get to implement all of the advanced features but these
;   seem like a good start for them. We offered them the chance to make a more advanced
;   game if they had time at the end.

(define objects (make-objects (circle 20 "solid" "green")
                              (circle 20 "solid" "purple")
                              (circle 20 "solid" "white")
                              (circle 20 "solid" "brown")))
(define targets (make-targets (make-entity (triangle 30 "solid" "red") 15) (make-entity (triangle 30 "solid" "blue") 15)))
(define player (rectangle 30 30 "solid" "gray"))
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
    [(string=? button "space")
     (make-bullet ammo (+ x 5) y "normal")]
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
(advanced-start "Student Game" backdrop update-target update-player update-object collide? targets (rectangle 30 30 "solid" "gray") objects offscreen? update-bullet shoot)
