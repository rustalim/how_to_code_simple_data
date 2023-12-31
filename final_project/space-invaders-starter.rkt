;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Space Invaders ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;
;; Constants: ;;
;;;;;;;;;;;;;;;;

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions: ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; Invaders is one of:
;;  - empty
;;  - (cons Invader Invaders)
;; interp. a list of Invaders

(define LOI0 empty)
(define LOI1 (list I1))
(define LOI2 (list I1 I2))

#;
(define (fn-for-invaders loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader  (first loi))
              (fn-for-invaders (rest loi)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader Invaders)
;;  - reference: (first loi) loi Invader
;;  - self-reference: (rest loi) loi Invaders


(define-struct missile (x y))
;; Missile loi (make-missile Number Number)
;; interp. the missile's location loi x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; Template rules used:
;;  - compound: 2 fields


;; Missiles is one of:
;;  - empty
;;  - (cons Missile Missiles)
;; interp. a list of missiles

(define LOM0 empty)
(define LOM1 (list M1))
(define LOM2 (list (make-missile 50 (+ 0 (/ (image-height MISSILE) 2)))
                   M1))

#;
(define (fn-for-missiles lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile  (first lom))
              (fn-for-missiles (rest lom)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Missile Missiles)
;;  - reference: (first lom) is Missile
;;  - self-reference: (rest lom) is Missiles

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;;;;;;;;;;;;;;;;
;; Functions: ;;
;;;;;;;;;;;;;;;;

;; Game -> Game
;; !!! start the world with (main G0)

(define (main g)
  (big-bang g                               ; G
    (on-tick   advance-game)                ; G -> G
    (stop-when game-over? render-game-over) ; G -> G
    (to-draw   render-game)                 ; G -> Image
    (on-key    handle-key)))                ; G KeyEvent -> G


;; ============================================
;; Functions for moving objects with every tick


;; Game -> Game
;; move invaders, missiles, tank on every tick
(check-expect (advance-game (make-game empty empty T0))
              (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-expect (advance-game (make-game (list I1) (list M1) T2))
              (make-game
               (list (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12))
               (list (make-missile 150 (- 300 MISSILE-SPEED)))
               (make-tank (- 50 TANK-SPEED) -1)))


;(define (advance-game g) G0)

;<use template from Game>

(define (advance-game g)
  (make-game (del-invader     (next-invaders (game-invaders g))
                              (move-missiles (game-missiles g)))
             (destroy-missile (move-missiles (game-missiles g))
                              (next-invaders (game-invaders g)))
             (move-tank       (game-tank g))))


;; Tank -> Tank
;; change position tank
(check-expect (move-tank T0) 
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (move-tank T1)
              (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (move-tank T2)
              (make-tank (- 50 TANK-SPEED) -1))
(check-expect (move-tank (make-tank (- WIDTH (/ (image-width TANK) 2)) 1))   ;tank at the right border
              (make-tank (- WIDTH (/ (image-width TANK) 2)) 1))             ;moving right
(check-expect (move-tank (make-tank (/ (image-width TANK) 2) -1))            ;tank at the left border
              (make-tank (/ (image-width TANK) 2) -1))                      ;moving left

;(define (move-tank t) T0)     ;stub

;<use template from Tank>
        
(define (move-tank t)
  (cond
    [(> (tank-dir t) 0)
     (if (< (+ (tank-x t) TANK-SPEED) (- WIDTH (/ (image-width TANK) 2)))
         (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))
         (make-tank (- WIDTH (/ (image-width TANK) 2)) (tank-dir t)))]
    [(< (tank-dir t) 0)
     (if (> (- (tank-x t) TANK-SPEED) (/ (image-width TANK) 2))
         (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))
         (make-tank (/ (image-width TANK) 2) (tank-dir t)))]))


;; Missiles -> Missiles
;; produce filtered and ticked list of missiles
(check-expect (move-missiles (list M1))
              (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (move-missiles (list M1 (make-missile 200 88)))
              (list (make-missile 150 (- 300 MISSILE-SPEED))
                    (make-missile 200 (- 88 MISSILE-SPEED))))
(check-expect (move-missiles
               (list (make-missile 50 (+ 0 (/ (image-height MISSILE) 2))) M1))
              (list (make-missile 150 (- 300 MISSILE-SPEED))))

;(define (move-missiles lom) lom)   ;stub

;<use template from Missiles>

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (del-missile
          (cons (move-missile  (first lom))
                (move-missiles (rest lom))))]))


;; Missile -> Missile
;; change positions of invaders, missiles and tank
(check-expect (move-missile M1)
              (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 50 (+ 0 (/ (image-height MISSILE) 2))))     ;at the upper edge
              (make-missile 50 (- (+ 0 (/ (image-height MISSILE) 2)) MISSILE-SPEED)))

;(define (move-missile m) M1)   ;stub

;<use template from Missile>

(define (move-missile m)
  (make-missile
   (missile-x m)
   (- (missile-y m) MISSILE-SPEED)))


;; Missiles -> Missiles
;; delete missile from list when it reaches the upper boundary of screen
(check-expect (del-missile empty) empty)
(check-expect (del-missile (list M1))
              (list M1))
(check-expect (del-missile (list (make-missile 50 (+ 0 (/ (image-height MISSILE) 2))) M1))
              (list M1))

;(define (del-missile lom) lom)   ;stub

;<use template from Missiles>

(define (del-missile lom)
  (cond [(empty? lom) lom]
        [else
         (if (out-of-screen? (first lom))
             (del-missile (rest lom))
             (cons (first lom) (del-missile (rest lom))))]))


;; Missile -> Boolean
;; produce true when (- (y coord) semimissile) is less or equal than upper edge of screen
(check-expect (out-of-screen? M1) false)
(check-expect (out-of-screen? (make-missile 50 (+ 0 (/ (image-height MISSILE) 2)))) true)
(check-expect (out-of-screen? (make-missile 200 -15)) true)

;(define (out-of-screen? m) true)   ;stub

(define (out-of-screen? m)
  (<= (- (missile-y m) (/ (image-height MISSILE) 2)) 0))


;; Invaders -> Invaders
;; produce list of Invaders:
;;  - move Invaders with every tick
;;  - add Invader at random point of upper screen border
;;  - delete Invader when missile hits it

;(define (next-invaders loi) loi)

(define (next-invaders loi)
  (move-invaders
   (add-invader loi)))


;; Invaders -> Invaders
;; move invaders in the list of invaders with every tick
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list I1))
              (list (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12)))
(check-expect (move-invaders (list I2 I1))
              (list
               (make-invader (- 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) -10)
               (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12)))

;(define (move-invaders is) empty)   ;stub

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi)) (move-invaders (rest loi)))]))


;; Invader -> Invader
;; move invader with every tick
(check-expect (move-invader I1)
              (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12))
(check-expect (move-invader I2)
              (make-invader (- 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) -10))
(check-expect (move-invader I3)
              (make-invader (+ 150 INVADER-X-SPEED) (+ (+ HEIGHT 10) INVADER-Y-SPEED) 10))
(check-expect (move-invader (make-invader (- WIDTH (/ (image-width INVADER) 2)) 150 12))  ;right edge
              (make-invader
               (- (- WIDTH (/ (image-width INVADER) 2)) INVADER-X-SPEED)
               (+ 150 INVADER-Y-SPEED) -12))
(check-expect (move-invader (make-invader (/ (image-width INVADER) 2) 180 -10))           ;left edge
              (make-invader
               (+ (/ (image-width INVADER) 2) INVADER-X-SPEED)
               (+ 180 INVADER-Y-SPEED) 10))

;(define (move-invader i) i)   ;stub

;<use template from Invader>

(define (move-invader i)
  (cond [(> (invader-dx i) 0)
         (if (< (+ (invader-x i) INVADER-X-SPEED) (- WIDTH (/ (image-width INVADER) 2)))
             (make-invader
              (+ (invader-x i) INVADER-X-SPEED)
              (+ (invader-y i) INVADER-Y-SPEED)
              (invader-dx i))
             (make-invader
              (- (invader-x i) INVADER-X-SPEED)
              (+ (invader-y i) INVADER-Y-SPEED)
              (-(invader-dx i))))]
        [(< (invader-dx i) 0)
         (if (> (- (invader-x i) INVADER-X-SPEED) (/ (image-width INVADER) 2))
             (make-invader
              (- (invader-x i) INVADER-X-SPEED)
              (+ (invader-y i) INVADER-Y-SPEED)
              (invader-dx i))
             (make-invader
              (+ (invader-x i) INVADER-X-SPEED)
              (+ (invader-y i) INVADER-Y-SPEED)
              (-(invader-dx i))))]))



;; Invaders -> Invaders
;; generete new invader to list of invaders
;; don't know how to check-expect with randoms:(
;(check-expect (add-invader empty) (list (make-invader 150 0 10)))
;(check-expect (add-invader empty) (list (make-invader 188 0 10)))
;(check-expect (add-invader (list I1)) (list (make-invader 150 0 10) I1))
;(check-expect (add-invader (list I1 I2)) (list (make-invader 188 0 10) I1 I2))

;(define (add-invader loi) empty)   ;stub

(define (add-invader loi)
  (cond [(> INVADE-RATE (random 2500))
         (cons (make-invader
                (random (floor (- WIDTH (image-width INVADER))))
                0 10)
               loi)]
        [else loi]))


;; Invaders Missiles -> Invaders
;; delete invader from list when hitted by missile
(check-expect (del-invader
               (list I1 (make-invader 20 30 10))
               (list M1 (make-missile 50 200)))
              (list I1 (make-invader 20 30 10)))
(check-expect (del-invader
               (list I1 (make-invader 20 30 10))
               (list (make-missile 150 109) (make-missile 50 200)))
              (list (make-invader 20 30 10)))
(check-expect (del-invader
               (list I1 (make-invader 20 30 10))
               (list M1 (make-missile 28 36)))
              (list I1))

;(define (del-invader loi lom) empty)   ;stub

(define (del-invader loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (destroyed-invader? (first loi) lom)
             (del-invader (rest loi) lom)
             (cons (first loi) (del-invader (rest loi) lom)))]))



;; Invader Missiles -> Boolean
;; produce true if invader hitted by one of missiles
(check-expect (destroyed-invader? I1 (list )) false)
(check-expect (destroyed-invader? I1 (list M1)) false)
(check-expect (destroyed-invader? I1 (list M1 (make-missile 150 109))) true)
(check-expect (destroyed-invader? I1 (list M1 (make-missile 145 98))) true)

;(define (destroyed-invader? i lom) false)   ;stub

(define (destroyed-invader? i lom)
  (cond [(empty? lom) false]
        [else
         (or (hitted? i (first lom))
             (destroyed-invader? i (rest lom)))]))


;; Missiles Invaders -> Missiles
;; remove missile wheh it hits invader
(check-expect (destroy-missile
               (list M1 (make-missile 50 200))
               (list I1 (make-invader 20 30 10)))
              (list M1 (make-missile 50 200)))
(check-expect (destroy-missile
               (list (make-missile 150 109) (make-missile 50 200))
               (list I1 (make-invader 20 30 10)))
              (list (make-missile 50 200)))
(check-expect (destroy-missile
               (list M1 (make-missile 28 36))
               (list I1 (make-invader 20 30 10)))
              (list M1))

;(define (destroy-missile lom loi) empty)   ;stub

(define (destroy-missile lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (destroyed-missile? (first lom) loi)
             (destroy-missile (rest lom) loi)
             (cons (first lom) (destroy-missile (rest lom) loi)))])) 




;; Missile Invaders -> Boolean
;; produce true if missile hits any of invaders
(check-expect (destroyed-missile? M1 (list )) false)
(check-expect (destroyed-missile? M1 (list I1)) false)
(check-expect (destroyed-missile? M1 (list (make-invader 150 295 12))) true)
(check-expect (destroyed-missile? M1 (list I1 (make-invader 150 295 12))) true)

;(define (destroyed-missile? m loi) false)   ;stub

(define (destroyed-missile? m loi)
  (cond [(empty? loi) false]
        [else
         (or (hitted? (first loi) m)
             (destroyed-missile? m (rest loi)))]))



;; Invader Missile -> Boolean
;; produce true when Missile hit Invader (coordinates of objects become close each other)
(check-expect (hitted? I1 M1) false)
(check-expect (hitted? I1 (make-missile 150 100)) true)
(check-expect (hitted? I1 (make-missile 161 100)) false)
(check-expect (hitted? I1 (make-missile 145 98))  true)
(check-expect (hitted? I1 (make-missile 150 109)) true)
(check-expect (hitted? I1 (make-missile 150 111)) false)

;(define (hitted? i m) false)   ;stub

(define (hitted? i m)
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))


;; Game -> Boolean
;; prduce true when invaders landed in game
(check-expect (game-over? (make-game empty empty T0)) false)
(check-expect (game-over? (make-game (list I1) (list M1) T0)) false)
(check-expect (game-over? (make-game (list I2) (list M1) T0)) true)

;(define (game-over? g) false)   ;stub

(define (game-over? g)
  (landed-invaders? (game-invaders g)))


;; Invaders -> Boolean
;; produce true when one of invaders landed
(check-expect (landed-invaders? (list )) false)
(check-expect (landed-invaders? (list I1)) false)
(check-expect (landed-invaders? (list (make-invader 150 HEIGHT 12))) true)
(check-expect (landed-invaders? (list I1 I2)) true)

;(define (landed-invaders? loi) false)   ;stub

(define (landed-invaders? loi)
  (cond [(empty? loi) false]
        [else
         (or (landed? (first loi))
             (landed-invaders? (rest loi)))]))


;; Invader -> Boolean
;; produce true when Invader landed
(check-expect (landed? I1) false)
(check-expect (landed? I2) true)
(check-expect (landed? I3) true)

;(define (landed? i) false)   ;stub

(define (landed? i)
  (>= (invader-y i) HEIGHT))


;; ============================
;; Functions to render objects:


;; Game -> Image
;; render tank, missiles and invaders on the BACKGROUND
(check-expect (render-game (make-game
                            (list I1)
                            (list M1)
                            T0))
              (place-image
               INVADER
               150 100
               (place-image
                MISSILE
                150 300
                (place-image
                 TANK
                 (/ WIDTH 2)
                 (- HEIGHT (/ (image-height TANK) 2))
                 BACKGROUND))))

;(define (render-game g) BACKGROUND)

;<use template from Game>

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-miss (game-missiles g)
                                (render-tank (game-tank g)))))


;; Tank -> Image
;; render current state of Game
(check-expect (render-tank T0)
              (place-image TANK
                           (/ WIDTH 2)
                           (- HEIGHT (/ (image-height TANK) 2))
                           BACKGROUND))

;(define (render-tank t) BACKGROUND)   ;stub

(define (render-tank t)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))


;; Missiles Image -> Image
;; render missiles on the BACKGROUND
(check-expect (render-miss (list M1) BACKGROUND)
              (place-image MISSILE 150 300 BACKGROUND))
(check-expect (render-miss (list (make-missile (/ WIDTH 2) (- HEIGHT (image-height TANK)))
                                 M1) BACKGROUND)
              (place-image
               MISSILE
               (/ WIDTH 2)
               (- HEIGHT (image-height TANK))
               (place-image MISSILE 150 300 BACKGROUND)))

;(define (render-miss lom img) BACKGROUND)   ;stub

;<use template from Missiles>

(define (render-miss lom img)
  (cond [(empty? lom) img]
        [else
         (render-mis (first lom)
                     (render-miss (rest lom) img))]))


;; Missile Image -> Image
;; render missile on the BAKGROUND
(check-expect (render-mis M1 BACKGROUND)
              (place-image MISSILE 150 300 BACKGROUND))

;(define (render-mis m img) BACKGROUND)

(define (render-mis m img)
  (place-image MISSILE
               (missile-x m)
               (missile-y m)
               img))


;; Invaders Image -> Image
;; render list of Invaders on BACKGROUND
(check-expect (render-invaders (list ) BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list I1) BACKGROUND)
              (place-image INVADER 150 100 BACKGROUND))
(check-expect (render-invaders
               (list I2 I1)
               BACKGROUND)
              (place-image INVADER 150 HEIGHT
                           (place-image INVADER 150 100 BACKGROUND)))

;(define (render-invaders loi img) BACKGROUND)   ;stub

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (render-invader (first loi)
                         (render-invaders (rest loi) img))]))


;; Invader Image -> Image
;; render Invader on BACKGROUND
(check-expect (render-invader I1 BACKGROUND)
              (place-image INVADER 150 100 BACKGROUND))

;(define (render-invader i img) BACKGROUND)   ;stub

(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))


;; Game -> Image
;; produce game over image on the screen

(define (render-game-over loi)
  (if (game-over? loi)
      (place-image (overlay
                    (text "GAME OVER" 50 "black")
                    (rectangle WIDTH HEIGHT "solid" "red"))
                   (/ WIDTH 2)
                   (/ HEIGHT 2)
                   BACKGROUND)
      loi))



;; ===============================
;; Functions to handle key events:

;; Game KeyEvent -> Game
;; initiate missile from current tank position when space button pressed, change dir of tank
;; when left of right arrow pressed
(check-expect (handle-key (make-game empty empty T0) " ")
              (make-game empty
                         (list (make-missile (/ WIDTH 2) (- HEIGHT (image-height TANK))))
                         T0))
(check-expect (handle-key (make-game (list I1) (list M1) T0) " ")
              (make-game (list I1)
                         (list (make-missile (/ WIDTH 2) (- HEIGHT (image-height TANK))) M1)
                         T0))

(check-expect (handle-key (make-game empty empty T0) "j")
              (make-game empty empty T0))
(check-expect (handle-key (make-game empty empty T0) "right")
              (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-expect (handle-key (make-game empty empty T0) "left")
              (make-game empty empty (make-tank (- (/ WIDTH 2) TANK-SPEED) -1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) -1)) "right")
              (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) -1)) "left")
              (make-game empty empty (make-tank (- (/ WIDTH 2) TANK-SPEED) -1)))
(check-expect (handle-key (make-game empty empty T0) "a")
              (make-game empty empty T0))

(check-expect (handle-key (make-game empty (list M1) T2) " ")
              (make-game empty
                         (list
                          (make-missile 50 (- HEIGHT (image-height TANK))) M1)
                         T2))

;(define (handle-key g ke) (make-game empty empty T0))   ;stub

(define (handle-key g ke)
  (cond [(key=? ke " ")
         (make-game
          (game-invaders g)
          (cons (make-missile (tank-x (game-tank g)) (- HEIGHT (image-height TANK)))
                (game-missiles g))
          (game-tank g))]
        [(or (key=? ke "right") (key=? ke "left"))
         (make-game
          (game-invaders g)
          (game-missiles g)
          (handle-key-tank (game-tank g) ke))]
        [else g]))


;; Tank KeyEvent -> Tank
;; Move TANK when left/right arrow pressed, init missile when space button pressed
(check-expect (handle-key-tank T0 "right")
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (handle-key-tank T0 "left")
              (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
(check-expect (handle-key-tank (make-tank (/ WIDTH 2) -1) "right")
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (handle-key-tank (make-tank (/ WIDTH 2) -1) "left")
              (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
(check-expect (handle-key-tank T0 "a")
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (handle-key-tank T0 " ")
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (handle-key-tank (make-tank (/ WIDTH 2) -1) "j")
              (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))

;(define (handle-key-tank t ke) T0)    ;stub

(define (handle-key-tank t ke)
  (cond [(key=? ke "right")
         (make-tank (+ (tank-x t) TANK-SPEED)  1)]
        [(key=? ke "left")
         (make-tank (- (tank-x t) TANK-SPEED) -1)]
        [else
         (if (> (tank-dir t) 0)
             (make-tank (+ (tank-x t) TANK-SPEED)  1)
             (make-tank (- (tank-x t) TANK-SPEED) -1))]))


#;
(main (make-game (list (make-invader 150 0 10))
                 (list M1)
                 T0))
