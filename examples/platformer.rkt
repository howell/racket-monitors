#lang racket

(require "../monitor.rkt")
(require "../drivers/timer.rkt")
(require "../../platformer-lib/platform_lib.rkt")
(require racket/gui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entities, Data

;; an Id is a Symbol, representing the identitiy of an entity in the game
(define player-id 'player)

(define FRAMES-PER-SEC 30)
(define FRAME-PERIOD (floor (/ 1000 FRAMES-PER-SEC)))

(define GRAVITY-PER-SEC 6)
(define JUMP-V-PER-SEC -200)

(define EFFECTIVE-GRAVITY (/ GRAVITY-PER-SEC FRAMES-PER-SEC))
(define EFFECTIVE-JUMP-V (/ JUMP-V-PER-SEC FRAMES-PER-SEC))

(define TERMINAL-VELOCITY-PER-SEC 200)
(define EFFECTIVE-TERMINAL-VELOCITY
  (/ TERMINAL-VELOCITY-PER-SEC FRAMES-PER-SEC))

(define DX-PER-SEC 75)
(define EFFECTIVE-DX (/ DX-PER-SEC FRAMES-PER-SEC))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfaces

;; a KeyObserver is an object that implements key-observer<%>
(define key-observer<%>
  (interface () on-key))

;; an EnemyController is an object implementing enemy-controller<%>
(define enemy-controller<%>
  (interface () death))

(define level-over-observer<%>
  ;; level-over : LevelOver -> Void
  (interface () level-over))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Logic

;; Entities send the game logic movement requests. Requests carry the Id of the
;; entity involved.

;; an Enemy is (enemy Id Rect EnemyController), representing the identity,
;; a channel the game logic will use to notify the enemy of relevant events,
;; location, and shape of an enemy
(struct enemy (id rect controller) #:transparent)

;; a GameState is a
;;   (game-state Rect (Listof Rect) Rect (Hashof Id Enemy) Posn)
;; representing the location of the player, the environment, the location
;; of the goal, the location of each enemy, and the size of the level
(struct game-state (player env goal enemies level-size) #:transparent)

;; a LevelOver is one of
;;  'level-complete
;;  'defeat

;; a GameLogic is an instance of game-logic%

(define game-logic%
  (class monitor%
    (super-new)
    (init-field game-clock ;; GameClock
                renderer   ;; Renderer
                )
    (init player0 ;; Rect
          goal0 ;; Rect
          level-size ;; Posn
          )
    
    ;; GameState
    (define gs (game-state player0 '() goal0 (hash) level-size))

    (define/synchronized (read-gs)
      gs)

    (define worker-thread
      (thread
       (thunk
        (let loop ()
          (let ([gs (send this read-gs)])
            (when gs
              (send game-clock wait-for-tick)
              (send renderer render-gamestate gs)
              (loop)))))))

    ;; Listof LevelOverObserver
    (define level-over-observers '())

    ;; LevelOverObserver -> Void
    (define/synchronized (subscribe observer)
      (set! level-over-observers (cons observer level-over-observers)))

    ;; LevelOverObserver -> Void
    (define/synchronized (unsubscribe observer)
      (set! level-over-observers (remove observer level-over-observers)))

    ;; LevelOver -> Void
    (define/synchronized (level-over! reason)
      (set! gs #f)
      (for ([obs (in-list level-over-observers)])
        (send obs level-over reason))
      )

    ;; ID Number -> Void
    (define/synchronized (move-x id dx)
      (when gs
        (define answer
          (match id
            [(== player-id)
             (player-motion-x gs dx)]
            [else
             (enemy-motion-x gs id dx)]))
        (match answer
          [(? game-state? next-gs)
           (set! gs next-gs)]
          [done
           (level-over! done)])))

    ;; ID Number -> Boolean
    ;; returns whether a y collision occurred
    (define/synchronized (move-y id dy)
      (and gs
        (let ()
          (define answer
            (match id
              [(== player-id)
               (player-motion-y gs dy)]
              [else
               (enemy-motion-y gs id dy)]))
          (match answer
            [(cons (? game-state? next-gs) collision?)
             (set! gs next-gs)
             collision?]
            [done
             (level-over! done)
             #f]))))

    ;; -> Boolean
    (define/synchronized (player-can-jump?)
      (and gs
           (let ()
             (define on-top-of-something?
               (cdr (move-player-y (game-state-player gs)
                                   1
                                   (game-state-env gs))))
             on-top-of-something?)))

    ;; ID Rect -> Void
    (define/synchronized (make-enemy id r controller)
      (and gs
           (set! gs (add-enemy gs id r controller))))

    ;; Rect -> Void
    (define/synchronized (make-env r)
      (and gs
           (let ()
             (define new-env (cons r (game-state-env gs)))
             (define next-state (struct-copy game-state gs [env new-env]))
             (set! gs next-state))))))


;; GameState Number -> (U GameState LevelOver)
;; move the player along the x-axis
(define (player-motion-x gs dx)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (match-define (posn x-limit y-limit) lsize)
  (define level-rect (rect (posn 0 0) x-limit y-limit))
  (define player-n (car (move-player-x player-old dx env-old)))
  (cond
    [(overlapping-rects? player-n cur-goal)
     'level-complete]
    [(not (overlapping-rects? player-n level-rect))
     'defeat]
    [(hit-enemy? enemies-old player-n)
     'defeat]
    [else
     (game-state player-n env-old cur-goal enemies-old lsize)]))

;; GameState Number ->
;; (U (Cons GameState Boolean) LevelOver)
;; move the player along the y-axis
(define (player-motion-y gs dy)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (match-define (posn x-limit y-limit) lsize)
  (define level-rect (rect (posn 0 0) x-limit y-limit))
  (match-define (cons player-n col?) (move-player-y player-old dy env-old))
  (define col-enemies
    (for/list ([e (hash-values enemies-old)]
               #:when (overlapping-rects? player-n (enemy-rect e)))
      e))
  (define enemies-new (hash-remove-enemies enemies-old col-enemies))
  (cond
    [(overlapping-rects? player-n cur-goal)
     'level-complete]
    [(not (overlapping-rects? player-n level-rect))
     'defeat]
    [(and (not (empty? col-enemies)) (negative? dy))
     ;; moved upwards into an enemy
     'defeat]
    [else
     (for ([e (in-list col-enemies)])
       (send (enemy-controller e) death))
     (cons (game-state player-n env-old cur-goal enemies-new lsize)
           col?)]))

;; (Hashof Id Enemy) (Listof Enemy) -> (Hashof Id Enemy)
;; remove a bunch of enemies from a hash
(define (hash-remove-enemies h enemies)
  (hash-remove* h (map enemy-id enemies)))

;; (hashof Key Any) (listof Key) -> (hashof Key Any)
;; remove a bunch of keys from a hash
(define (hash-remove* h keys)
  (for/fold ([acc h])
            ([k keys])
    (hash-remove acc k)))

;; (Hashof Id Enemy) Rect -> Boolean
(define (hit-enemy? enemies-old player-n)
  (for/or ([e (in-hash-values enemies-old)])
    (overlapping-rects? player-n (enemy-rect e))))

;; GameState Id Number -> (U GameState 'defeat)
;; move an enemy along the x-axis
(define (enemy-motion-x gs id dx)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (define maybe-enemy (hash-ref enemies-old id #f))
  ;; the enemy might not be in the hash if it was recently killed
  (cond
    [maybe-enemy
     (match-define (enemy _  e-rect e-controller) maybe-enemy)
     (define e-rect-new (car (move-player-x e-rect dx env-old)))
     (cond
       [(overlapping-rects? player-old e-rect-new)
        'defeat]
       [else
        (define enemies-new
          (hash-set enemies-old id (enemy id e-rect-new e-controller)))
        (game-state player-old env-old cur-goal enemies-new lsize)])]
    [else gs]))

;; GameState Id Number -> (U (Cons GameState Boolean) 'defeat)
(define (enemy-motion-y gs id dy)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (define maybe-enemy (hash-ref enemies-old id #f))
  ;; the enemy might not be in the hash if it was recently killed
  (cond
    [maybe-enemy
     (match-define (enemy _ e-rect e-controller) maybe-enemy)
     (match-define (cons e-rect-new col?) (move-player-y e-rect dy env-old))
     (define enemies-new
       (hash-set enemies-old id (enemy id e-rect-new e-controller)))
     (define player-collision? (overlapping-rects? player-old e-rect-new))
     (cond
       [(and player-collision? (positive? dy))
        ;; enemy fell on player
        'defeat]
       [player-collision?
        ;; enemy moved upward into player
        (send e-controller death)
        (define enemies-final (hash-remove enemies-new id))
        (cons (game-state player-old env-old cur-goal enemies-final lsize)
              #f)]
       [else
        (cons (game-state player-old env-old cur-goal enemies-new lsize)
              col?)])]
    [else
     (cons gs #f)]))

;; GameState Id Rect EnemyController -> GameState
(define (add-enemy gs id r controller)
  (define old-enemies (game-state-enemies gs))
  (define new-enemies (hash-set old-enemies id (enemy id r controller)))
  (struct-copy game-state gs [enemies new-enemies]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player Avatar

;; Translate keyboard events to commands to the game logic

(define player%
  (class* monitor% (level-over-observer<%>
                    key-observer<%>)
    (super-new)
    (init-field game-logic ;; GameLogic
                game-clock ;; GameClock
                keyboard-driver ;; KeyboardDriver
                )

    (define dx EFFECTIVE-DX)
    (define gravity EFFECTIVE-GRAVITY)
    (define jump-v EFFECTIVE-JUMP-V)

    (define left-down? #f)
    (define right-down? #f)
    (define vy 0)

    (send keyboard-driver subscribe this)

    ;; make sure mutation is synchronized
    (define/synchronized (update-vy new-vy)
      (set! vy new-vy))

    (define continue? #t)
    (define worker-thread
      (thread
       (thunk
        (let loop ()
          (send game-clock wait-for-tick)
          (define vx (- (if right-down? dx 0)
                     (if left-down? dx 0)))
          (send game-logic move-x player-id vx)
          (define collision?
            (send game-logic move-y player-id vy))
          (define vy-new
            (if collision?
                0
                (min (+ vy gravity) EFFECTIVE-TERMINAL-VELOCITY)))
          (send this update-vy vy-new)
          (when continue?
            (loop))))))

    ;; KeyEvent -> Void
    (define/synchronized (on-key ke)
      (match ke
        [(key-press #\space)
         (when (send game-logic player-can-jump?)
           (set! vy jump-v))]
        [(key-press 'left)
         (set! left-down? #t)]
        [(key-release 'left)
         (set! left-down? #f)]
        [(key-press 'right)
         (set! right-down? #t)]
        [(key-release 'right)
         (set! right-down? #f)]
        [_ (void)]))

    ;; LevelOver -> Void
    (define/synchronized (level-over reason)
      (set! continue? #f)
      (send keyboard-driver unsubscribe)
      (send game-logic unsubscribe))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Clock

;; a GameClock is an instance of game-clock%
(define game-clock%
  (class monitor%
    (inherit notify-all
             wait)
    (super-new)
    (init period-ms)
    (define period-s (/ period-ms 1000))
    
    (define/synchronized (awaken-sleepers)
      (notify-all))
    
    (define/synchronized (wait-for-tick)
      (wait))
    
    (define worker-thread
      (thread
       (thunk
        (let loop ()
          (sleep period-s)
          (awaken-sleepers)
          (loop)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Driver

;; a KeyEvent is one of (key-press KeyCode) or (key-release KeyCode) signifying
;; the press and release of a key, respectively.
;; a KeyCode is as defined by the gui
(struct key-press (key) #:transparent)
(struct key-release (key) #:transparent)

;; Any -> Bool
(define (key-event? x)
  (or (key-press? x) (key-release? x)))

;; a KeyboardDriver is an instance of keyboard-driver%
(define keyboard-driver%
  (class monitor%
    (super-new)
    ;; (Listof KeyObserver)
    (define observers '())

    ;; Key-Event -> Void
    (define/public (handle-key ke)
      (notify-subscribers ke))

    ;; KeyEvent -> Void
    (define/synchronized (notify-subscribers ke)
      (for ([obs (in-list observers)])
        (send obs on-key ke)))
    
    ;; KeyObserver -> Void
    (define/synchronized (subscribe observer)
      (set! observers (cons observer observers)))
    
    ;; KeyObserver -> Void
    (define/synchronized (unsubscribe listener)
      (set! observers (remove listener observers)))))

;; a KeyboardSubscription is a
;;  (keyboard-subscription (Channelof KeyEvent) (Channelof Unsubscribe))
;; informing the keyboard driver to send KeyEvent messages along the first
;; channel until an Unsubscribe is sent on the second
(struct keyboard-subscription (key-chan unsub-chan) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level Managment

;; a Level is a (level Rect
;;                     (Listof Rect)
;;                     Rect
;;                     (GameLogic GameClock -> Any)
;;                     Posn)
;; representing the beginning position of the player,
;; the layout of the environment,
;; the initial position of the goal,
;; a procedure that will spawn processes for the initial enemies in the level
;;   when given the game-logic and game-clock objects
;; and the size of the level as an xy coordinate.
(struct level (player0 env0 goal make-enemies size) #:transparent)

(define level-manager%
  (class* monitor% (level-over-observer<%>)
    (super-new)
    (init-field levels ;; NonemptyListof Level
                game-clock ;; GameClock
                renderer ;; Renderer
                keyboard-driver ;; KeyboardDriver
                )
    (define current-level (first levels))
    (set! levels (rest levels))

    ;; Level GameLogic -> Void
    (define (load-level! lvl game-logic)
        (match-define (level _ env _ mk-es _) lvl)
        (for ([r (in-list env)])
          (send game-logic make-env r))
        (mk-es game-logic game-clock))

    (define (start-current)
      (define game-logic (new game-logic%
                              [renderer renderer]
                              [game-clock game-clock]
                              [player0 (level-player0 current-level)]
                              [goal0 (level-goal current-level)]
                              [level-size (level-size current-level)]))
      (define player (new player%
                          [game-logic game-logic]
                          [game-clock game-clock]
                          [keyboard-driver keyboard-driver]))
      (send game-logic subscribe this)
      (load-level! current-level game-logic))

    (define/synchronized (level-over reason)
      (match reason
        ['level-complete
         (match levels
           ['()
            (send renderer render-victory)]
           [(cons next-level rest-levels)
            (set! current-level next-level)
            (set! levels rest-levels)
            (start-current)])]
        ['defeat
         (printf "restarting\n")
         (start-current)]))

    (start-current)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enemies

(define enemies-where
  (make-weak-hash))

(define enemy%
  (class* monitor% (enemy-controller<%>
                   level-over-observer<%>)
    (super-new)

    (init-field game-logic ;; GameLogic
                game-clock ;; GameClock
                )

    (init x0 ;; Number
          y0 ;; Number
          w  ;; Number
          h  ;; Number
          )

    (define id (gensym 'enemy))

    (define continue? #t)
    (define worker-thread
      (thread
       (thunk
        ;; order of these two important?
        (send game-logic subscribe this)
        (send game-logic make-enemy id (rect (posn x0 y0) w h) this)
        (let loop ([n 0])
          (send game-clock wait-for-tick)
          (behavior n id)
          (when continue?
            (loop (add1 n)))))))
    
    ;; -> Void
    (define/synchronized (death)
      (set! continue? #f))

    ;; LevelOver -> Void
    (define/public (level-over reason)
      (death))

    ;; Natural ID -> Void
    (define/public (behavior n id)
      #f)))

(define horizontal-enemy%
  (class enemy%
    (super-new)
    (inherit-field game-logic)

    (init x-dist ;; Number
          dx0    ;; Number
          )

    (define dx (/ (* dx0 24) FRAMES-PER-SEC))
    (define THRESHOLD (/ x-dist dx))

    (define/override (behavior n id)
      (define right? (< (modulo n (floor (* 2 THRESHOLD)))
                        THRESHOLD))
      (send game-logic move-x id (if right? dx (- dx))))))

;; spawn an enemy that travels from (x0, y0) to (x0 + x-dist, y0) then back to
;; (x0, y0) at a rate of dx per clock tick
(define (make-horiz-enemy game-logic game-clock x0 y0 w h x-dist dx0)
  (new horizontal-enemy%
       [game-logic game-logic]
       [game-clock game-clock]
       [x0 x0]
       [y0 y0]
       [w w]
       [h h]
       [x-dist x-dist]
       [dx0 dx0]))

(define vertical-enemy%
  (class enemy%
    (super-new)
    (inherit-field game-logic)

    (init y-dist ;; Number
          dy0    ;; Number
          )

    (define dy (/ (* dy0 24) FRAMES-PER-SEC))
    (define THRESHOLD (/ y-dist dy))

    (define/override (behavior n id)
      (define up? (< (modulo n (floor (* 2 THRESHOLD))) THRESHOLD))
      (send game-logic move-y id (if up? dy (- dy))))))

;; spawn an enemy that travels from (x0, y0) to (x0, y0 + y-dist) then back to
;; (x0, y0) at a rate of dy per clock tick
(define (make-vert-enemy game-logic game-clock x0 y0 w h y-dist dy0)
  (new vertical-enemy%
       [game-logic game-logic]
       [game-clock game-clock]
       [x0 x0]
       [y0 y0]
       [w w]
       [h h]
       [y-dist y-dist]
       [dy0 dy0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering

;; a RenderMessge is a RenderGamestate or RenderVictory
;; a RenderGamestate message, (render-gamestate GameState), is sent to the
;; renderer to draw the gamestate to the screen.
;; a RenderVictory is (render-victory)

;; a Renderer is an instance of renderer%

(define renderer%
  (class monitor%
    (super-new)
    ;; dc<%>
    (init-field dc ;; dc<%>
                )

    (define game-over? #f)

    ;; -> Void
    (define/synchronized (render-victory)
      (set! game-over? #t)
      (draw-victory dc))

    ;; GameState -> Void
    (define/synchronized (render-gamestate gs)
      (unless game-over?
        (draw-game-state dc gs)))))

(define (draw-victory dc)
  (big-text dc "Victory!" "green"))

(define (big-text dc text color)
  (send dc suspend-flush)
  (send dc clear)
  (send dc set-text-mode 'solid)
  (send dc set-text-foreground color)
  (define fnt (make-object font% 100 'default))
  (send dc set-font fnt)
  (define tl-x (/ (posn-x canvas-bot-right) 6))
  (define tl-y (/ (posn-y canvas-bot-right) 4))
  (send dc draw-text text tl-x tl-y)
  (send dc resume-flush))

;; DC GameState -> Void
(define (draw-game-state dc gs)
  (match-define (game-state old-player old-env old-goal old-enemies lsize) gs)
  (render-game dc old-player old-env old-goal (hash-values old-enemies) lsize))

(define (star-points scale)
  (map (lambda (pr) (cons (* scale (car pr)) (* scale (cdr pr))))
       `((0 . 10)
         (2 . 6)
         (0 . 4)
         (3 . 4)
         (5 . 0)
         (7 . 4)
         (10 . 4)
         (8 . 6)
         (10 . 10)
         (5 . 7))))

;; drawing-context goal -> void
;; draws the goal as a 50x50 yellow star
(define (draw-goal dc g)
  (match-define (rect (posn x0 y0) _ _) g)
  (send dc set-brush "yellow" 'solid)
  (send dc set-pen "yellow" 1 'solid)
  (send dc set-smoothing 'aligned)
  (send dc draw-polygon (star-points 5) x0 y0))

;; drawing-context rect color -> void
;; draws a solid rectangle
(define (draw-rect dc r color)
  (match-define (rect (posn x0 y0) w h) r)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'solid)
  (send dc draw-rectangle x0 y0 w h))

;; drawing-context rect (listof rect) goal (listof enemy) -> void
;; draws the game
(define (draw-game dc player env gl enemies)
  (for ([r env])
    (draw-rect dc r "black"))
  (draw-goal dc gl)
  (for ([e enemies])
    (draw-rect dc (enemy-rect e) "red"))
  (draw-rect dc player "blue"))

;; num num num -> num
;; determine an offset for side-scrolling
(define (scroll-offset player canvas-size level-size)
  (define csize/2 (/ canvas-size 2))
  (cond
    ;; don't scroll when the player is close to the beginning of the level
    [(< (- player csize/2) 0) 0]
    ;; similarly, don't scroll when near the end
    [(> (+ player csize/2) level-size) (- level-size canvas-size)]
    ;; otherwise put the player at the center of the screen
    [else (- player csize/2)]))

(define (render-game canvas-dc player env gl enemies lsize)
  (match-define (posn x-size y-size) canvas-bot-right)
  (match-define (posn player-x player-y) (rect-top-left player))
  (match-define (posn x-limit y-limit) lsize)
  (define src-x (scroll-offset player-x x-size x-limit))
  (define src-y (scroll-offset player-y y-size y-limit))
  (define bitmap (make-object bitmap% x-limit y-limit))
  (define bitmap-dc (send bitmap make-dc))
  (draw-game bitmap-dc player env gl enemies)
  (send canvas-dc suspend-flush)
  (send canvas-dc clear)
  (send canvas-dc draw-bitmap-section bitmap 0 0 src-x src-y x-size y-size)
  (send canvas-dc resume-flush))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gui stuff

(define game-canvas%
  (class canvas%
    (init-field key-handler)
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (define release-code (send event get-key-release-code))
      (cond
        [(release? key-code) (key-handler (key-release release-code))]
        [else (key-handler (key-press key-code))]))
    (super-new)))

(define (space? key)
  (equal? key #\space))

(define (release? key)
  (equal? key 'release))

(define (arrow? key)
  (match key
    [(or 'left 'right 'up 'down) #t]
    [_ #f]))

;; global (mutable) variable with the canvas's bottom-right posn 
(define canvas-bot-right #f)

;; KeyboardDriver PositiveInteger PositiveInteger -> dc<%>
(define (make-frame keyboard-driver width height)
  (parameterize ((current-eventspace (make-eventspace)))
    (define frame (new frame%
                       [label "My Frame"]
                       [width width]
                       [height height]))
    (define canvas
      (new game-canvas%
           [parent frame]
           ;; TODO - really seems like blocking here could cause trouble
           [key-handler (lambda (x) (send keyboard-driver handle-key x))]))
    (send canvas focus)
    (send frame show #t)
    (define-values (x-max y-max) (send canvas get-client-size))
    (set! canvas-bot-right (posn x-max y-max))
    (send canvas get-dc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Booting the game

(define (run-the-game)
  (define game-clock (new game-clock% [period-ms FRAME-PERIOD]))
  (define keyboard-driver (new keyboard-driver%))
  (define dc (make-frame keyboard-driver 600 400))
  (define renderer (new renderer% [dc dc]))
  (define level-manager (new level-manager%
                             [levels ALL-LEVELS]
                             [game-clock game-clock]
                             [renderer renderer]
                             [keyboard-driver keyboard-driver]))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level Data

(define (make-player x0 y0)
  (rect (posn x0 y0) 8 32))

(define (make-goal x0 y0)
  (rect (posn x0 y0) 50 50))

(define PLAYER0 (make-player 0 0))
(define GOAL0 (make-goal 900 150))

(define level0
  (level PLAYER0
         (list (rect (posn 0 200) 150 10)
               (rect (posn 400 200) 1000 10)
               (rect (posn 200 178) 50 10)
               (rect (posn 300 150) 50 10))
         GOAL0
         (lambda (game-logic game-clock)
           (make-horiz-enemy game-logic game-clock 0 180 20 20 130 2)
           (make-horiz-enemy game-logic game-clock 200 158 20 20 30 1)
           (make-horiz-enemy game-logic game-clock 300 130 20 20 30 1)
           (make-horiz-enemy game-logic game-clock 400 180 20 20 180 3))
         (posn 1000 400)))

(define GOAL1 (make-goal 500 150))

(define level1
  (level PLAYER0
         (list (rect (posn 0 200) 600 10))
         GOAL1
         (lambda (game-logic game-clock)
           (make-horiz-enemy game-logic game-clock 0 180 20 20 580 4)
           (make-horiz-enemy game-logic game-clock 0 140 20 20 580 8)
           (make-vert-enemy game-logic game-clock 50 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 100 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 150 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 200 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 250 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 300 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 350 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 400 125 20 20 75 4))
         (posn 600 400)))

;; int int int int int nat nat -> (list rect)
;; make a stair case starting at a given position
(define (ascending-stairs x0 y0 hdist vdist w h n)
  (for/list ([i (in-range n)])
    (define dx (* hdist i))
    (define dy (* vdist i))
    (rect (posn (+ x0 dx) (+ y0 dy)) w h)))

(define level2
  (let ([stairs (ascending-stairs (+ 50 50) (- 800 40)
                                  100 -40
                                  50 10
                                  10)]
        [birdies (lambda (game-logic game-clock)
                  (for/list ([i (in-range 5)])
                    (make-vert-enemy game-logic game-clock
                                     (+ 160 (* i 200))
                                     (- 650 (* i 80))
                                     20
                                     20
                                     120
                                     4)))])
    (level (make-player 0 750)
           (flatten (list stairs
                          (rect (posn 0 800) 50 200)))
           (make-goal 1100 950)
           birdies
           (posn 2000 1000))))

(define ALL-LEVELS (list level0 level1 level2))