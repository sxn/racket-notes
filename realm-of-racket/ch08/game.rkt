#lang racket/base

#| 
   The Orc game 
   -------------

   The Orc game is a turn-based battle game between monsters and the player. 

   The player encounters a room full of monsters of all kinds, including 
   orcs, hydras, slimes, and brigands. They are ready to attack. It is
   the player's task to get rid of the monsters. 

   When the game starts up, it is the player's turn, meaning she is given 
   permission to attack a (randomly chosen number) of times. The player uses
   nine keys to play
    -- With the four arrow keys the player navigates among the twelve monsters.
    -- With "s", "f", and "h", 
    -- the player can 's'tab a specific monster, 
    -- the player may 'f'lail at several monsters; 
    -- the player may 'h'eal herself. 
   When the player runs out of attacks, all live monsters attack the player. 
   After that, it is the player's turn again. 
 
   Just in case, the player can end a turn prematurely with "e". 

   Play
   ----
 
   Run and evaluate 
     (start-game)
   This will pop up a window that displays the player's vitals, the orcs and
   their basic state, and the game instructions. 
|#

(require 2htdp/universe
         2htdp/image
         racket/list)	

;
;
;
;  ██████╗ ██████╗  ██████╗    ██╗    ██╗ ██████╗ ██████╗ ██╗     ██████╗ 
; ██╔═══██╗██╔══██╗██╔════╝    ██║    ██║██╔═══██╗██╔══██╗██║     ██╔══██╗
; ██║   ██║██████╔╝██║         ██║ █╗ ██║██║   ██║██████╔╝██║     ██║  ██║
; ██║   ██║██╔══██╗██║         ██║███╗██║██║   ██║██╔══██╗██║     ██║  ██║
; ╚██████╔╝██║  ██║╚██████╗    ╚███╔███╔╝╚██████╔╝██║  ██║███████╗██████╔╝
;  ╚═════╝ ╚═╝  ╚═╝ ╚═════╝     ╚══╝╚══╝  ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═════╝ 
;                                                                         
;                                                            
;
;

(provide start-game)

;; ----------------------------------------------------------------------------
;; Structs
;; ----------------------------------------------------------------------------
(struct orc-world (player lom attack# target) #:mutable #:transparent)

(struct player (health agility strength) #:mutable #:transparent)

(struct monster (image [health #:mutable]) #:transparent)
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand monster () #:transparent)

;; -----------------------------------------------------------------------------
;; CONSTANTS
;; ----------------------------------------------------------------------------

;; Player attributes
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

;; Game attributes
(define ATTACKS# 20)
(define STAB-DAMAGE 2)
(define FLAIL-DAMAGE 3)
(define HEALING 8)

;; Monster attributes
(define MONSTER# 12)
(define PER-ROW 4)
(unless (zero? (remainder MONSTER# PER-ROW))
  (error 'constraint "PER-ROW must divide MONSTER# evenly into rows"))

(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH 8)
(define SLIMINESS 5)

(define HEALTH-DAMAGE -2)
(define AGILITY-DAMAGE -3)
(define STRENGTH-DAMAGE -4)

;; Strings
(define STRENGTH "strength")
(define AGILITY "agility")
(define HEALTH "health")
(define LOSE "You LOSE. Good day sir!")
(define WIN "Winner winner chicken dinner!")
(define DEAD "X_X")
(define REMAINING "Remaining attacks ")
(define INSTRUCTIONS-2 "Select a monster using the arrow keys")
(define INSTRUCTIONS-1
  "Press S to stab a monster | Press F to Flail wildly | Press H to Heal")

;; UI
(define HEALTH-BAR-HEIGHT 12)
(define HEALTH-BAR-WIDTH  50)

;; computed constants for image frames 
(define ORC     (bitmap "graphics/orc.png"))
(define HYDRA   (bitmap "graphics/hydra.png"))
(define SLIME   (bitmap "graphics/slime.bmp"))
(define BRIGAND (bitmap "graphics/brigand.bmp"))

(define PIC-LIST (list ORC HYDRA SLIME BRIGAND))
(define w (apply max (map image-width PIC-LIST)))
(define h (apply max (map image-height PIC-LIST)))

;; images: player, monsters, constant texts
(define PLAYER-IMAGE  (bitmap "graphics/player.bmp"))

(define FRAME (rectangle w h 'outline 'white))
(define TARGET (circle (- (/ w 2) 2) 'outline 'blue))

(define ORC-IMAGE     (overlay ORC FRAME))
(define HYDRA-IMAGE   (overlay HYDRA FRAME))
(define SLIME-IMAGE   (overlay SLIME FRAME))
(define BRIGAND-IMAGE (overlay BRIGAND FRAME))

(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))

;; fonts & texts & colors
(define AGILITY-COLOR "blue")
(define HEALTH-COLOR "crimson")
(define STRENGTH-COLOR "forest green") 
(define MONSTER-COLOR "crimson")
(define MESSAGE-COLOR "black")
(define ATTACK-COLOR "crimson")

(define HEALTH-SIZE (- HEALTH-BAR-HEIGHT 4))
(define DEAD-TEXT-SIZE (- HEALTH-BAR-HEIGHT 2))
(define INSTRUCTION-TEXT-SIZE 16)
(define MESSAGES-SIZE 40)

(define INSTRUCTION-TEXT
  (above 
   (text INSTRUCTIONS-2 (- INSTRUCTION-TEXT-SIZE 2) "blue")
   (text INSTRUCTIONS-1 (- INSTRUCTION-TEXT-SIZE 4) "blue")))

(define DEAD-TEXT (text DEAD DEAD-TEXT-SIZE "crimson"))
;
;
;
;  ███╗   ███╗ █████╗ ██╗███╗   ██╗
;  ████╗ ████║██╔══██╗██║████╗  ██║
;  ██╔████╔██║███████║██║██╔██╗ ██║
;  ██║╚██╔╝██║██╔══██║██║██║╚██╗██║
;  ██║ ╚═╝ ██║██║  ██║██║██║ ╚████║
;  ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝
;
;
;
(define (start-game)
  (big-bang (initialize-orc-world)
            (on-key player-acts-on-monsters)
            (to-draw render-orc-battle)
            (stop-when end-of-orc-battle? render-the-end)))

(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))

  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (player-acts-on-monsters w k)
  (cond
    [(zero? (orc-world-attack# w)) (void)]
    [(key=? "s" k) (stab w)]
    [(key=? "f" k) (flail w)]
    [(key=? "h" k) (heal w)]
    [(key=? "e" k) (end-turn w)]
    [(key=? "n" k) (initialize-orc-world)]
    [(key=? "left" k) (move-target w -1)]
    [(key=? "right" k) (move-target w +1)]
    [(key=? "up" k) (move-target w (- PER-ROW))]
    [(key=? "down" k) (move-target w (+ PER-ROW))])
  (give-monster-turn-if-attack#=0 w)
  w)

(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))

(define (end-of-orc-battle? w) 
  (or (win? w) (lose? w)))

(define (render-the-end w)
  (render-orc-world w #f (message (if (lose? w) LOSE WIN))))

;; ----------------------------------------------------------------------------
;; World management
;; ----------------------------------------------------------------------------

;
;
;
; ██╗███╗   ██╗██╗████████╗
; ██║████╗  ██║██║╚══██╔══╝
; ██║██╔██╗ ██║██║   ██║   
; ██║██║╚██╗██║██║   ██║   
; ██║██║ ╚████║██║   ██║   
; ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝   
;                          
;
;
;

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (initialize-monsters)
  (build-list
    MONSTER#
    (lambda (_)
      (define health (random+ MONSTER-HEALTH0))
      (case (random 4)
        [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
        [(1) (hydra HYDRA-IMAGE health)]
        [(2) (slime SLIME-IMAGE health (random+ SLIMINESS))]
        [(3) (brigand BRIGAND-IMAGE health)]))))

(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACKS#))

;
;
;
; ██╗  ██╗███████╗██╗   ██╗███████╗██╗   ██╗███████╗███╗   ██╗████████╗███████╗
; ██║ ██╔╝██╔════╝╚██╗ ██╔╝██╔════╝██║   ██║██╔════╝████╗  ██║╚══██╔══╝██╔════╝
; █████╔╝ █████╗   ╚████╔╝ █████╗  ██║   ██║█████╗  ██╔██╗ ██║   ██║   ███████╗
; ██╔═██╗ ██╔══╝    ╚██╔╝  ██╔══╝  ╚██╗ ██╔╝██╔══╝  ██║╚██╗██║   ██║   ╚════██║
; ██║  ██╗███████╗   ██║   ███████╗ ╚████╔╝ ███████╗██║ ╚████║   ██║   ███████║
; ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝  ╚═══╝  ╚══════╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝
;                                                                              
;
;
;

;; ----------------------------------------------------------------------------
;; Player actions
;; ----------------------------------------------------------------------------

(define (move-target w delta)
  (define new (+ (orc-world-target w) delta))
  (set-orc-world-target! w (modulo new MONSTER#)))

(define (end-turn w) 
  (set-orc-world-attack#! w 0))

(define (heal w)
  (decrease-attack# w)
  (player-health+ (orc-world-player w) HEALING))

(define (stab w)
  (decrease-attack# w)
  (define target (current-target w))
  (define damage
    (random-quotient (player-strength (orc-world-player w))
                     STAB-DAMAGE))
  (damage-monster target damage))

(define (flail w)
  (decrease-attack# w)
  (define target (current-target w))
  (define alive (filter monster-alive? (orc-world-lom w)))
  (define pick#
    (min
      (random-quotient (player-strength (orc-world-player w))
                       FLAIL-DAMAGE)
      (length alive)))
  (define getem (cons target (take alive pick#)))

  (for-each (lambda (m) (damage-monster m 1)) getem))

(define (decrease-attack# w)
  (set-orc-world-attack#! w (sub1 (orc-world-attack# w))))

(define (damage-monster m delta)
  (set-monster-health! m (interval- (monster-health m) delta)))

(define (current-target w)
  (list-ref (orc-world-lom w) (orc-world-target w)))

;; ----------------------------------------------------------------------------
;; Monster actions
;; ----------------------------------------------------------------------------

(define (give-monster-turn-if-attack#=0 w)
  (when (zero? (orc-world-attack# w))
    (define player (orc-world-player w))
    (all-monsters-attack-player player (orc-world-lom w))
    (set-orc-world-attack#! w (random-number-of-attacks player))))

(define (all-monsters-attack-player player lom)
  (define (one-monster-attacks-player m)
    (cond
      [(orc? m)
       (player-health+ player (random- (orc-club m)))]
      [(hydra? m)
       (player-health+ player (random- (monster-health m)))]
      [(slime? m)
       (player-health+ player -1)
       (player-agility+ player (random- (slime-sliminess m)))]
      [(brigand? m)
       (case (random 3)
         [(0) (player-health+ player HEALTH-DAMAGE)]
         [(1) (player-agility+ player AGILITY-DAMAGE)]
         [(2) (player-strength+ player STRENGTH-DAMAGE)])]))
  (define live-monsters (filter monster-alive? lom))

  (for-each one-monster-attacks-player live-monsters))

;; ----------------------------------------------------------------------------
;; Actions on player
;; ----------------------------------------------------------------------------

(define (player-update! setter selector max)
  (lambda (the-player delta)
    (setter the-player
      (interval+ (selector the-player) delta max))))
(define player-health+
  (player-update! set-player-health! player-health MAX-HEALTH))
(define player-agility+
  (player-update! set-player-agility! player-agility MAX-AGILITY))
(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))

;
;
;
; ██████╗ ███████╗███╗   ██╗██████╗ ███████╗██████╗ ██╗███╗   ██╗ ██████╗ 
; ██╔══██╗██╔════╝████╗  ██║██╔══██╗██╔════╝██╔══██╗██║████╗  ██║██╔════╝ 
; ██████╔╝█████╗  ██╔██╗ ██║██║  ██║█████╗  ██████╔╝██║██╔██╗ ██║██║  ███╗
; ██╔══██╗██╔══╝  ██║╚██╗██║██║  ██║██╔══╝  ██╔══██╗██║██║╚██╗██║██║   ██║
; ██║  ██║███████╗██║ ╚████║██████╔╝███████╗██║  ██║██║██║ ╚████║╚██████╔╝
; ╚═╝  ╚═╝╚══════╝╚═╝  ╚═══╝╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝ ╚═════╝ 
;                                                                         
;
;
;

(define (render-orc-world w t info)
  (define i-player (render-player (orc-world-player w)))
  (define i-monster (render-monsters (orc-world-lom w) t))
  (above V-SPACER
         (beside H-SPACER
                 i-player
                 H-SPACER H-SPACER H-SPACER
                 (above i-monster
                        V-SPACER V-SPACER V-SPACER
                        info)
                 H-SPACER)
         V-SPACER))

(define (render-player p)
  (define s (player-strength p))
  (define a (player-agility p))
  (define h (player-health p))
  (above/align
    "left"
    (status-bar s MAX-STRENGTH STRENGTH-COLOR STRENGTH)
    V-SPACER
    (status-bar a MAX-AGILITY AGILITY-COLOR AGILITY)
    V-SPACER
    (status-bar h MAX-HEALTH HEALTH-COLOR HEALTH)
    V-SPACER V-SPACER V-SPACER
    PLAYER-IMAGE))

(define (status-bar v-current v-max color label)
  (define w (* (/ v-current v-max) HEALTH-BAR-WIDTH))
  (define f (rectangle w HEALTH-BAR-HEIGHT 'solid color))
  (define b (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGHT 'outline color))
  (define bar (overlay/align 'left 'top f b))
  (beside bar H-SPACER (text label HEALTH-SIZE color)))

(define (message str)
  (text str MESSAGES-SIZE MESSAGE-COLOR))

(define (instructions w) 
  (define na (number->string (orc-world-attack# w)))
  (define ra (string-append REMAINING na))
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
  (above txt INSTRUCTION-TEXT))

(define (render-monsters lom with-target)
  (define target
    (if (number? with-target)
        (list-ref lom with-target)
        'a-silly-symbol-that-cannot-be-eq-to-an-orc))

  (define (render-one-monster m)
    (define image
      (if (eq? m target)
          (overlay TARGET (monster-image m))
          (monster-image m)))
    (define health (monster-health m))
    (define health-bar
      (if (= health 0)
          (overlay DEAD-TEXT (status-bar 0 1 'white ""))
          (status-bar health MONSTER-HEALTH0 MONSTER-COLOR "")))
    (above health-bar image))

  (arrange (map render-one-monster lom)))

(define (arrange lom)
  (cond
    [(empty? lom) empty-image]
    [else (define r (apply beside (take lom PER-ROW)))
          (above r (arrange (drop lom PER-ROW)))]))

;
;
;
; ███████╗███╗   ██╗██████╗ ██████╗ 
; ██╔════╝████╗  ██║██╔══██╗╚════██╗
; █████╗  ██╔██╗ ██║██║  ██║  ▄███╔╝
; ██╔══╝  ██║╚██╗██║██║  ██║  ▀▀══╝ 
; ███████╗██║ ╚████║██████╔╝  ██╗   
; ╚══════╝╚═╝  ╚═══╝╚═════╝   ╚═╝   
;                                   
;
;
;

(define (win? w)
  (all-dead? (orc-world-lom w)))

(define (lose? w)
  (player-dead? (orc-world-player w)))

(define (player-dead? p)
  (or (= (player-health p) 0)
      (= (player-agility p) 0)
      (= (player-strength p) 0)))

(define (all-dead? lom)
  (not (ormap monster-alive? lom)))

(define (monster-alive? m)
  (> (monster-health m) 0))

;
;
;
; ██╗   ██╗████████╗██╗██╗     ███████╗
; ██║   ██║╚══██╔══╝██║██║     ██╔════╝
; ██║   ██║   ██║   ██║██║     ███████╗
; ██║   ██║   ██║   ██║██║     ╚════██║
; ╚██████╔╝   ██║   ██║███████╗███████║
;  ╚═════╝    ╚═╝   ╚═╝╚══════╝╚══════╝
;                                      
;
;
;
(define (interval- n m (max-value 100))
  (min (max 0 (- n m)) max-value))

(define (interval+ n m (max-value 100))
  (interval- n (- m) max-value))

(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div)
       0
       (random+ (add1 div))))

(define (random+ n)
  (add1 (random n)))

(define (random- n)
  (- (add1 (random n))))
;
;
;
; ████████╗███████╗███████╗████████╗███████╗
; ╚══██╔══╝██╔════╝██╔════╝╚══██╔══╝██╔════╝
;    ██║   █████╗  ███████╗   ██║   ███████╗
;    ██║   ██╔══╝  ╚════██║   ██║   ╚════██║
;    ██║   ███████╗███████║   ██║   ███████║
;    ╚═╝   ╚══════╝╚══════╝   ╚═╝   ╚══════╝
;                                           
;
;
;

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  ;; Test structs
  (define WORLD0 (orc-world (initialize-player) empty 0 0))
  (define WORLD1 (struct-copy orc-world (initialize-orc-world) [attack# 5]))
  (define (WORLD2) (struct-copy orc-world (initialize-orc-world) [attack# 0]))
  (define AN-ORC (orc 'image 0 5))
  (define A-SLIME (slime 'image 1 6))
  (define A-HYDRA (hydra 'image 2))
  (define A-BRIGAND (brigand 'image 3))

  ;; --------------------------------------------------------------------------
  ;; Test `move-target`
  ;; --------------------------------------------------------------------------
  (check-equal? (let ([w (orc-world 'dummy 'dummy 'dummy 0)])
                  (move-target w +1)
                  w)
                (orc-world 'dummy 'dummy 'dummy 1))
  (check-equal? (let ([w (orc-world 'dummy 'dummy 'dummy 0)])
                  (move-target w -1)
                  w)
                (orc-world 'dummy 'dummy 'dummy (- MONSTER# 1)))
  (check-equal? (let ([w (orc-world 'dummy 'dummy 'dummy 0)])
                  (move-target w (- PER-ROW))
                  w)
                (orc-world 'dummy 'dummy 'dummy (- MONSTER# PER-ROW)))
  (check-equal? (let ([w (orc-world 'dummy 'dummy 'dummy 1)])
                  (move-target w (+ PER-ROW))
                  w)
                (orc-world 'dummy 'dummy 'dummy (+ PER-ROW 1)))
  (check-equal? (begin
                  (move-target WORLD1 0)
                  WORLD1)
                WORLD1)
  (check-equal? (let ()
                  (define w (struct-copy orc-world WORLD1))
                  (move-target w 4)
                  w)
                (struct-copy orc-world WORLD1 [target (+ 4 (orc-world-target WORLD1))]))
  (check-equal? (current-target WORLD1) 
                (first (orc-world-lom WORLD1)))
  ;; --------------------------------------------------------------------------
  ;; Test player manipulations
  ;; --------------------------------------------------------------------------
  (check-equal? (let ([p (player 1 0 0)])
                  (player-health+ p 5)
                  p)
                (player 6 0 0))
  (check-equal? (let ([p (player 0 1 0)])
                  (player-agility+ p 5)
                  p)
                (player 0 6 0))
  
  (check-equal? (let ([p (player 0 0 1)])
                  (player-strength+ p 5)
                  p)
                (player 0 0 6))
  
  (check-equal? (let ([p (player 5 5 5)])
                  (all-monsters-attack-player p (list (orc 'image 1 1)))
                  p)
                (player 4 5 5))
  
  (check-equal? (let ([p (player 5 5 5)])
                  (all-monsters-attack-player p (list (hydra 'image 1)))
                  p)
                (player 4 5 5))
  
  (check-equal? (let ([p (player 5 5 5)])
                  (all-monsters-attack-player p (list (slime 'image 1 1)))
                  p)
                (player 4 4 5))
  
  (check member 
         (let ([p (player 5 5 5)]) 
           (all-monsters-attack-player p (list (brigand 'image 1)))
           p)
         (list (player 3 5 5)
               (player 5 2 5)
               (player 5 5 1)))
  ;; --------------------------------------------------------------------------
  ;; Properties
  ;; --------------------------------------------------------------------------
  ;; --------------------------------------------------------------------------
  ;; Test initializers
  ;; --------------------------------------------------------------------------
  (check-true (monster? (first (initialize-monsters))))
  (check-true (> 10 (monster-health (first (initialize-monsters)))))
  (check-equal? (length (initialize-monsters)) MONSTER#)
  (check-equal? (length (orc-world-lom WORLD1)) MONSTER#)
  (check-true (>= (let ([p (initialize-player)])
                    (player-health p))
                  (let ([p (initialize-player)])
                    (all-monsters-attack-player p (list AN-ORC))
                    (player-health p))))
  (check-true (> (player-health (initialize-player)) 
                 (let ([p (initialize-player)])
                   (all-monsters-attack-player p (list A-HYDRA))
                   (player-health p))))
  (check-true (< (let ([p (initialize-player)])
                   (all-monsters-attack-player p (list A-SLIME))
                   (player-agility p))
                 (let ([p (initialize-player)])
                   (player-agility p))))
  (check-true (let ([p (initialize-player)])
                (all-monsters-attack-player p (list A-BRIGAND))
                (or (= (player-health p)
                       (- (player-health (initialize-player)) 2))
                    (= (player-agility p)
                       (- (player-agility (initialize-player)) 3))
                    (= (player-strength p)
                       (- (player-strength (initialize-player)) 4)))))
  (check-equal? (length (orc-world-lom WORLD1)) MONSTER#)
  (check-equal? (orc-world-player WORLD1) (orc-world-player WORLD1))
  ;; --------------------------------------------------------------------------
  ;; Test monster actions
  ;; --------------------------------------------------------------------------
  (check-true (or (> (player-health (orc-world-player (WORLD2)))
                     (player-health (orc-world-player 
                                     (let ([w (WORLD2)])
                                       (all-monsters-attack-player (orc-world-player w) (orc-world-lom w))
                                       w))))
                  (> (player-strength (orc-world-player (WORLD2)))
                     (player-strength (orc-world-player
                                       (let ([w (WORLD2)])
                                         (all-monsters-attack-player (orc-world-player w) (orc-world-lom w))
                                         w))))
                  (> (player-agility (orc-world-player (WORLD2)))
                     (player-agility (orc-world-player
                                      (let ([w (WORLD2)])
                                        (all-monsters-attack-player (orc-world-player w) (orc-world-lom w))
                                        w))))))
  ;; --------------------------------------------------------------------------
  ;; Test player actions
  ;; --------------------------------------------------------------------------
  (test-begin (define o (orc 'image 0 5))
              (damage-monster o 5)
              (check-equal? o (orc 'image 0 5)))
  (test-begin (define o (orc 'image 0 5))
              (damage-monster o 0)
              (check-equal? o (orc 'image 0 5)))
  (check-equal? (player-health (orc-world-player
                                (let () 
                                  (define w (struct-copy orc-world WORLD1))
                                  (heal w)
                                  w)))
                (min MAX-HEALTH
                     (+ 8 (player-health (orc-world-player WORLD1)))))
  
  (check-equal? (length (orc-world-lom 
                         (let ()
                           (define w (struct-copy orc-world WORLD1))
                           (stab w)
                           w)))
                MONSTER#)
  
  ;; --------------------------------------------------------------------------
  ;; Test game predicates
  ;; --------------------------------------------------------------------------
  (check-false (lose? WORLD0))
  (check-true (lose? (orc-world (player 0 30 30) empty 0 0)))
  (check-true (all-dead? (list (orc 'image 0 0) (hydra 'image 0))))
  (check-true (all-dead? (list AN-ORC)))
  (check-true (win? (orc-world (initialize-player) (list (orc 'image 0 0)) 0 0)))
  (check-true (win? (orc-world (initialize-player) (list AN-ORC) 0 0)))
  (check-true (end-of-orc-battle? (orc-world (initialize-player) (list (orc 'image 0 0)) 0 0)))
  (check-true (end-of-orc-battle? (orc-world (initialize-player) (list AN-ORC) 0 0)))
  (check-true (end-of-orc-battle? (orc-world (player 0 30 30) empty 0 0)))
  (check-true (player-dead? (player 0 2 5)))
  (check-false (player-dead? (initialize-player)))
  (check-false (not (monster-alive? A-HYDRA)))
  (check-true (monster-alive? (monster 'image 1)))
  (check-false (monster-alive? (orc 'image 0 0)))

  "all tests run")
