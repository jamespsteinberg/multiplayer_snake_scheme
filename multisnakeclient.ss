;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname multisnakeclient) (read-case-sensitive #t) (teachpacks ((lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.ss" "teachpack" "2htdp")))))
#|

MULTIPLAYER SNAKE 

INSTRUCTIONS TO PLAY

1. Copy the client code and put it on as many different computers
as you wish. Or run it on the same computer.
\Just run it in a different scheme window.

2. Choose a computer to run the server on, find out the ip address
 of the computer. Make this the HOST constant under each of the clients used. 
If all the clients are run under the same computer as the server, 
the term "LOCALHOST" may be used too.
Make sure all the computers are in the same LAN
It helps if they are all connected to NU using a cable rather than wifi

3. Run the server first on a computer by running (server u)

4. Make sure the clients each have different values for the NAME constant

5. The clients may enter and leave independent of each other
as long as the server remains running.

GAMEPLAY:
q quits
arrow keys move

Every time the player respawns, they must wait 5 seconds to move
they are invincible during this time
The first snake to grow to 25 segments wins! (eat 25 foods)
There is no way to permanently die, feel free to kamikazee the 
winning player and watch out yourself.



|# 

#|
-------------------------------------------------------------
-------------------------------------------------------------
CLIENT
-------------------------------------------------------------
-------------------------------------------------------------
|#



;; client

;; if newer version of scheme, may need:
; (require 2htdp/universe)
; (require 2htdp/image)
; (require htdp/image)

;; A SPosn is a (list Natural Natural)
;; int. An SPosn is a position with the first element being the X
;; coordinates and the second element being the Y coordinates
(define x first)
(define y second)

;; a Player is a (list String [Listof SPosn] State Direction)
;; int. a Player is a list of the name, a list of SPosns, 
;; the game state, and the direction
(define playerName first)
(define loPlayerSPosn second)
(define gameState third)
(define direction fourth)

;; a state is one of:
;; -- "playing"
;; -- Number ;; counts down to 0 to play

;; a direction is one of:
;; -- "left"
;; -- "right"
;; -- "up"
;; -- "down"

;; A Food is a (list Type [Listof SPosn])
;; int. a Food is a list of the Type and a list of SPosns
(define type first)
(define loFoodSPosn second)

;; a Type is one of:
;; -- "normal"

;; a Universe is a (list [Listof Player] [Listof Food])
;; int. A universe is a list of a list of players and a list of foods
(define loPlayers first)
(define loFood second)

;; a Message is one of:
;; -- Universe
;; -- (string-append iworld " Won")

;; a worldSetting is one of:
;; -- "quit"
;; -- "continue"
;; -- Number ;;counts down to 0

(define-struct world (name universe dir wS))
;; world (make-world String Message Direction worldSetting)


; Result is
;   (make-package World SPosn)


;; CONSTANTS
;; ------------
(define NAME "Jim") ;; player name
(define YOURCOLOR "blue") ;; player body color when moving
(define OTHERCOLOR "pink") ;; enemy body color when moving
(define YOURHEAD "teal") ;; player head
(define OTHERHEAD "red") ;; enemy head
(define YOURWAITCOLOR "blue") ;; player color when waiting
(define OTHERWAITCOLOR "pink") ;; enemy color when waiting

(define TICKRATE 0.20) ;; must be same as clients 
(define SCREENTIMER 3) ;; seconds the win screen should be up
  
(define WONSCREEN (/ SCREENTIMER TICKRATE)) 
;; number the to count down till removing it

(define sceneSize 400) ;; must be same as clients
(define segmentSide 20) ;; must be same as clients
(define MT (empty-scene sceneSize sceneSize)) 
(define yourHead (rectangle segmentSide segmentSide "solid" YOURHEAD))
(define otherHead (rectangle segmentSide segmentSide "solid" OTHERHEAD))
(define yourBody (rectangle segmentSide segmentSide "solid" YOURCOLOR))
(define otherBody (rectangle segmentSide segmentSide "solid" OTHERCOLOR))
(define yourWait (rectangle segmentSide segmentSide "solid" YOURWAITCOLOR))
(define otherWait (rectangle segmentSide segmentSide "solid" OTHERWAITCOLOR))
(define normalFood (rectangle segmentSide segmentSide "solid" "brown"))
(define FONTSIZE 12)
(define FONTCOLOR "black")

(define HOST "LOCALHOST") ;; ip address of server
;; LOCALHOST if on same computer as server

;; ------------------------------


;; FOR TESTING 
;;--------------------------------------

(define MTtest (empty-scene 100 100))

;; represents sposns 
(define sposn1 (list 1 2))
(define sposn2 (list 1 3))
(define sposn3 (list 1 4))

(define sposn4 (list 1 1))
(define sposn5 (list 2 2))
(define sposn6 (list 2 3))

;; represents a list of Sposns
(define lsposn1 (list sposn1 sposn2 sposn3))
(define lsposn2 (list sposn4 sposn5 sposn6))
(define lsposn3 (list sposn4))
(define lsposn4 (list sposn4))
(define lsposn10 (list))

;; represents players 
(define player1 (list "Jim" lsposn1 "playing" "left"))
(define player2 (list "Zack" lsposn1 "quit" "left"))
(define player3 (list "Jim" lsposn3 "playing" "left"))
(define player4 (list "Sam" lsposn4 "playing" "left"))


;; represents a list of players 
(define players1 (list player1 player2))
(define players2 (list player3 player4))
(define players3 (list player4))
(define players10 (list))


;; represents food 
(define food1(list "normal" lsposn1))
(define food2(list "normal" lsposn2))

;; represents a list of foods 
(define foods1 (list food1 food2))
(define foods2 (list empty))
(define foods3 (list food1))

;; represents a universe 
(define univ1 (list players1 foods1))
(define univ2 (list players2 foods1))

;; represents a world 
(define world1 (make-world "world1" univ1 "left" "continue"))
(define world2 (make-world "world3" univ2 "left" "continue"))
(define world3 (make-world "world4" "someone Won" "left" 50))
(define world4 (make-world "world5" "someone Won" "left" "continue"))
(define world5 (make-world "world1" univ1 "up" "continue"))

;;----------------------------------------------------

 
;; receive: World Message -> World
;; make the current world have the data from Universe
(define (receive world univ)
  (cond
    [(and (number? (world-wS world)) (< 0 (world-wS world))) world]
    [(and (string? (world-wS world)) (string? (world-universe world))) (make-world (world-name world) (world-universe world) (world-dir world) WONSCREEN)]
    [else 
     (make-world (world-name world) univ (world-dir world) "continue")]))

(check-expect (receive world1 univ1) 
              (make-world (world-name world1) univ1 "left" "continue"))
(check-expect (receive world2 univ2) 
              (make-world (world-name world2) univ2 "left" "continue"))
(check-expect (receive world3 "someone Won") world3)
(check-expect (receive world4 univ1)
              (make-world (world-name world4) (world-universe world4) (world-dir world4) WONSCREEN)) 
 
 
;; setDirection World Direction -> Package
;; updates the world with a new direction
(define (setDirection world key)
  (cond
    [(and (string=? key "left") (differentDir? key world))
     (make-package 
      (make-world (world-name world) 
                  (world-universe world) "left" (world-wS world)) 
      "left")]
    [(and (string=? key "right") (differentDir? key world))
     (make-package (make-world (world-name world) 
                               (world-universe world) "right" (world-wS world)) 
                   "right")]
    [(and (string=? key "up") (differentDir? key world))
     (make-package (make-world (world-name world) 
                               (world-universe world) "up" (world-wS world)) 
                  "up")]
    [(and (string=? key "down") (differentDir? key world))
     (make-package (make-world (world-name world) 
                               (world-universe world) "down" (world-wS world)) 
                   "down")]
    [(string=? key "q") 
     (make-world (world-name world) (world-universe world) (world-dir world) "quit")]
    [else world]))

(check-expect (setDirection world1 "up")
              (make-package (make-world (world-name world1)
                          (world-universe world1)
                          "up" (world-wS world1)) "up"))
(check-expect (setDirection world5 "left")
              (make-package (make-world (world-name world1)
                          (world-universe world1)
                          "left" (world-wS world1)) "left"))
(check-expect (setDirection world2 "right") 
              (make-package (make-world (world-name world2)
                          (world-universe world2)
                          "right" (world-wS world1)) "right"))
(check-expect (setDirection world2 "up") 
              (make-package (make-world (world-name world2)
                          (world-universe world2)
                          "up" (world-wS world1)) "up"))
(check-expect (setDirection world2 "down") 
              (make-package (make-world (world-name world2)
                          (world-universe world2)
                          "down" (world-wS world1)) "down"))
(check-expect (setDirection world2 "q") 
              (make-world (world-name world2) 
          (world-universe world2) (world-dir world2) "quit"))
(check-expect (setDirection world2 "left") 
              world2)
          

;; differentDir? : String World -> String 
;; decides if a player can make a desired move
(define (differentDir? newDir wrld)
  (cond
    [(string=? newDir (world-dir wrld)) false]
    [(and (string=? newDir "left") (string=? (world-dir wrld) "right")
          (isHead>2 NAME (loPlayers (world-universe wrld)))) false]
    [(and (string=? newDir "right") (string=? (world-dir wrld) "left")
          (isHead>2 NAME (loPlayers (world-universe wrld)))) false]
    [(and (string=? newDir "up") (string=? (world-dir wrld) "down")
          (isHead>2 NAME (loPlayers (world-universe wrld)))) false]
    [(and (string=? newDir "down") (string=? (world-dir wrld) "up")
          (isHead>2 NAME (loPlayers (world-universe wrld)))) false]
    [else true]))

(check-expect (differentDir? "right" world1) false)
(check-expect (differentDir? "right" world2) true)
(check-expect (differentDir? "left" world2) false)
(check-expect (differentDir? "up" world2) true)
(check-expect (differentDir? "left" (make-world "m" (list players1 
                              '()) "right" "continue")) false)
(check-expect (differentDir? "up" (make-world "m" (list players1 
                             '()) "down" "continue")) false)
(check-expect (differentDir? "down" (make-world "m" (list players1 
                                '()) "up" "continue")) false)
        
        
;; isHead>2 : String [Listof Player] -> Boolean 
;; determines if the head of this player's snake is bigger than two boxes
(define (isHead>2 name players)
  (cond 
    [(empty? players) false]
    [(equal? NAME (playerName (first players))) 
     (cond 
       [(empty? (rest (loPlayerSPosn (first players)))) false]
       [(empty? (rest (rest (loPlayerSPosn (first players))))) false]
       [else true])]
    [else (isHead>2 name (rest players))]))

(check-expect (isHead>2 "Jim" players1) true)
(check-expect (isHead>2 "Jim" players10) false)
(check-expect (isHead>2 "Dan" players2) false)
(check-expect (isHead>2 "Sam" players2) false)
(check-expect (isHead>2 "Sam" players3) false)
(check-expect (isHead>2 "Jim" (list (list "Jim" 
     (list sposn1 sposn1) "playing" "left"))) false)


;; drawWorld: World -> Scene
;; to render the scene
(define (drawWorld world)
  (cond 
    [(string? (world-universe world))
     (place-image (text (world-universe world) FONTSIZE FONTCOLOR) 
            (/ (- sceneSize (image-width 
       (text (world-universe world) FONTSIZE FONTCOLOR))) 2)
     (/ (- sceneSize (image-height 
   (text (world-universe world) FONTSIZE FONTCOLOR))) 2) MT)]
    [else
     (addHorizontalGrid 
      (addVerticalGrid 
       (addFoods 
        (addPlayers MT (loPlayers (world-universe world))) 
        (loFood (world-universe world))) 0) 0)]))


;; addPlayers: [Listof Player] Scene -> Scene
;; render a scene with all the players added
(define (addPlayers scn lop)
  (cond
    [(empty? lop) scn]
    [else (addPlayers 
           (addPlayerHead 
            (x (loPlayerSPosn (first lop)))
            (playerName (first lop)) 
            (addOneData (loPlayerSPosn (first lop)) 
                        (playerBodyImage? (playerName 
                                           (first lop))) scn) 
            (gameState (first lop))) 
           (rest lop))]))

(check-expect (addPlayers MT players3)
              (place-image otherHead 1 1 MT))
(check-expect (addPlayers MT players10) MT)  



;; playerBodyImage?: Name -> Image
;; to return the image for the player
(define (playerBodyImage? n)
  (cond
   [(equal? n NAME) yourBody]
   [else otherBody]))

(check-expect (playerBodyImage? "Jim") yourBody)
(check-expect (playerBodyImage? "mark") otherBody)



;; addPlayerHead: SPosn Name Scene State -> Scene
;; to return a scene with the head of the player added
(define (addPlayerHead p n scn state)
  (cond
    [(equal? n NAME)
     (cond
       [(number? state) (place-image yourWait (x p) (y p) scn)]
       [else (place-image yourHead (x p) (y p) scn)])]
    [else 
     (cond
       [(number? state) (place-image otherWait (x p) (y p) scn)]
       [else (place-image otherHead (x p) (y p) scn)])]))

(check-expect (addPlayerHead sposn1 "mark" MT "playing")
              (place-image otherHead 1 2 MT))
(check-expect (addPlayerHead sposn1 "mark" MT 54)
              (place-image otherWait 1 2 MT))
(check-expect (addPlayerHead sposn3 "Jim" MT "playing")
              (place-image yourHead 1 4 MT))
(check-expect (addPlayerHead sposn3 "Jim" MT 54)
              (place-image yourWait 1 4 MT))


;; addFoods: Scene [Listof Food] -> Scene
;; render a scene with all the foods added
(define (addFoods scn lof)
  (cond
    [(empty? lof) scn]
    [else  (addFoods 
            (addOneData (loFoodSPosn (first lof)) 
                        (foodImage? (type (first lof))) scn) 
            (rest lof))]))

(check-expect (addFoods MT empty) MT)
(check-expect (addFoods MT foods3) 
              (place-image 
               normalFood
               1 4 MT))


;; foodImage?: Type -> Image 
;; to return the correct image for the type of food
(define (foodImage? tp)
  normalFood)
;; add cond for other types of food besides normal

(check-expect (foodImage? "normal") normalFood)


;; addOneData: [Listof SPosn] img Scene -> Scene
;; to return a scene with all of this Data added
(define (addOneData lop img scn)
  (cond
   [(empty? lop) scn]
   [else (place-image img 
                      (x (first lop)) 
                      (y (first lop)) 
                      (addOneData (rest lop) img scn))]))

(check-expect (addOneData lsposn10 yourHead MT) MT)
(check-expect (addOneData lsposn4 yourHead MT)
              (place-image yourHead 1 1 MT))


;; addHorizontalGrid: Scene Integer -> Scene
;; adds Horizontal lines to make the scene look like a grid
(define (addHorizontalGrid scn y)
  (cond
    [(<= y sceneSize) 
     (add-line 
      (addHorizontalGrid scn (+ y segmentSide)) 0 y sceneSize y "black")]
    [else scn]))
              

;; addVerticalGrid: Scene Integer -> Scene
;; adds Vertical lines to make the scene look like a grid
(define (addVerticalGrid scn x)
  (cond
    [(<= x sceneSize) 
     (add-line 
      (addVerticalGrid scn (+ x segmentSide)) x 0 x sceneSize "black")]
    [else scn]))


(define EMPTY-U (list empty empty))
(define EMPTY-W (make-world NAME EMPTY-U "right" "continue"))

;; quit?: World -> Boolean
;; returns true when world 
(define (quit? wrld)
  (and (string? (world-wS wrld)) (string=? (world-wS wrld) "quit")))

(check-expect (quit? (make-world "name" '() "left" "quit")) true)
(check-expect (quit? (make-world "name" '() "left" "continue")) false)

;; removeWinScreen?: World -> World
;; changes worldSetting to continue or counts down if its still a number
(define (removeWinScreen? wrld)
  (cond 
    [(number? (world-wS wrld))
     (make-world (world-name wrld) (world-universe wrld) 
                (world-dir wrld) (sub1 (world-wS wrld)))]
    [else wrld]))

(check-expect (removeWinScreen? (make-world "name" '() "left" "continue")) (make-world "name" '() "left" "continue"))
(check-expect (removeWinScreen? (make-world "name" '() "left" 50)) (make-world "name" '() "left" 49))

; String -> WorldState
; create and hook up a world with the LOCALHOST server
(define (play n)
  (big-bang EMPTY-W
            (on-receive receive)
            (on-draw drawWorld)
            (on-key setDirection)
            (on-tick removeWinScreen? TICKRATE)
            (name n)
            (register HOST)
            (stop-when quit?)))
