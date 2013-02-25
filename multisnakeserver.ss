;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname multisnakeserver) (read-case-sensitive #t) (teachpacks ((lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.ss" "teachpack" "2htdp")))))
;; check client file first



#|
-------------------------------------------------
------------------------------------------------
SERVER
------------------------------------------------
-------------------------------------------------
|#

;; to start:
;; (server u)

;;server

;; if newer version of scheme
; (require 2htdp/universe)

;; A SPosn is a (list Natural Natural)
;; int. An SPosn is a position with the first element 
;; being the X
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
;; -- "moved"
;; -- Number ;; counts down to 0 to play
;; -- "quit"

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
;; int. A universe is a list of a list of players and a 
;; list of foods
(define loPlayers first)
(define loFood second)

; Result is
;   (make-bundle Universe
;                (list (make-mail iworld Message))
;                '())

;; a Message is one of:
;; -- Universe
;; -- (string-append iworld " Won")
;; -- 0


;; CONSTANTS
;; ---------------
(define START 5) ;; starting time in seconds
(define TICKRATE 0.20) ;; must be same as clients 
(define WAIT (/ START TICKRATE)) ;;countdown to move

(define normalNum 0) ;; food type to pick when given a random number
(define foodChoices 1) ;; number of food choices

(define dirChoices 4) ;; number of direction choices
;; different directions
(define leftNum 0)
(define rightNum 1)
(define upNum 2)
(define downNum 3)

(define sceneSize 400) ;; must be same as clients
(define segmentSide 20) ;; must be same as clients

(define WON 25) ;; length of snake to win
;; ------------------



;; FOR TESTING 
;;--------------------------------------

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
(define lsposn4 (list sposn4 sposn5))
(define lsposn5 (list sposn1 sposn1 sposn2 sposn3
                      sposn4 sposn5 sposn6 sposn4
                      sposn4 sposn5 sposn6 sposn4
                      sposn4 sposn5 sposn6 sposn4
                      sposn4 sposn5 sposn6 sposn4
                      sposn4 sposn5 sposn6 sposn4))

;; represents players 
(define player1 (list iworld1 lsposn1 "playing" "left"))
(define player2 (list iworld2 lsposn1 "quit" "left"))
(define player3 (list iworld3 lsposn3 "playing" "left"))
(define player5 (list iworld3 lsposn5 5 "left"))
(define player6 (list iworld3 lsposn2 2 "left"))

;; represents a list of players 
(define players1 (list player1 player2))
(define players2 (list player3 player2))
(define players5 (list player5))
(define players10 (list empty empty))



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

;; SPosn?: [Listof X] -> Boolean
;; returns true if given a SPosn
(define (SPosn? p)
  (and (number? (x p)) (number? (y p))))

(check-expect (SPosn? sposn1) true)
(check-expect (SPosn? univ1) false)

;; Food? [Listof X] -> Boolean
;; returns true if given a Food
(define (Food? f)
  (and (string? (type f)) (andmap SPosn? (loFoodSPosn f))))

(check-expect (Food? food1) true)
(check-expect (Food? univ1) false)

;; Player? [Listof X] -> Boolean
;; returns true if given a Player
(define (Player? plyr)
  (and (iworld? (playerName plyr)) 
       (andmap SPosn? (loPlayerSPosn plyr)) 
       (or (number? (gameState plyr)) 
           (string? (gameState plyr))) 
       (string? (direction plyr))))

(check-expect (Player? player1) true)
(check-expect (Player? univ1) false)

;; Universe?: [Listof X] -> Boolean
;; returns true if given a Universe
(define (Universe? univ)
  (and (andmap Player? (loPlayers univ)) 
       (andmap Food? (loFood univ))))

(check-expect (Universe? univ1) true)
;; produces an error, we want it to since
;; these functions are for testing only
;;(check-expect (Universe? players1) false)

;;----------------------------------------------------

;add-player: Universe iworld -> Result
; add new player to the universe and send it to all the players
(define (add-player univ wrld)
  (sendBundle (cons (list wrld empty "" "") (loPlayers univ))
              (placeFood 
               (placePlayer univ wrld))))

;; trust us, this works :P

;; sendAll: [Listof Player] Message -> [Listof Mail]
;; to produce a list of mail containing a message 
;; to every player in [Listof Player]
(define (sendAll lop univ)
  (cond
    [(empty? lop) empty]
    [else (cons (make-mail (playerName (first lop)) univ) 
                (sendAll (rest lop) univ))]))

(check-expect 
 (sendAll players1 
          (list (makeSExpression players1) 
                (list empty))) 
 (list (make-mail iworld1 (list (makeSExpression players1)
                                (list empty))) 
       (make-mail iworld2 (list (makeSExpression players1)
                                (list empty)))))
(check-expect 
 (sendAll players2 
          (list (makeSExpression players2) 
                (list empty))) 
 (list (make-mail iworld3 
                  (list (makeSExpression players2) 
                        (list empty))) 
       (make-mail iworld2 (list (makeSExpression players2) 
                                (list empty))))) 

;; updatePlayers: Universe iworld direction -> Result 
;; updates all the Players directions
(define (updatePlayers univ wrld dir)
  (make-bundle 
   (list (findPlayer (loPlayers univ) wrld dir changeDir) 
         (loFood univ)) '() '())) 

(check-expect (updatePlayers univ1 iworld1 "up")
              (make-bundle
               (list
                (list
                 (list
                  iworld1
                  (list
                   (list 1 2)
                   (list 1 3)
                   (list 1 4))
                  "playing"
                  "up")
                 (list
                  iworld2
                  (list
                   (list 1 2)
                   (list 1 3)
                   (list 1 4))
                  "quit"
                  "left"))
                (list
                 (list
                  "normal"
                  (list
                   (list 1 2)
                   (list 1 3)
                   (list 1 4)))
                 (list
                  "normal"
                  (list
                   (list 1 1)
                   (list 2 2)
                   (list 2 3)))))
               empty
               empty))
(check-expect (updatePlayers univ1 iworld2 "up") 
              (make-bundle
               (list
                (list
                 (list
                  iworld1
                  (list
                   (list 1 2)
                   (list 1 3)
                   (list 1 4))
                  "playing"
                  "left")
                 (list
                  iworld2
                  (list
                   (list 1 2)
                   (list 1 3)
                   (list 1 4))
                  "quit"
                  "up"))
                (list
                 (list
                  "normal"
                  (list
                   (list 1 2)
                   (list 1 3)
                   (list 1 4)))
                 (list
                  "normal"
                  (list
                   (list 1 1)
                   (list 2 2)
                   (list 2 3)))))
               empty
               empty))
;; changeDir: Player Direction -> Player
;; to change the player's direction
(define (changeDir plyr dir)
  (list (playerName plyr) (loPlayerSPosn plyr) 
        (gameState plyr) dir))

(check-expect (changeDir player1 "right")
              (list (playerName player1) 
                    (loPlayerSPosn player1)
                    (gameState player1) "right"))
(check-expect (changeDir player2 "up")
              (list (playerName player2)
                    (loPlayerSPosn player2)
                    (gameState player2) "up"))

;; removeFood: Universe SPosn -> Universe
;; to remove the food at that SPosn in the Universe
(define (removeFood univ p)
  (list (loPlayers univ) (removeFood-helper (loFood univ) p)))

(check-expect (removeFood univ1 sposn1)
              (list
 (list
  (list
   iworld1
   (list
    (list 1 2)
    (list 1 3)
    (list 1 4))
   "playing"
   "left")
  (list
   iworld2
   (list
    (list 1 2)
    (list 1 3)
    (list 1 4))
   "quit"
   "left"))
 (list
  (list
   "normal"
   (list
    (list 1 3)
    (list 1 4)))
  (list
   "normal"
   (list
    (list 1 1)
    (list 2 2)
    (list 2 3))))))

;; removeFood-helper: [Listof Food] SPosn -> [Listof Food]
;; to remove the one food from the list of foods
(define (removeFood-helper lof p)
  (cond 
    [(empty? lof) empty]
    [else 
     (cond 
       [(empty? (searchType 
                 (loFoodSPosn (first lof)) p)) 
        (removeFood-helper (rest lof) p)]
       [else 
        (cons (list (type (first lof)) 
                    (searchType 
                     (loFoodSPosn (first lof)) p)) 
              (removeFood-helper (rest lof) p))])]))

(check-expect (removeFood-helper foods1 sposn1)
              (list
               (list
                "normal"
                (list (list 1 3) (list 1 4)))
               (list
                "normal"
                (list
                 (list 1 1)
                 (list 2 2)
                 (list 2 3)))))
(check-expect (removeFood-helper empty sposn1) empty)

;; searchType: [Listof SPosn] SPosn -> [Listof SPosn]
;; to remove the SPosn from the list
(define (searchType lop p)
  (cond
    [(empty? lop) empty]
    [(equal? p (first lop)) (rest lop)]
    [else (cons (first lop) (searchType (rest lop) p))]))

(check-expect (searchType lsposn1 sposn1)
              (list sposn2 sposn3))
(check-expect (searchType lsposn1 sposn2)
              (list sposn1 sposn3))

;; sendBundle: [Listof Player] Message -> Result
;; sends a bundle, giving the current Universe to all Players
(define (sendBundle lop m)
  (cond
    [(string? m) 
     (make-bundle (reset lop (list empty empty))
                  (sendAll lop m) '())]
    [else 
     (make-bundle m
                  (sendAll lop (list (makeSExpression 
                                      (loPlayers m)) 
                                     (loFood m)))'())]))

;; reset: [Listof Player] Universe -> Universe
;; empties the data from all the players and food
(define (reset lop univ)
  (cond
    [(empty? lop) univ]
    [else (reset (rest lop)
                 (placeFood 
                  (placePlayer univ 
                               (playerName (first lop)))))]))

;; contatins random numbers that were already tested


;; won?: [Listof Player] Universe -> Message
;; returns a Message whether a player has won or continues on
(define (won? lop univ)
  (cond
    [(empty? lop) univ]
    [(< WON (length (loPlayerSPosn (first lop)))) 
     (string-append (iworld-name 
                     (playerName (first lop))) " Won" )]
    [else (won? (rest lop) univ)]))

(check-expect (won? players1 univ1) univ1)
(check-expect (won? players5 univ1) 
              (string-append (iworld-name 
                     (playerName (first players5))) " Won" ))
                           
;; makeSExpression: [Listof Player] -> [Listof Player]
;; make each player an S-expression by replacing the 
;; iworld structure with only the name
(define (makeSExpression lop)
  (cond
    [(empty? lop) empty]
    [else 
     (cons 
      (list (iworld-name 
             (playerName (first lop))) 
            (loPlayerSPosn (first lop)) 
            (gameState (first lop)) 
            (direction (first lop))) 
      (makeSExpression (rest lop)))]))

;; hitWall?: SPosn -> Boolean
;; returns true if outside the walls
(define (hitWall? p)
  (cond
    [(not (< 0 (y p) sceneSize)) true]
    [(not (< 0 (x p) sceneSize)) true]
    [else false]))

(check-expect (hitWall? sposn1) false)
(check-expect (hitWall? (list 2000 2000)) true)
(check-expect (hitWall? (list 2000 2)) true)

;; growPlayer: Universe iworld -> Universe
;; to add a SPosn to the end of a player
(define (growPlayer univ wrld)
  (list (findPlayer 
         (loPlayers univ) wrld '() addTail) (loFood univ)))

(check-expect (growPlayer univ1 iworld1)
              (list
               (list
                (list
                 iworld1
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4)
                  empty)
                 "playing"
                 "left")
                (list
                 iworld2
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4))
                 "quit"
                 "left"))
               (list
                (list
                 "normal"
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4)))
                (list
                 "normal"
                 (list
                  (list 1 1)
                  (list 2 2)
                  (list 2 3))))))

;; placePlayer: Universe iworld -> Universe
;; to place a player where no other Data is and tell that player
(define (placePlayer univ wrld)
  (list 
   (cons (list wrld 
               (list (placeThing 
                      (append 
                       (appendPlayers (loPlayers univ)) 
                       (appendFoods (loFood univ))))) 
               WAIT         
               (local [(define (randomDir rdm) 
                         (cond
                           [(= rdm leftNum) "left"]
                           [(= rdm rightNum) "right"]
                           [(= rdm upNum) "up"]
                           [else "down"]))]           
                 
                 (randomDir (random dirChoices))))
         (loPlayers univ)) (loFood univ)))

;; usues random numbers that are tested separately 


;; someoneLeft: Universe iworld -> Universe
(define (someoneLeft univ wrld)
  (sendBundle (loPlayers univ) (removePlayer univ wrld)))

(check-expect (someoneLeft univ1 iworld1)
              (make-bundle
 (list
  (list
   (list
    iworld2
    (list
     (list 1 2)
     (list 1 3)
     (list 1 4))
    "quit"
    "left"))
  (list
   (list
    "normal"
    (list
     (list 1 2)
     (list 1 3)
     (list 1 4)))
   (list
    "normal"
    (list
     (list 1 1)
     (list 2 2)
     (list 2 3)))))
 (list
  (make-mail
   iworld1
   (list
    (list
     (list
      "iworld2"
      (list
       (list 1 2)
       (list 1 3)
       (list 1 4))
      "quit"
      "left"))
    (list
     (list
      "normal"
      (list
       (list 1 2)
       (list 1 3)
       (list 1 4)))
     (list
      "normal"
      (list
       (list 1 1)
       (list 2 2)
       (list 2 3))))))
  (make-mail
   iworld2
   (list
    (list
     (list
      "iworld2"
      (list
       (list 1 2)
       (list 1 3)
       (list 1 4))
      "quit"
      "left"))
    (list
     (list
      "normal"
      (list
       (list 1 2)
       (list 1 3)
       (list 1 4)))
     (list
      "normal"
      (list
       (list 1 1)
       (list 2 2)
       (list 2 3)))))))
 empty))

;; removePlayer: Universe iworld -> Universe
;; to remove the given player
(define (removePlayer univ wrld)
  (list (removePlayer-helper (loPlayers univ) wrld) 
        (loFood univ)))

(check-expect (removePlayer univ1 iworld1)
(list
 (list
  (list
   iworld2
   (list
    (list 1 2)
    (list 1 3)
    (list 1 4))
   "quit"
   "left"))
 (list
  (list
   "normal"
   (list
    (list 1 2)
    (list 1 3)
    (list 1 4)))
  (list
   "normal"
   (list
    (list 1 1)
    (list 2 2)
    (list 2 3))))))

;; removePlayer-helper: [Listof Player] iworld -> 
;; [Listof Player]
;; removes the player from the list of players
(define (removePlayer-helper lop wrld)
  (cond
    [(empty? lop) empty]
    [(iworld=? wrld (playerName (first lop)))
     (removePlayer-helper (rest lop) wrld)]
    [else (cons (first lop) 
                (removePlayer-helper (rest lop)
                                     wrld))]))

(check-expect (removePlayer-helper players1 iworld1)
              (list
               (list
                iworld2
                (list
                 (list 1 2)
                 (list 1 3)
                 (list 1 4))
                "quit"
                "left")))


;; appendPlayers: [Listof Player] -> [Listof SPosn]
;; makes a list of all the SPosns filled by Players
(define (appendPlayers lop)
  (cond
    [(empty? lop) empty]
    [else (append  (removeTailPosn? 
                    (loPlayerSPosn (first lop)) 
                    (gameState (first lop))) 
                   (appendPlayers (rest lop)))]))

;; removeTailPosn?: [Listof SPosn] state -> [Listof SPosn]
;; to remove the tail if the state = "playing" because it will move from that
(define (removeTailPosn? lop s)
  (cond
    [(and (string? s) (string=? s "playing")) (removeTail lop)]
    [else lop]))

(check-expect (removeTailPosn? lsposn1 "playing")
              (list sposn1 sposn2))
(check-expect (removeTailPosn? lsposn1 "moved")
              lsposn1)


;; appendFoods: [Listof Food] -> [Listof SPosn]
;; makes a list of all the SPosns filled by Foods
(define (appendFoods lof)
  (cond
    [(empty? lof) empty]
    [(empty? (first lof)) 
     (appendFoods (rest lof))]
    [else (append (loFoodSPosn (first lof)) 
                  (appendFoods (rest lof)))]))

(check-expect (appendFoods foods1)
              (list sposn1 sposn2 sposn3 sposn4 sposn5 sposn6))
(check-expect (appendFoods foods2) empty)

;; placeFood: Universe -> Universe
;; to place food randomly where no other Data currently is
(define (placeFood univ)
  (list (loPlayers univ) 
        (addToType             
         (local 
           [(define (randomType rdm) 
              (cond
                [(= rdm normalNum) "normal"]
                [else "normal"]))]
           
           (randomType (random foodChoices)))
         
         (placeThing (append (appendPlayers (loPlayers univ)) 
                             (appendFoods (loFood univ)))) 
         (loFood univ))))

(check-range  
 (first (first (second 
                (first (first (rest (placeFood univ1))))))) 
 0 500)
(check-range  
 (first (first (second 
                (first (first (rest (placeFood univ2))))))) 
 0 500)


;; addToType: Type SPosn [Listof Food] -> [Listof Food]
;; to add a food of the given type and SPosn to 
;; the List of foods
(define (addToType tp p lof)
  (cond
    [(empty? lof) (cons (list tp (list p)) empty)]
    [(string=? tp (type (first lof))) 
     (cons (list tp (cons p (loFoodSPosn (first lof)))) (rest lof))]
    [else (cons (first lof) (addToType tp p (rest lof)))]))


(check-expect (addToType "normal" sposn6 foods1)
              (list
               (list
                "normal"
                (list
                 (list 2 3)
                 (list 1 2)
                 (list 1 3)
                 (list 1 4)))
               (list
                "normal"
                (list
                 (list 1 1)
                 (list 2 2)
                 (list 2 3)))))
(check-expect (addToType "not normal" sposn6 foods1)
              (list
               (list
                "normal"
                (list
                 (list 1 2)
                 (list 1 3)
                 (list 1 4)))
               (list
                "normal"
                (list
                 (list 1 1)
                 (list 2 2)
                 (list 2 3)))
               (list "not normal" 
                     (list 
                      (list 2 3)))))
(check-expect (addToType "normal" sposn6 empty)
              (cons (list "normal" (list sposn6)) empty))

              
;; placeThing: [Listof SPosn] -> SPosn
;; forms random SPosn and returns one that has no data on it
(define (placeThing lop)
  (placeThing-helper lop 
                     (list 
                      (+ (/ segmentSide 2) 
                         (* (random (/ sceneSize segmentSide)) 
                            segmentSide)) 
                      (+ (/ segmentSide 2) 
                         (* (random 
                             (/ sceneSize segmentSide)) 
                            segmentSide)))))

(check-range (first (placeThing lsposn1)) 0 500)
(check-range (first (placeThing lsposn2)) 0 500)

;;placeThing-helper: [Listof SPosn] SPosn -> SPosn
;; returns a SPosn that doesn't have data on it already
(define (placeThing-helper lop p)
  (cond 
    [(onList? lop p) (placeThing lop)]
    [else p]))

(check-range (first (placeThing-helper lsposn1 sposn1)) 
              0 500)
(check-expect (placeThing-helper lsposn3 sposn1) 
              (list 1 2))

;; onList?: [Listof SPosn] SPosn -> Boolean
;; returns true if the SPosn is on an item in the list
(define (onList? lop p)
  (cond 
    [(empty? lop) false]
    [else 
     (cond
       [(equal? (first lop) p) true]
       [else (onList? (rest lop) p)])]))

(check-expect (onList? lsposn1 sposn1) true)
(check-expect (onList? lsposn1 sposn6) false)

;; playerMoved: Universe iworld SPosn -> Universe
;; to tell all the Players where a player moved
(define (playerMoved univ wrld p)
  (list (findPlayer 
         (loPlayers univ) wrld p addHead) (loFood univ)))

(check-expect (playerMoved univ1 iworld1 sposn1)
              (list
               (list
                (list
                 iworld1
                 (list
                  (list 1 2)
                  (list 1 2)
                  (list 1 3))
                 "playing"
                 "left")
                (list
                 iworld2
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4))
                 "quit"
                 "left"))
               (list
                (list
                 "normal"
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4)))
                (list
                 "normal"
                 (list
                  (list 1 1)
                  (list 2 2)
                  (list 2 3))))))

;; findPlayer: [Listof Player] iworld Data 
;; [Player Data -> Player] -> [Listof Player]
;; to find a player and perform a function on it
(define (findPlayer lop wrld d func)
  (cond
    [(empty? lop) empty]
    [else 
     (cond
       [(iworld=? wrld (playerName (first lop))) 
        (cons (func (first lop) d) (rest lop))]
       [else (cons (first lop) 
                   (findPlayer (rest lop) wrld d func))])]))

;; (check-expect (findPlayer players1 iworld1 ) players1)  

;; addTail: Player SPosn -> Player
;; to add a segment and temporarily put it on the head of the snake
(define (addTail plyr p)
  (list (playerName plyr) 
        (append (loPlayerSPosn plyr) (list p)) 
        (gameState plyr) (direction plyr)))

;; addHead: Player SPosn -> Player
;; to add a SPosn to the front of a list of SPosn
(define (addHead plyr p)
  (list (playerName plyr) 
        (cons p (removeTail (loPlayerSPosn plyr))) 
        (gameState plyr) (direction plyr)))

;; removeTail: [Listof SPosn] -> [Listof SPosn]
;; to remove the last item of the list of SPosns
(define (removeTail lop)
  (cond
    [(empty? (rest lop)) empty]
    [else (cons (first lop) (removeTail (rest lop)))]))

;; findPath:  SPosn Direction -> SPosn
;; return the SPosn in the direction after the given SPosn
(define (findPath p dir)
  (cond
    [(string=? dir "left") 
     (list (- (x p) segmentSide) (y p))]
    [(string=? dir "right") 
     (list (+ (x p) segmentSide) (y p))]
    [(string=? dir "up") 
     (list (x p) (- (y p) segmentSide))]
    [else 
     (list (x p) (+ (y p) segmentSide))]))

(check-expect (first (findPath sposn1 "left")) -19)
(check-expect (first (findPath sposn1 "right")) 21)
(check-expect (first (findPath sposn1 "up")) 1)
(check-expect (first (findPath sposn1 "down")) 1)


;; movePlayers: Universe -> Result 
;; to move all the players
(define (movePlayers univ)
  (sendBundle 
   (loPlayers univ) 
   (won? (loPlayers univ) 
         (makePlaying 
          (traversePlayers (loPlayers univ) univ)))))



;; traversePlayers: [Listof Player] Universe -> Universe
;; to check each player for collisions
(define (traversePlayers lop univ)
  (cond 
    [(empty? lop) univ]
    [(and (string? (gameState (first lop))) 
          (string=? "playing" (gameState (first lop)))) 
     (traversePlayers 
      (rest lop) 
      (movedState 
       (checkCollision univ 
                       (findPath 
                        (first (loPlayerSPosn (first lop))) 
                        (direction (first lop))) 
                       (playerName (first lop))) 
       (playerName (first lop))))]
    [else (traversePlayers (rest lop) univ)]))


;; uses random numbers that are tested in smaller 
;; helper functiosn

;; makePlaying: Universe -> Universe
;; change all the players back to "playing" game 
;; state or counts them down until they are playing
(define (makePlaying univ)
  (list (makePlaying-helper (loPlayers univ)) (loFood univ)))

(check-expect (makePlaying univ1)
              (list
               (list
                (list
                 iworld1
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4))
                 "playing"
                 "left")
                (list
                 iworld2
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4))
                 "playing"
                 "left"))
               (list
                (list
                 "normal"
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4)))
                (list
                 "normal"
                 (list
                  (list 1 1)
                  (list 2 2)
                  (list 2 3))))))


;; makePlaying-helper: Universe [Listof Player] -> 
;; [Listof Player]
;; goes through all the players and changes the states

(define (makePlaying-helper lop) 
  (cond
    [(empty? lop) empty]
    [(and (number? (gameState (first lop))) 
          (< 0 (gameState (first lop)))) 
     (cons (list (playerName 
                  (first lop)) 
                 (loPlayerSPosn (first lop)) 
                 (sub1 (gameState (first lop))) 
                 (direction (first lop))) 
           (makePlaying-helper (rest lop)))]
    [else
     (cons 
      (list (playerName (first lop)) 
            (loPlayerSPosn (first lop)) 
            "playing" (direction (first lop))) 
      (makePlaying-helper (rest lop)))]))



;; checkCollision: Universe SPosn iworld -> Universe
;; to return the universe after the collisions
(define (checkCollision univ p wrld)
  (cond 
    [(onList? (appendFoods (loFood univ)) p) 
     (placeFood 
      (playerMoved (growPlayer (removeFood univ p) wrld) wrld p))]
    [(onList? (playerHeads (loPlayers univ)) p) 
     (remove2 univ wrld (headsCollide (loPlayers univ) p))]
    [(or (hitWall? p) 
         (onList? (appendPlayers (loPlayers univ)) p)) 
     (placePlayer (removePlayer univ wrld) wrld)]
    [else (playerMoved univ wrld p)]))

;; contains randomly generated numbers that are tested elsewhere 


;;  movedState: Universe iworld -> Universe
;; to return the universe with the player state changed to "moved"
(define (movedState univ wrld)
  (list (findPlayer (loPlayers univ) wrld "moved" makeMoved) 
        (loFood univ)))

(check-expect (movedState univ1 iworld1)
              (list
               (list
                (list
                 iworld1
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4))
                 "moved"
                 "left")
                (list
                 iworld2
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4))
                 "quit"
                 "left"))
               (list
                (list
                 "normal"
                 (list
                  (list 1 2)
                  (list 1 3)
                  (list 1 4)))
                (list
                 "normal"
                 (list
                  (list 1 1)
                  (list 2 2)
                  (list 2 3))))))

;; makeMoved: Player State -> Player
;; changes the players state to "moved" when not waiting
(define (makeMoved plyr s)
  (cond
    [(number? (gameState plyr)) plyr]
    [else
     (list (playerName plyr) (loPlayerSPosn plyr) s (direction plyr))]))

(check-expect (makeMoved player6 "playing")
              (list
               iworld3
               (list (list 1 1) (list 2 2) (list 2 3))
               2
               "left"))
(check-expect (makeMoved player1 "playing")
              (list
               iworld1
               (list (list 1 2) (list 1 3) (list 1 4))
               "playing"
               "left"))

;; remove2: Universe iworld iworld -> Universe
;; removes two players and resets them randomly in a different spot
(define (remove2 univ wrld1 wrld2)
  (placePlayer 
   (removePlayer (placePlayer 
                  (removePlayer univ wrld1) wrld1) wrld2) wrld2))

;; uses random numbers that are tested elsewhere 
 

;; playerHeads: [Listof Players] -> [Listof SPosn]
;; compile a list of the SPosns that are player's heads
(define (playerHeads lop)
  (cond
    [(empty? lop) empty]
    [else
     (cond
       [(number? (gameState (first lop))) 
        (playerHeads (rest lop))]
       [else (cons (first (loPlayerSPosn (first lop))) 
                   (playerHeads (rest lop)))])]))

(check-expect (playerHeads players1) (list (list 1 2) (list 1 2)))
(check-expect (playerHeads players2) (list (list 1 1) (list 1 2)))
(check-expect (playerHeads players5) empty)


;; headsCollide: [Listof Players] SPosn -> iworld
;; returns the iworld of the player that collided
(define (headsCollide lop p)
  (cond 
    [(empty? (rest lop)) 
     (playerName (first lop))]
    [(equal? (first (loPlayerSPosn (first lop))) p) 
     (playerName (first lop))]
    [else (playerName (first lop))]))

(check-expect (headsCollide players1 sposn1) iworld1)
(check-expect (headsCollide (list sposn1) sposn1) 1)
(check-expect (headsCollide players1 sposn6) iworld1)

;; empty Universe
(define u (list '() '()))

;; universe
(define (server universe1)
  (universe universe1
            (on-new add-player)
            (on-msg updatePlayers)
            (on-tick movePlayers TICKRATE)
            (on-disconnect someoneLeft)))