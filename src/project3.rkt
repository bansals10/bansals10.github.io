#lang typed/racket

(require typed/test-engine/racket-tests)
(require (only-in typed/racket/gui/base put-file get-file))

(require "cs151-core.rkt")
(require "cs151-image.rkt")
(require "cs151-universe.rkt")

;; data definitions

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Stone
  (U 'black 'white))

(define-struct LogicalLoc
  ([col : Integer]
   [row : Integer]))

;; Each entry in these vectors is an (Optional Stone),
;; representing the color of stone,
;; or the absence of a stone, at a given location.
(define-type Board
  (Vectorof (Vectorof (Optional Stone))))

(define-struct Go
  ([board : Board]
   [next-to-play : Stone]
   [history : (Listof Board)]
   [last-turn-place : (Optional LogicalLoc)]
   [last-turn-opp-captures : (Listof LogicalLoc)]
   [last-turn-self-captures : (Listof LogicalLoc)]
   [consecutive-passes : Integer]))

(define-struct PhysicalLoc
  ([x-offset-from-left : Integer]
   [y-offset-from-top  : Integer]))

(define-struct BoardSpec
  ([background-color : Image-Color]
   [cell-size-pixels : Integer]
   [margin-pixels : Integer]
   [stone-radius-pixels : Integer]))

(define-struct World
  ([spec : BoardSpec]
   [game : Go]
   [status-message : String]
   [black-tenths : Integer]
   [white-tenths : Integer]
   [hover : (Optional LogicalLoc)]))

(define-struct LocDim
  ([spec : BoardSpec]
   [dim : Integer]
   [i : Integer]
   [logloc : LogicalLoc]))

(define-struct Territory
  ([locations : (Listof LogicalLoc)]
   [color : Symbol]))

(define-struct Outcome
  ([black  : Integer]
   [white  : Integer]
   [winner : (U Stone 'draw)]))


;; test BoardSpec
(define test-spec1 (BoardSpec 'moccasin 15 5 3))
(define test-spec2 (BoardSpec 'moccasin 30 15 9))
(define test-spec3 (BoardSpec 'purple 10 10 7))

;; test Board
(: board1 : Board)
(define board1 (vector (vector 'None (Some 'black) 'None)
                       (vector (Some 'black) (Some 'white) (Some 'white))
                       (vector 'None 'None 'None)))
(: board2 : Board)
(define board2 (vector (vector 'None (Some 'black) 'None)
                       (vector (Some 'white) (Some 'white) (Some 'white))
                       (vector 'None 'None 'None)))
(: board3 : Board)
(define board3 (vector (vector 'None 'None) (vector 'None 'None)))

(: board4 : Board)
(define board4 (vector
                (vector (Some 'black) 'None (Some 'white) 'None)
                (vector (Some 'black) (Some 'black) (Some 'white) 'None)
                (vector (Some 'black) (Some 'white) (Some 'white)
                        (Some 'black))
                (vector (Some 'black) (Some 'white) (Some 'white)
                        'None)))

(: board5 : Board)
(define board5 (vector
                (vector (Some 'black) (Some 'black) (Some 'white) 'None)
                (vector (Some 'black) (Some 'black) (Some 'white) 'None)
                (vector (Some 'black) (Some 'white) (Some 'white)
                        (Some 'black))
                (vector (Some 'black) (Some 'white) (Some 'white) 'None)))

(: board6 : Board)
(define board6 (vector
                (vector (Some 'white) (Some 'white) (Some 'black))
                (vector (Some 'white) (Some 'white) (Some 'black))
                (vector (Some 'black) (Some 'black) 'None)))

(: boardSingle : Board)
(define boardSingle
  (vector (vector (Some 'white) (Some 'black) (Some 'black))
          (vector (Some 'black) 'None (Some 'black))
          (vector (Some 'black) (Some 'black) 'None)))

(: explore-board : Board)
(define explore-board (vector (vector 'None (Some 'white) 'None)
                              (vector 'None (Some 'white) 'None)
                              (vector (Some 'white) 'None (Some 'black))))

;; test go
(define go1 (Go board1 'black '() 'None '() '() 0))
(define go2 (Go board2 'black '() (Some (LogicalLoc 1 0))
                (list (LogicalLoc 0 0)) (list (LogicalLoc 1 1)) 0))
(define go3 (Go board3 'black '() 'None '() '() 0))
(define go4 (Go board4 'black
                (list (vector
                       (vector (Some 'white) 'None (Some 'white) 'None)
                       (vector (Some 'black) (Some 'black) (Some 'white) 'None)
                       (vector (Some 'black) (Some 'white) (Some 'white)
                               (Some 'black))
                       (vector (Some 'black) (Some 'white) (Some 'white) 'None))
                      (vector
                       (vector (Some 'black) (Some'black) (Some 'white) 'None)
                       (vector (Some 'black) (Some 'black) (Some 'white) 'None)
                       (vector (Some 'black) (Some 'white) (Some 'white)
                               (Some 'black))
                       (vector (Some 'black) (Some 'white) (Some 'white)
                               'None)))
                'None '() '() 0))

;; test world
(define world1 (World test-spec2 go1 "test-message" 5 10 'None))
(define world2 (World test-spec2 go2 "test-message" 30 10 (Some (LogicalLoc 0 0))))

;; Converts a given logical location to its corresponding physical location
;; The integer argument is the dimension (locations per side) of the board
(: logical->physical : LogicalLoc Integer BoardSpec -> PhysicalLoc)
(define (logical->physical logloc dim board)
  (match* (logloc board)
    [((LogicalLoc col row) (BoardSpec b c m _))
     (if (or (< col 0) (< row 0) (>= col dim) (>= row dim))
         (error "off the board")
         (PhysicalLoc (+ m (* c col)) (+ m (* c (- dim row 1)))))]))

(check-error (logical->physical (LogicalLoc 0 -4) 4 test-spec1) "off the board")
(check-expect
 (logical->physical (LogicalLoc 1 1) 3 (BoardSpec "brown" 10 12 4))
 (PhysicalLoc 22 22))
(check-expect
 (logical->physical (LogicalLoc 1 1) 4 (BoardSpec "brown" 10 12 3))
 (PhysicalLoc 22 32))
(check-expect
 (logical->physical (LogicalLoc 1 0) 3 (BoardSpec "brown" 10 12 3))
 (PhysicalLoc 22 32))
(check-expect
 (logical->physical (LogicalLoc 0 1) 3 (BoardSpec "brown" 10 12 3))
 (PhysicalLoc 12 22))
(check-expect
 (logical->physical (LogicalLoc 0 0) 19 (BoardSpec "brown" 10 10 5))
 (PhysicalLoc 10 190))
(check-expect
 (logical->physical (LogicalLoc 7 6) 19 (BoardSpec "brown" 10 10 3))
 (PhysicalLoc 80 130))
(check-expect
 (logical->physical (LogicalLoc 7 6) 10 (BoardSpec "brown" 10 10 3))
 (PhysicalLoc 80 40))
(check-expect
 (logical->physical (LogicalLoc 4 9) 10 (BoardSpec "brown" 10 5 3))
 (PhysicalLoc 45 5))
(check-expect
 (logical->physical (LogicalLoc 1 1) 19 (BoardSpec "brown" 10 5 3))
 (PhysicalLoc 15 175))

;; Taken from Professor Shaw's Code
;;`````````````````````````````````````````````````````````````````````````````

;; calculates the distance between 2 physical locations
(: distance : Integer Integer Integer Integer -> Real)
(define (distance x0 y0 x1 y1)
  (local {(define dx (- x0 x1))
          (define dy (- y0 y1))}
    (sqrt (+ (* dx dx) (* dy dy)))))

(check-expect (distance 0 0 10 0) 10)
(check-expect (distance 3 0 0 4) 5)

;; There is no right end to the number line -- it extends forever.
(: snap-to-grid : Integer Integer Real -> Integer)
(define (snap-to-grid margin gap-size x)
  (if (>= x 0)
      (+ margin (* gap-size (exact-round (/ (- x margin) gap-size))))
      margin))

(check-expect (snap-to-grid 5 1 4.9) 5)
(check-expect (snap-to-grid 5 1 5.1) 5)
(check-expect (snap-to-grid 5 1 5.6) 6)
(check-expect (snap-to-grid 5 1 5.9) 6)
(check-expect (snap-to-grid 5 1 9.9) 10)
(check-expect (snap-to-grid 5 5 4.9) 5)
(check-expect (snap-to-grid 5 5 5.9) 5)
(check-expect (snap-to-grid 5 5 9.9) 10)
(check-expect (snap-to-grid 5 5 -99999) 5)

;; on-the-board? checks if a logical location is on the board, given dimension
(: on-the-board? : Integer LogicalLoc -> Boolean)
(define (on-the-board? dim lloc)
  (match lloc
    [(LogicalLoc x y) (and (<= 0 x (sub1 dim)) (<= 0 y (sub1 dim)))]))

;; compares radius to distance in order to convert from physical location to  
;; its logical location
;; the integer argument is the dimension (locations per side) of the board
(: physical->logical : PhysicalLoc Integer BoardSpec -> (Optional LogicalLoc))
(define (physical->logical ploc dim spec)
  (match ploc
    [(PhysicalLoc x y)
     (match spec
       [(BoardSpec _ cell margin stone)
        (local
          {(define nearx (snap-to-grid margin cell x))
           (define neary (snap-to-grid margin cell y))
           (define lloc-candidate
             (LogicalLoc (quotient (- nearx margin) cell)
                         (quotient (- (* (sub1 dim) cell)
                                      (- neary margin)) cell)))}
          (if (and (on-the-board? dim lloc-candidate)
                   (<= (distance x y nearx neary) stone))
              (Some lloc-candidate)
              'None))])]))

;;`````````````````````````````````````````````````````````````````````````````

(check-expect
 (physical->logical (PhysicalLoc 22 32) 4 (BoardSpec "brown" 10 12 3))
 (Some (LogicalLoc 1 1)))
(check-expect
 (physical->logical (PhysicalLoc 22 32) 3 (BoardSpec "brown" 10 12 3))
 (Some (LogicalLoc 1 0)))
(check-expect
 (physical->logical (PhysicalLoc 12 22) 3 (BoardSpec "brown" 10 12 3))
 (Some (LogicalLoc 0 1)))
(check-expect
 (physical->logical (PhysicalLoc 10 190) 19 (BoardSpec "brown" 10 10 5))
 (Some (LogicalLoc 0 0)))
(check-expect
 (physical->logical (PhysicalLoc 80 130) 19 (BoardSpec "brown" 10 10 3))
 (Some (LogicalLoc 7 6)))
(check-expect
 (physical->logical (PhysicalLoc 80 40) 10 (BoardSpec "brown" 10 10 3))
 (Some (LogicalLoc 7 6)))
(check-expect
 (physical->logical (PhysicalLoc 45 5) 10 (BoardSpec "brown" 10 5 3))
 (Some (LogicalLoc 4 9)))
(check-expect
 (physical->logical (PhysicalLoc 15 175) 19 (BoardSpec "brown" 10 5 3))
 (Some (LogicalLoc 1 1)))
(check-expect
 (physical->logical (PhysicalLoc 12 12) 10 (BoardSpec "brown" 10 10 5))
 (Some (LogicalLoc 0 9))) 
(check-expect
 (physical->logical (PhysicalLoc 0 0) 19 (BoardSpec "brown" 10 5 3)) 'None)
(check-expect
 (physical->logical (PhysicalLoc 3 13) 19 (BoardSpec "brown" 10 5 3))
 (Some (LogicalLoc 0 17)))
(check-expect
 (physical->logical (PhysicalLoc 33 46) 4 (BoardSpec "brown" 12 16 3)) 'None)
(check-expect
 (physical->logical (PhysicalLoc 31 16) 10 (BoardSpec "brown" 10 5 4)) 'None)
(check-expect
 (physical->logical (PhysicalLoc 10 50) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 0 0)))
(check-expect
 (physical->logical (PhysicalLoc 20 50) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 1 0)))
(check-expect
 (physical->logical (PhysicalLoc 20 40) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 1 1)))
(check-expect
 (physical->logical (PhysicalLoc 50 10) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 4 4)))

;; creates a list of letters from a start value to an end value corresponding
;; with the ASCII table
(: list-letters : Integer Integer -> (Listof Char))
(define (list-letters s e)
  (if (< s e) (cons (integer->char s) (list-letters (+ s 1) e))
      (list (integer->char s))))

(check-expect (list-letters 65 70) (list #\A #\B #\C #\D #\E #\F))
(check-expect (list-letters 69 72) (list #\E #\F #\G #\H))

;; Converts logical locations to strings such as "A1", "B3", etc.
(: logical->string : LogicalLoc -> String)
(define (logical->string logloc)
  (match logloc
    [(LogicalLoc col row)
     (string-append
      (make-string
       (+ 1 (quotient col 25)) (list-ref
                                (append (list-letters 65 72)
                                        (list-letters 74 90))
                                (- col (* 25 (quotient col 25)))))
      (number->string (+ 1 row)))]))

(check-expect (logical->string (LogicalLoc 0 2)) "A3")
(check-expect (logical->string (LogicalLoc 25 2)) "AA3")
(check-expect (logical->string (LogicalLoc 3 3)) "D4")
(check-expect (logical->string (LogicalLoc 52 3)) "CCC4")

;; Returns the stone at the specified location on the board, or indicates
;; it is unoccupied
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref g logloc)
  (match* (g logloc)
    [((Go board next _ _ _ _ _) (LogicalLoc col row))
     (vector-ref (vector-ref board col) row)]))

;; Returns the stone at the specified location on the board, or indicates
;; it is unoccupied
(: bd-ref  : Board LogicalLoc -> (Optional Stone))
(define (bd-ref board logloc)
  (match logloc
    [(LogicalLoc col row) (vector-ref (vector-ref board col) row)]))

(check-expect (bd-ref board1 (LogicalLoc 0 1)) (Some 'black))
(check-expect (bd-ref board2 (LogicalLoc 1 0)) (Some 'white))
(check-expect (bd-ref board2 (LogicalLoc 0 1)) (Some 'black))
(check-expect (bd-ref board2 (LogicalLoc 0 2)) 'None)

; modifies a Go board to store the specified stone (or no stone at all) at the
; given location.
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! g logloc stone)
  (match* (g logloc)
    [((Go board next _ _ _ _ _) (LogicalLoc col row))
     (vector-set! (vector-ref board col) row stone)]))
                          

;; modifies a board to store the specified stone (or no stone at all) at the
;; given location.
(: bd-set! : Board LogicalLoc (Optional Stone) -> Void)
(define (bd-set! board logloc stone)
  (match logloc
    [(LogicalLoc col row)
     (vector-set! (vector-ref board col) row stone)]))

(: boardTest1 : Board)
(define boardTest1 (vector (vector 'None 'None (Some 'black))
                           #(None None None)
                           #(None None None)))
(bd-set! boardTest1 (LogicalLoc 0 0) (Some 'white))
(check-expect boardTest1 (vector (vector (Some 'white) 'None (Some 'black))
                                 #(None None None)
                                 #(None None None)))

(: boardTest2 : Board)
(define boardTest2 (vector (vector 'None 'None (Some 'black))
                           #(None None None)
                           #(None None None)))
(bd-set! boardTest2 (LogicalLoc 0 2) 'None)
(check-expect boardTest2 (vector #(None None None)
                                 #(None None None)
                                 #(None None None)))


;; index=?: checks one index of board for equality
(: index=? : Board Board Integer Integer -> Boolean)
(define (index=? b1 b2 col row)
  (match* ((vector-ref (vector-ref b1 col) row)
           (vector-ref (vector-ref b2 col) row))
    [((Some a) (Some b)) (symbol=? a b)]
    [('None 'None) #t]
    [(_ _) #f]))

(check-expect (index=? board1 board2 1 0) #f)
(check-expect (index=? board1 board2 0 0) #t)
(check-expect (index=? board1 board2 2 2) #t)

;; board=? : determines if the two given boards are equal
(: board=? : Board Board -> Boolean)
(define (board=? b1 b2)
  (local {(: checkRow : Integer Integer -> Boolean)
          (define (checkRow col row)
            (cond
              [(= col (vector-length b1)) #t]
              [(= row (vector-length b1)) (checkRow (add1 col) 0)]
              [else (if (index=? b1 b2 col row)
                        (checkRow col (add1 row)) #f)]))}
    (checkRow 0 0)))

(check-expect (board=? board1 board2) #f)

(check-expect (board=? board2
                       (vector (vector 'None (Some 'black) 'None)
                               (vector (Some 'white) (Some 'white)
                                       (Some 'white))
                               (vector 'None 'None 'None))) #t)
(check-expect (board=? (vector (vector 'None (Some 'black))
                               (vector (Some 'white) (Some 'white)))
                       (vector (vector 'None (Some 'black))
                               (vector (Some 'white) (Some 'white)))) #t)
(check-expect (board=? board3
                       (vector (vector 'None 'None)
                               (vector 'None 'None))) #t)

;; empty-board : creates the initial empty board
(: empty-board : Integer -> Board)
(define (empty-board dim)
  (build-vector dim
                (lambda ([i : Integer])
                  (make-vector dim (cast 'None (Optional Stone))))))

(check-expect (empty-board 2) '#(#(None None)
                                 #(None None)))
(check-expect (empty-board 3)
              (vector (vector 'None 'None 'None)
                      (vector 'None 'None 'None)
                      (vector 'None 'None 'None)))
(check-expect (empty-board 5)
              '#(#(None None None None None)
                 #(None None None None None)
                 #(None None None None None)
                 #(None None None None None)
                 #(None None None None None)))

;; board-copy: make a completely independent copy of a board, and use this upon
;; storing boards in the history.
(: board-copy : Board -> Board)
(define (board-copy board)
  (local
    {(define len (vector-length board))}
    (local
      {(define new-board (empty-board len))
       (: lp : Integer Integer -> Board)
       (define (lp row col)
         (cond
           [(= col len) new-board]
           [(= row len) (lp 0 (add1 col))]
           [else (begin (bd-set! new-board (LogicalLoc col row)
                                 (bd-ref board (LogicalLoc col row)))
                        (lp (add1 row) col))]))}
      (lp 0 0))))

(check-expect (board-copy board1)
              (vector
               (vector 'None (Some 'black) 'None)
               (vector (Some 'black) (Some 'white) (Some 'white))
               (vector 'None 'None 'None)))
(check-expect (board-copy board2)
              (vector
               (vector 'None (Some 'black) 'None)
               (vector (Some 'white) (Some 'white) (Some 'white))
               (vector 'None 'None 'None)))
(check-expect (board-copy board4)
              (vector
               (vector (Some 'black) 'None (Some 'white) 'None)
               (vector (Some 'black) (Some 'black) (Some 'white) 'None)
               (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
               (vector (Some 'black) (Some 'white) (Some 'white) 'None)))

;; A board spec is valid if the cell size, margin, and stone radius
;; are all positive; the stone radius is less than half the cell size;
;; and the margin exceeds the stone radius.
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? spec)
  (match spec
    [(BoardSpec _ c m s)
     (and (andmap positive? (list c m s)) (> m s) (< s (/ c 2)))]))

(check-expect (valid-board-spec? (BoardSpec "brown" 10 5 3)) #t)
(check-expect (valid-board-spec? (BoardSpec "blue" -10 5 3)) #f)
(check-expect (valid-board-spec? (BoardSpec "blue" 10 -5 3)) #f)
(check-expect (valid-board-spec? (BoardSpec "blue" 10 5 -3)) #f)
(check-expect (valid-board-spec? (BoardSpec "blue" 4 5 3)) #f)
(check-expect (valid-board-spec? (BoardSpec "blue" 10 1 3)) #f)

;; creates grid based on given specifications
(: grid : BoardSpec Integer -> Image)
(define (grid spec dim)
  (match spec
    [(BoardSpec b c m _)
     (local {(: gen-block : (Image Image -> Image) Image Integer -> Image)
             (define (gen-block arrange img n)
               (foldr arrange empty-image (make-list n img)))}
       (overlay
        (gen-block above (gen-block beside (square c "outline" 'black)
                                    (- dim 1)) (- dim 1))
        (square (+ (* (- dim 1) c) m m) "solid" b)))]))

;; eyeball test
(grid test-spec1 10)
(grid test-spec2 5)

;; determines of c is within byte range
(: int->text-size : Integer -> Byte)
(define (int->text-size n)
  (cond
    [((make-predicate Byte) n) n]
    [else 255]))

(check-expect (int->text-size 100) 100)
(check-expect (int->text-size 350) 255)

;; produces an x label for a given LocDim
(: x-lab : LocDim -> Image)
(define (x-lab ld)
  (match ld
    [(LocDim spec dim x logloc)
     (match* (spec logloc)
       [((BoardSpec b c m s) (LogicalLoc col row))
        (place-image
         (text (make-string (+ 1 (quotient col 25))
                            (list-ref (append (list-letters 65 72)
                                              (list-letters 74 90))
                                      (- col (* 25 (quotient col 25)))))
               (int->text-size (quotient c 2)) "black")
         (+ m (* c x)) (/ c 2) (rectangle (+ (* c (- dim 1)) m m) c
                                          "outline" "white"))])]))
;; eyeball tests
(x-lab (LocDim test-spec1 5 3 (LogicalLoc 5 5)))
(x-lab (LocDim test-spec2 20 3 (LogicalLoc 10 15)))

;; produces a y label for a given LocDim
(: y-lab : LocDim -> Image)
(define (y-lab ld)
  (match ld
    [(LocDim spec dim y logloc)
     (match* (spec logloc)
       [((BoardSpec b c m s) (LogicalLoc col row))
        (place-image
         (text (number->string (add1 row))
               (int->text-size (quotient c 2)) "black")
         (/ c 2) (+ m (* c (- dim y 1)))
         (rectangle c (+ (* c (- dim 1)) m m) "outline" "white"))])]))

;; eyeball test
(y-lab (LocDim test-spec2 5 3 (LogicalLoc 5 5)))

;; creates a list of LocDim
(: list-of-LocDim : Integer Integer BoardSpec -> (Listof LocDim))
(define (list-of-LocDim i dim spec)
  (if (< i dim) (cons (LocDim spec dim i (LogicalLoc i i))
                      (list-of-LocDim (add1 i) dim spec))
      (list (LocDim spec dim i (LogicalLoc i i)))))

(check-expect (list-of-LocDim 2 5 test-spec3)
              (list
               (LocDim test-spec3 5 2 (LogicalLoc 2 2))
               (LocDim test-spec3 5 3 (LogicalLoc 3 3))
               (LocDim test-spec3 5 4 (LogicalLoc 4 4))
               (LocDim test-spec3 5 5 (LogicalLoc 5 5))))

;; make-lab: creates the labels for the grid
(: make-lab : (LocDim -> Image) (Listof LocDim) -> Image)
(define (make-lab f ls)
  (match ls
    ['() empty-image]
    [(cons h t) (overlay (f h) (make-lab f t))]))

;; eyeball test
(make-lab x-lab (list
                 (LocDim test-spec3 5 2 (LogicalLoc 2 2))
                 (LocDim test-spec3 5 3 (LogicalLoc 3 3))
                 (LocDim test-spec3 5 4 (LogicalLoc 4 4))
                 (LocDim test-spec3 5 5 (LogicalLoc 5 5))))
(make-lab x-lab '())

;; draws the grid and labels
(: draw-board : Go BoardSpec -> Image)
(define (draw-board game spec)
  (match game
    [(Go board next _ _ _ _ _)
     (local {(define dim (vector-length board))}
       (above/align "left" (beside (grid spec dim)
                                   (make-lab y-lab (list-of-LocDim 0 dim spec)))
                    (make-lab x-lab (list-of-LocDim 0 dim spec))))]))

;; eyeball test
(draw-board (Go (empty-board 10) 'black '() 'None '() '() 0) test-spec2)

;; draws the pieces onto the board based on their logical locations
(: draw-circles : Go BoardSpec Image -> Image)
(define (draw-circles go spec background)
  (local
    {(define dim (vector-length (Go-board go)))
     (define new-board background)
     (: lp : Integer Integer -> Image)
     (define (lp row col)
       (cond
         [(= col dim) new-board]
         [(= row dim) (lp 0 (add1 col))]
         [else
          (place-image
           (match (board-ref go (LogicalLoc col row))
             ['None empty-image]
             [(Some a) (circle (BoardSpec-stone-radius-pixels spec) "solid"
                               (symbol->string a))])
           (PhysicalLoc-x-offset-from-left
            (logical->physical (LogicalLoc col row) dim spec))
           (PhysicalLoc-y-offset-from-top (logical->physical
                                           (LogicalLoc col row) dim spec))
           (lp (add1 row) col))]))}
    (lp 0 0)))

;; eyeball test
(draw-circles go1 test-spec2 (draw-board go1 test-spec2))
(draw-circles go2 test-spec2 (draw-board go2 test-spec2))

;; format-tenths : takes in tenths of a second and produces string
;; representation
(: format-tenths : Integer -> String)
(define (format-tenths t)
  (string-append
   (number->string (quotient t 600))
   ":"
   (if (< (quotient (remainder t 600) 10) 10) "0" "")
   (number->string (quotient (remainder t 600) 10))
   "."
   (number->string (remainder (remainder t 600) 10))))

(check-expect (format-tenths 1234) "2:03.4")
(check-expect (format-tenths 6000) "10:00.0")

;; draws timer
(: draw-timer : World -> Image)
(define (draw-timer w)
  (match w 
    [(World spec game _ b-tenths w-tenths _)
     (match* (spec game)
       [((BoardSpec b c m r) (Go board _ _ _ _ _ _))
        (local
          {(define dim (vector-length board))}
          (overlay/align "left" "middle"
                         (above (text (format-tenths (+  b-tenths w-tenths))
                                      (int->text-size (+ 1 (quotient c 2)))
                                      "black")
                                (text "Total Time Elapsed"
                                      (int->text-size (+ 1 (quotient c 2)))
                                      "black"))
                         (overlay/align "middle" "middle"
                                        (above (text (format-tenths b-tenths)
                                                     (int->text-size
                                                      (+ 1 (quotient c 2)))
                                                     "black")
                                               (text "Black Time Elapsed"
                                                     (int->text-size
                                                      (+ 1 (quotient c 2)))
                                                     "black"))
                                        (overlay/align "right" "middle"
                                                       (above
                                                        (text
                                                         (format-tenths
                                                          w-tenths)
                                                         (int->text-size
                                                          (+ 1 (quotient c 2)))
                                                         "black")
                                                        (text
                                                         "White Time Elapsed"
                                                         (int->text-size
                                                          (+ 1 (quotient c 2)))
                                                         "black"))
                                                       (rectangle
                                                        (+ (* c (- dim 1)) m m)
                                                        c
                                                        "solid" b)))))])]))

(define world-emp
  (World test-spec2
         (Go (empty-board 19) 'black '() 'None '() '() 0)
         "test-message" 30 10 (Some (LogicalLoc 0 0))))

;; eyeball test
(draw-timer world-emp)

;; A second new GUI feature is a transparent stone that appears under the mouse
;; cursor when it is hovering over an intersection. This image should appear
;; when the mouse cursor is within a stone's radius of an intersection,
;; and the stone it depicts should be "snapped" to the intersection
;; (centered on the intersection, as it would appear if you clicked and placed a
;; stone there). No "ghost" stone should be shown if the mouse is not
;; sufficiently close to an intersection, or if the intersection near the mouse
;; would not permit a legal move for the current player.
(: draw-hover : Go BoardSpec LogicalLoc -> Image)
(define (draw-hover go spec logloc)
  (match go
    [(Go board next _ _ _ _ _)
     (local
       {(define dim (vector-length (Go-board go)))}
       (place-image
        (circle (BoardSpec-stone-radius-pixels spec) 128
                (symbol->string next))
        (PhysicalLoc-x-offset-from-left
         (logical->physical logloc dim spec))
        (PhysicalLoc-y-offset-from-top (logical->physical
                                        logloc dim spec))
        (draw-circles go spec (draw-board go spec))))]))

;; eyeball tests
(draw-hover go1 test-spec2 (LogicalLoc 0 0))
(draw-hover go2 test-spec2 (LogicalLoc 0 0))

;; The next feature is to show where the stone was placed during the last turn.
;; Modify your code in the appropriate place to record where the last move has
;; occurred, and update your drawing function to visually indicate the location
;; of the last move in the vicinity of that part of the board. You have
;; discretion as to the exact appearance, so long as it is graphical and not
;; textual and is overlaid on the board itself in the vicinity of the relevant
;; intersection. This visual indicator should disappear immediately upon a pass.
(: draw-last : Go BoardSpec Image -> Image)
(define (draw-last go spec img)
  (match go
    [(Go board next _ l-place _ _ _)
     (match l-place
       ['None img]
       [(Some last)
        (local
          {(define dim (vector-length (Go-board go)))}
          (place-image
           (circle (BoardSpec-stone-radius-pixels spec) "outline"
                   (make-pen "purple" 2 "solid" "round" "round"))
           (PhysicalLoc-x-offset-from-left
            (logical->physical last dim spec))
           (PhysicalLoc-y-offset-from-top (logical->physical
                                           last dim spec))
           img))])]))

;; eyeball test
(draw-last go2 test-spec2 (draw-circles go2 test-spec2
                                        (draw-board go2 test-spec2)))

;; Provides similar visual cues at the intersections where just-captured
;; stones used to be before the last turn. 
(: draw-capture : Go BoardSpec LogicalLoc Image-Color Image -> Image)
(define (draw-capture go spec logloc color img)
  (local
    {(define dim (vector-length (Go-board go)))}
    (place-image
     (circle (BoardSpec-stone-radius-pixels spec) "outline"
             (make-pen color 2 "solid" "round" "round"))
     (PhysicalLoc-x-offset-from-left
      (logical->physical logloc dim spec))
     (PhysicalLoc-y-offset-from-top (logical->physical
                                     logloc dim spec))
     img)))

(draw-capture go1 test-spec2 (LogicalLoc 1 1) 'black
              (draw-last go1 test-spec2 (draw-circles go2 test-spec2
                                                      (draw-board go2
                                                                  test-spec2))))

;; draw-captures : outlines captured stones from previous round
(: draw-captures : (Listof LogicalLoc) (Listof LogicalLoc) Go BoardSpec Image
   -> Image)
(define (draw-captures l-o-cap l-s-cap go spec img)
  (match l-o-cap
    ['() (foldr (lambda ([ll : LogicalLoc] [i : Image])
                  (draw-capture go spec ll
                                (if (symbol=? (Go-next-to-play go) 'black)
                                    'white 'black) i))
                img l-s-cap)]
    [_ (foldr (lambda ([ll : LogicalLoc] [i : Image])
                (draw-capture go spec ll (Go-next-to-play go) i))
              img l-o-cap)]))

(draw-captures '() (list (LogicalLoc 0 0) (LogicalLoc 0 1) (LogicalLoc 0 2))
               go2 test-spec2
               (draw-last go2 test-spec2
                          (draw-circles go2 test-spec2
                                        (draw-board go2 test-spec2))))

;; check-liberty : determines if a location is empty
(: check-liberty : Board LogicalLoc -> Boolean)
(define (check-liberty board logloc)
  (local
    {(define dim (vector-length board))}
    (match logloc
      [(LogicalLoc col row)
       (if (and (< col dim) (< row dim)
                (>= col 0) (>= row 0))
           (match (bd-ref board logloc)
             ['None #t]
             [_ #f])
           #f)])))

(check-expect (check-liberty board1 (LogicalLoc 0 1)) #f)
(check-expect (check-liberty board1 (LogicalLoc 2 1)) #t)
(check-expect (check-liberty board6 (LogicalLoc 2 2)) #t)
(check-expect (check-liberty board2 (LogicalLoc 1 2)) #f)

;; liberties? : determines if there are any empty around a logical location
(: liberties? : Board LogicalLoc -> Boolean)
(define (liberties? board logloc)
  (match logloc
    [(LogicalLoc col row)
     (ormap (lambda ([l : LogicalLoc]) (check-liberty board l))
            (list (LogicalLoc (add1 col) row)
                  (LogicalLoc (sub1 col) row)
                  (LogicalLoc col (add1 row))
                  (LogicalLoc col (sub1 row))))]))

(check-expect (liberties? board1 (LogicalLoc 0 2)) #f)
(check-expect (liberties? board1 (LogicalLoc 1 0)) #t)
(check-expect (liberties? board1 (LogicalLoc 0 0)) #f)
(check-expect (liberties? board2 (LogicalLoc 2 0)) #t)

;; identify-adj : identifies adjacent locations to a given piece
(: identify-adj : Board LogicalLoc -> (Listof LogicalLoc))
(define (identify-adj board logloc)
  (local {(define acc '())
          (define dim (vector-length board))}
    (match logloc
      [(LogicalLoc col row)
       (append (if (< (add1 col) dim)
                   (cons (LogicalLoc (add1 col) row) acc) acc)
               (if (>= (sub1 col) 0)
                   (cons (LogicalLoc (sub1 col) row) acc) acc)
               (if (< (add1 row) dim)
                   (cons (LogicalLoc col (add1 row)) acc) acc)
               (if (>= (sub1 row) 0)
                   (cons (LogicalLoc col (sub1 row)) acc) acc))])))

(check-expect (identify-adj board6 (LogicalLoc 0 0))
              (list (LogicalLoc 1 0) (LogicalLoc 0 1)))
(check-expect (identify-adj board6 (LogicalLoc 0 2)) (list (LogicalLoc 1 2)
                                                           (LogicalLoc 0 1)))
(check-expect (identify-adj board6 (LogicalLoc 0 1))
              (list (LogicalLoc 1 1) (LogicalLoc 0 2) (LogicalLoc 0 0)))
(check-expect (identify-adj board2 (LogicalLoc 0 1))
              (list (LogicalLoc 1 1) (LogicalLoc 0 2) (LogicalLoc 0 0)))


;; contains? : determines if the location is among the given list of locations
(: contains? : LogicalLoc (Listof LogicalLoc) -> Boolean)
(define (contains? logloc lls)
  (ormap (lambda ([l : LogicalLoc]) (and (= (LogicalLoc-col l)
                                            (LogicalLoc-col logloc))
                                         (= (LogicalLoc-row l)
                                            (LogicalLoc-row logloc)))) lls))
(check-expect
 (contains? (LogicalLoc 0 0)
            (list (LogicalLoc 0 1) (LogicalLoc 1 0) (LogicalLoc 0 0))) #t)

(check-expect
 (contains? (LogicalLoc 0 4)
            (list (LogicalLoc 0 1) (LogicalLoc 1 0) (LogicalLoc 0 0)))
 #f)
(check-expect
 (contains? (LogicalLoc 1 3)
            (list (LogicalLoc 0 1) (LogicalLoc 1 0) (LogicalLoc 0 0)))
 #f)

;; to-explore : finds adjacent pieces of the same color
(: to-explore :  Board LogicalLoc -> (Listof LogicalLoc))
(define (to-explore board logloc)
  (match (bd-ref board logloc)
    ['None '()]
    [(Some s)
     (filter (lambda ([l : LogicalLoc])
               (match (bd-ref board l)
                 ['None #f]
                 [(Some color) (symbol=? color s)]))
             (identify-adj board logloc))]))

(check-expect (to-explore board2 (LogicalLoc 1 0)) (list (LogicalLoc 1 1)))
(check-expect (to-explore board2 (LogicalLoc 1 1))
              (list (LogicalLoc 1 2) (LogicalLoc 1 0)))
(check-expect (to-explore board2 (LogicalLoc 0 1)) '())

;; returns unmarked neighbors of the same color
;; unmarked-neighbors : board source marked-locations -> unmarked-locations
(: unmarked-neighbors : Board LogicalLoc (Listof LogicalLoc) ->
   (Listof LogicalLoc))
(define (unmarked-neighbors board logloc marked-list)
  (match (bd-ref board logloc)
    ['None '()]
    [(Some s)
     (filter (lambda ([l : LogicalLoc]) (not (contains? l marked-list)))
             (to-explore board logloc))]))

(check-expect (unmarked-neighbors board4 (LogicalLoc 0 0)
                                  (list (LogicalLoc 1 0))) '())
(check-expect (unmarked-neighbors board4 (LogicalLoc 1 0)
                                  (list (LogicalLoc 0 0)))
              (list (LogicalLoc 2 0) (LogicalLoc 1 1)))
(check-expect (unmarked-neighbors board4 (LogicalLoc 2 2) '())
              (list (LogicalLoc 3 2) (LogicalLoc 1 2) (LogicalLoc 2 1)))
(check-expect (unmarked-neighbors board1 (LogicalLoc 1 1)
                                  (list (LogicalLoc 1 0)))
              (list (LogicalLoc 1 2)))

;; identify-chain : identifies any chains on the board given the board,
;; all possible stones to explore, and marked stones
(: identify-chain : Board (Listof LogicalLoc) (Listof LogicalLoc) ->
   (Optional (Listof LogicalLoc)))
(define (identify-chain board explore-list marked-list)
  (match explore-list
    ['() (Some marked-list)]
    [(cons h t)
     (if (liberties? board h)
         'None
         (identify-chain board
                         (append (unmarked-neighbors board h marked-list) t)
                         (append (unmarked-neighbors board h marked-list)
                                 marked-list)))]))

(check-expect (identify-chain board5 (list (LogicalLoc 1 0))
                              (list (LogicalLoc 1 0)))
              (Some (list (LogicalLoc 0 1) (LogicalLoc 3 0) (LogicalLoc 2 0)
                          (LogicalLoc 0 0) (LogicalLoc 1 1) (LogicalLoc 1 0))))

(check-expect (identify-chain board6 (list (LogicalLoc 0 0))
                              (list (LogicalLoc 0 0)))
              (Some
               (list (LogicalLoc 1 1)
                     (LogicalLoc 1 0)
                     (LogicalLoc 0 1)
                     (LogicalLoc 0 0))))

(check-expect (identify-chain board6 (list (LogicalLoc 0 1))
                              (list (LogicalLoc 0 1)))
              (Some (list (LogicalLoc 1 0) (LogicalLoc 1 1)
                          (LogicalLoc 0 0) (LogicalLoc 0 1))))

(check-expect (identify-chain board6 (list (LogicalLoc 0 2))
                              (list (LogicalLoc 0 2))) 'None)

(check-expect (identify-chain (vector (vector (Some 'white) (Some 'black) 'None)
                                      (vector (Some 'black) (Some 'black) 'None)
                                      (vector 'None 'None 'None))
                              (list (LogicalLoc 0 0)) (list (LogicalLoc 0 0)))
              (Some (list (LogicalLoc 0 0))))

;; removes piece: removes piece from board
(: remove-piece : LogicalLoc Board -> Board)
(define (remove-piece logloc board)
  (begin (bd-set! board logloc 'None) board))

(check-expect (remove-piece (LogicalLoc 0 0)
                            (vector (vector (Some 'white) (Some 'black) 'None)
                                    (vector (Some 'black) (Some 'black) 'None)
                                    (vector 'None 'None 'None)))
              (vector (vector 'None (Some 'black) 'None)
                      (vector (Some 'black) (Some 'black) 'None)
                      (vector 'None 'None 'None)))
(check-expect (remove-piece (LogicalLoc 1 0)
                            (vector (vector (Some 'white) (Some 'black) 'None)
                                    (vector (Some 'black) (Some 'black) 'None)
                                    (vector 'None 'None 'None)))
              (vector (vector (Some 'white) (Some 'black) 'None)
                      (vector 'None (Some 'black) 'None)
                      (vector 'None 'None 'None)))

;; potential-captures : finds adjacent pieces of the same color
(: potential-captures : Board LogicalLoc -> (Listof LogicalLoc))
(define (potential-captures board logloc)
  (match (bd-ref board logloc)
    ['None '()]
    [(Some s)
     (filter (lambda ([l : LogicalLoc])
               (match (bd-ref board l)
                 ['None #f]
                 [(Some color) (not (symbol=? color s))]))
             (identify-adj board logloc))]))

(: boardP : Board)
(define boardP (vector (vector 'None (Some 'black) 'None)
                       (vector (Some 'black) (Some 'white) (Some 'white))
                       (vector 'None 'None 'None)))

(check-expect (potential-captures boardP (LogicalLoc 1 0))
              (list (LogicalLoc 1 1)))
(check-expect (potential-captures boardP (LogicalLoc 1 1))
              (list (LogicalLoc 0 1) (LogicalLoc 1 0)))
(check-expect (potential-captures
               (vector (vector (Some 'white) (Some 'white) (Some 'black))
                       (vector (Some 'white) (Some 'white) (Some 'black))
                       (vector (Some 'black) (Some 'black) (Some 'black)))
               (LogicalLoc 2 2)) '())

;; capture-chain : determines if any of the potential captures
;; are a part of a chain
(: capture-chain : Board LogicalLoc -> (Listof (Optional (Listof LogicalLoc))))
(define (capture-chain board logloc)
  (remove-duplicates (remove* '(None) (map (lambda ([ll : LogicalLoc])
                                             (identify-chain board (list ll) (list ll)))
                                           (potential-captures board logloc)))))

(check-expect (capture-chain
               (vector (vector (Some 'white) (Some 'white) (Some 'black))
                       (vector (Some 'white) (Some 'white) (Some 'black))
                       (vector (Some 'black) (Some 'black) (Some 'black)))
               (LogicalLoc 1 2)) (list (Some (list (LogicalLoc 0 0)
                                                   (LogicalLoc 0 1)
                                                   (LogicalLoc 1 0)
                                                   (LogicalLoc 1 1)))))
(check-expect (capture-chain
               (vector (vector (Some 'white) (Some 'white) (Some 'black))
                       (vector (Some 'white) (Some 'white) (Some 'black))
                       (vector (Some 'black) (Some 'black) (Some 'black)))
               (LogicalLoc 1 1)) (list
                                  (Some (list (LogicalLoc 0 2)
                                              (LogicalLoc 1 2)
                                              (LogicalLoc 2 2)
                                              (LogicalLoc 2 0)
                                              (LogicalLoc 2 1)))
                                  (Some (list (LogicalLoc 2 0)
                                              (LogicalLoc 2 1)
                                              (LogicalLoc 2 2)
                                              (LogicalLoc 0 2)
                                              (LogicalLoc 1 2)))))
                                  
;; remove-capture: takes in a list of (Optional (Listof LogicalLoc))
;; and returns a board
(: remove-captured : (Listof (Optional (Listof LogicalLoc))) Board -> Board)
(define (remove-captured lls board)
  (match lls
    ['() board]
    [(cons h t) (match h
                  ['None (remove-captured t board)]
                  [(Some caplist)
                   (remove-captured t (foldr remove-piece board caplist))])]))

(check-expect (remove-captured
               (list 'None (Some (list (LogicalLoc 2 0) (LogicalLoc 2 1))))
               (vector (vector (Some 'white) (Some 'white) 'None)
                       (vector (Some 'white) (Some 'white) (Some 'black))
                       (vector (Some 'black) (Some 'black) (Some 'white))))
              (vector (vector (Some 'white) (Some 'white) 'None)
                      (vector (Some 'white) (Some 'white) (Some 'black))
                      (vector 'None 'None (Some 'white))))

;; place stone : places a stone onto the given board at the given
;; logical location
(: place-stone : Board LogicalLoc Stone -> Board)
(define (place-stone board logloc next)
  (begin
    (bd-set! board logloc (Some next))
    (match (capture-chain board logloc)
      ['() (match (identify-chain board (list logloc) (list logloc))
             ['None board]
             [(Some chain) (foldr remove-piece board chain)])]
      [list (remove-captured list board)])))

(check-expect (place-stone (vector
                            (vector (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone)) (Some 'black))
                            (vector (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone)) (Some 'black))
                            (vector (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone))))
                           (LogicalLoc 2 2)
                           'white)
              (vector
               (vector 'None 'None (Some 'black))
               (vector 'None 'None (Some 'black))
               (vector 'None 'None (Some 'white))))

(check-expect (place-stone (vector
                            (vector (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone)) (Some 'black))
                            (vector (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone)) (Some 'black))
                            (vector (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone))))
                           (LogicalLoc 1 1)
                           'white)
              (vector
               (vector 'None 'None (Some 'black))
               (vector 'None (Some 'white) (Some 'black))
               (vector 'None 'None 'None)))

(check-expect (place-stone (vector
                            (vector (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone)) (Some 'black))
                            (vector (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone)) (Some 'black))
                            (vector (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone))
                                    (cast 'None (Optional Stone))))
                           (LogicalLoc 0 0)
                           'black)
              (vector
               (vector (Some 'black) 'None (Some 'black))
               (vector 'None 'None (Some 'black))
               (vector 'None 'None 'None)))

;; legal-move? Determines if a desired move is allowed as per the rules of Go
(: legal-move? : Go LogicalLoc -> Boolean)
(define (legal-move? go logloc)
  (if (two-passes? go) #f
      (match go
        [(Go board next history _ _ _ _)
         (match (board-ref go logloc)
           ['None
            (match (place-stone (board-copy board) logloc next)
              [newBoard
               (match history
                 ['() #t]
                 [h (not (ormap (lambda ([b : Board])
                                  (board=? b newBoard)) h))])])]
           [_ #f])])))

(check-expect (legal-move? go4 (LogicalLoc 0 2)) #f)
(check-expect (legal-move? go4 (LogicalLoc 0 0)) #f)
(check-expect (legal-move? go4 (LogicalLoc 1 2)) #f)
(check-expect (legal-move? go4 (LogicalLoc 3 3)) #t)

;; makes a list of captured locations
(: list-of-cap : (Listof (Optional (Listof LogicalLoc))) -> (Listof LogicalLoc))
(define (list-of-cap captured-locs)
  (match captured-locs
    ['() '()]
    [(cons h t) (match h
                  ['None (list-of-cap t)]
                  [(Some list) (append list (list-of-cap t))])]))

(check-expect (list-of-cap (list 'None (Some (list (LogicalLoc 0 2)
                                                   (LogicalLoc 1 1)))
                                 (Some (list (LogicalLoc 1 4)))))
              (list (LogicalLoc 0 2) (LogicalLoc 1 1) (LogicalLoc 1 4)))

;; apply-move : applies a move to the given Go
(: apply-move : Go LogicalLoc -> Go)
(define (apply-move game logloc)
  (local {(define historyBoard (board-copy (Go-board game)))
          (define capBoard (board-copy (Go-board game)))}
    (match game
      [(Go board next history _ l-o-cap l-s-cap consec-pass)
       (Go
        (place-stone board logloc next)
        (if (symbol=? next 'black) 'white 'black)
        (cons historyBoard
              history)
        (Some logloc)
        (list-of-cap (capture-chain
                      (begin (bd-set! capBoard logloc (Some next)) capBoard)
                      logloc))
        (match (identify-chain capBoard (list logloc) (list logloc))
          ['None '()]
          [(Some list) list])
        0)])))

(define goCheck (Go (vector (vector (Some 'white) (Some 'white) 'None)
                            (vector (Some 'white) (Some 'white) (Some 'black))
                            (vector (Some 'black) (Some 'black) 'None))
                    'black
                    '() 'None '() '() 1))
(define goCheck2 (Go (vector (vector (Some 'white) (Some 'white) 'None)
                             (vector (Some 'white) (Some 'white) (Some 'black))
                             (vector (Some 'black) (Some 'black) 'None))
                     'white
                     '() 'None '() '() 1))

(check-expect (apply-move goCheck (LogicalLoc 0 2))
              (Go
               (vector (vector 'None 'None (Some 'black))
                       (vector 'None 'None (Some 'black))
                       (vector (Some 'black) (Some 'black) 'None))
               'white
               (list
                (vector (vector (Some 'white) (Some 'white) 'None)
                        (vector (Some 'white) (Some 'white) (Some 'black))
                        (vector (Some 'black) (Some 'black) 'None)))
               (Some (LogicalLoc 0 2))
               (list (LogicalLoc 1 0) (LogicalLoc 1 1) (LogicalLoc 0 0)
                     (LogicalLoc 0 1))
               '()
               0))
(check-expect (apply-move goCheck2 (LogicalLoc 2 2))
              (Go
               (vector
                (vector (Some 'white) (Some 'white) 'None)
                (vector (Some 'white) (Some 'white) (Some 'black))
                (vector 'None 'None (Some 'white)))
               'black
               (list
                (vector
                 (vector (Some 'white) (Some 'white) 'None)
                 (vector (Some 'white) (Some 'white) (Some 'black))
                 (vector (Some 'black) (Some 'black) 'None)))
               (Some (LogicalLoc 2 2))
               (list (LogicalLoc 2 0) (LogicalLoc 2 1))
               (list (LogicalLoc 2 2)) 0))

(check-expect (apply-move (Go
                           (vector '#(None None None)
                                   (vector 'None (Some 'white) (Some 'white))
                                   (vector (Some 'white) 'None (Some 'black)))
                           'black
                           (list
                            (vector '#(None None None)
                                    (vector 'None (Some 'white) (Some 'white))
                                    (vector (Some 'white) 'None (Some 'black)))
                            (vector '#(None None None)
                                    (vector 'None (Some 'white) 'None)
                                    (vector (Some 'white) 'None (Some 'black)))
                            (vector '#(None None None)
                                    (vector 'None (Some 'white) 'None)
                                    (vector (Some 'white) 'None (Some 'black)))
                            (vector '#(None None None) '#(None None None)
                                    (vector (Some 'white) 'None (Some 'black)))
                            (vector '#(None None None) '#(None None None)
                                    (vector (Some 'white) 'None (Some 'black)))
                            (vector '#(None None None) '#(None None None)
                                    (vector 'None 'None (Some 'black))))
                           'None
                           '()
                           '()
                           1)
                          (LogicalLoc 2 1))
              (Go
               (vector '#(None None None)
                       (vector 'None (Some 'white) (Some 'white))
                       (vector (Some 'white) 'None 'None))
               'white
               (list
                (vector '#(None None None)
                        (vector 'None (Some 'white) (Some 'white))
                        (vector (Some 'white) 'None (Some 'black)))
                (vector '#(None None None)
                        (vector 'None (Some 'white) (Some 'white))
                        (vector (Some 'white) 'None (Some 'black)))
                (vector '#(None None None)
                        (vector 'None (Some 'white) 'None)
                        (vector (Some 'white) 'None (Some 'black)))
                (vector '#(None None None) (vector 'None (Some 'white) 'None)
                        (vector (Some 'white) 'None (Some 'black)))
                (vector '#(None None None) '#(None None None)
                        (vector (Some 'white) 'None (Some 'black)))
                (vector '#(None None None) '#(None None None)
                        (vector (Some 'white) 'None (Some 'black)))
                (vector '#(None None None) '#(None None None)
                        (vector 'None 'None (Some 'black))))
               (Some (LogicalLoc 2 1))
               '()
               (list (LogicalLoc 2 2) (LogicalLoc 2 1))
               0))
              
;; two-passes? : report whether the last two moves have been passes,
;; indicating that the game is now over.
(: two-passes? : Go -> Boolean)
(define (two-passes? go)
  (match go
    [(Go _ _ _ _ _ _ consec-pass)
     (>= consec-pass 2)]))

(check-expect (two-passes?
               (Go
                (vector
                 '#(None None None None None)
                 (vector 'None (Some 'black) 'None 'None 'None)
                 '#(None None None None None)
                 '#(None None None None None)
                 '#(None None None None None))
                'white
                (list
                 (vector
                  '#(None None None None None)
                  (vector 'None (Some 'black) 'None 'None 'None)
                  '#(None None None None None)
                  '#(None None None None None)
                  '#(None None None None None))
                 (vector
                  '#(None None None None None)
                  (vector 'None (Some 'black) 'None 'None 'None)
                  '#(None None None None None)
                  '#(None None None None None)
                  '#(None None None None None))
                 '#(#(None None None None None)
                    #(None None None None None)
                    #(None None None None None)
                    #(None None None None None)
                    #(None None None None None)))
                'None
                '()
                '()
                2))
              #t)

;; draw: Draws the given world
(: draw (World -> Image))
(define (draw w)
  (match w 
    [(World spec game status b-tenths w-tenths hover)
     (match* (spec game)
       [((BoardSpec b c m r)
         (Go board next history l-place l-o-cap l-s-cap consec-pass))
        (local
          {(define dim (vector-length board))}
          (above/align
           "left"
           (draw-captures
            l-o-cap l-s-cap game spec
            (draw-last game spec
                       (match hover
                         ['None (draw-circles game spec
                                              (draw-board game spec))]
                         [(Some logloc)
                          (if
                           (legal-move? game
                                        logloc)
                           (draw-hover game spec logloc)
                           (draw-circles game spec
                                         (draw-board game spec)))])))
           (overlay (text status (int->text-size (quotient c 2))
                          "black")
                    (rectangle (+ (* c (- dim 1)) m m) c
                               "solid" b))
           (draw-timer w)))])]))

(draw world-emp)

;; empty-list: takes in a board and returns a list of all of the
;; empty locations on the board
(: empty-list : Board -> (Listof LogicalLoc))
(define (empty-list board)
  (local
    {(define len (vector-length board))
     (: lp : Integer Integer -> (Listof LogicalLoc))
     (define (lp row col)
       (cond
         [(= col len) '()]
         [(= row len) (lp 0 (add1 col))]
         [else (match (bd-ref board (LogicalLoc col row))
                 ['None (cons (LogicalLoc col row) (lp (add1 row) col))]
                 [_ (lp (add1 row) col)])]))}
    (lp 0 0)))

(check-expect (empty-list (vector (vector 'None (Some 'black) 'None)
                                  (vector (Some 'black) (Some 'white) (Some 'white))
                                  '#(None None None)))
              (list (LogicalLoc 0 0) (LogicalLoc 0 2) (LogicalLoc 2 0)
                    (LogicalLoc 2 1) (LogicalLoc 2 2)))

;; explore-empty : determines if an empty location has other empty locations
;; near it and returns the logical locations of those empty
(: explore-empty :  Board LogicalLoc -> (Listof LogicalLoc))
(define (explore-empty board logloc)
  (match (bd-ref board logloc)
    ['None (filter (lambda ([l : LogicalLoc])
                     (match (bd-ref board l)
                       ['None #t]
                       [_ #f]))
                   (identify-adj board logloc))]
    [(Some s) '()]))

(check-expect (explore-empty (vector
                              '#(None None None None None)
                              (vector 'None (Some 'black) 'None 'None 'None)
                              '#(None None None None None)
                              '#(None None None None None)
                              '#(None None None None None)) (LogicalLoc 1 1))
              '())

(check-expect (explore-empty (vector
                              '#(None None None None None)
                              (vector 'None (Some 'black) 'None 'None 'None)
                              '#(None None None None None)
                              '#(None None None None None)
                              '#(None None None None None)) (LogicalLoc 1 0))
              (list (LogicalLoc 2 0)
                    (LogicalLoc 0 0)))
                   
;; returns empty unmarked neighbors
;; empty-unmarked-neighbors : board source marked-locations ->
;; unmarked-locations
(: empty-unmarked-neighbors : Board LogicalLoc (Listof LogicalLoc) ->
   (Listof LogicalLoc))
(define (empty-unmarked-neighbors board logloc marked-list)
  (match (bd-ref board logloc)
    ['None (filter (lambda ([l : LogicalLoc]) (not (contains? l marked-list)))
                   (explore-empty board logloc))]
    [_ '()]))

(check-expect (empty-unmarked-neighbors explore-board (LogicalLoc 1 0)'())
              (list (LogicalLoc 0 0)))
(check-expect (empty-unmarked-neighbors explore-board (LogicalLoc 0 2)
                                        (list (LogicalLoc 1 2)))
              '())

;; adj-stones : determines symbols of adjacent stones and creates list
(: adj-stones : Board (Listof LogicalLoc) -> (Listof Symbol))
(define (adj-stones board lls)
  (remove* (list 'None)(map (lambda ([ll : LogicalLoc]) (match (bd-ref board ll)
                                                          ['None 'None]
                                                          [(Some stone) stone])) lls)))

(check-expect (adj-stones
               (vector '#(None None None)
                       (vector (Some 'black) (Some 'black)
                               (Some 'black))
                       (vector (Some 'white) 'None 'None))
               (identify-adj (vector '#(None None None)
                                     (vector (Some 'black) (Some 'black)
                                             (Some 'black))
                                     (vector (Some 'white) 'None 'None))
                             (LogicalLoc 0 0))) '(black))

(check-expect (adj-stones
               (vector '#(None None None)
                       (vector (Some 'black) (Some 'black)
                               (Some 'black))
                       (vector (Some 'white) 'None 'None))
               (identify-adj (vector '#(None None None)
                                     (vector (Some 'black) (Some 'black)
                                             (Some 'black))
                                     (vector (Some 'white) 'None 'None))
                             (LogicalLoc 1 0))) '(white black))
   
;; identify-territory : determines a territory within given board
(: identify-territory : Board (Listof LogicalLoc) (Listof LogicalLoc)
   (Listof Symbol) -> (Optional Territory))
(define (identify-territory board explore-list marked-list symbol-acc)
  (if (and (ormap (lambda ([s : Symbol]) (symbol=? s 'black)) symbol-acc)
           (ormap (lambda ([s : Symbol]) (symbol=? s 'white)) symbol-acc))
      'None
      (match explore-list
        ['() (if (empty? symbol-acc) 'None
                 (Some (Territory marked-list (first symbol-acc))))]
        [(cons h t)
         (identify-territory board
                             (append
                              (empty-unmarked-neighbors board h marked-list) t)
                             (append
                              (empty-unmarked-neighbors board h marked-list)
                              marked-list)
                             (append
                              (adj-stones board (identify-adj board h))
                              symbol-acc))])))

(check-expect (identify-territory (vector '#(None None None)
                                          (vector (Some 'black) (Some 'black)
                                                  (Some 'black))
                                          (vector (Some 'white) 'None 'None))
                                  (list (LogicalLoc 0 0))
                                  (list (LogicalLoc 0 0))
                                  '())
              (Some (Territory (list (LogicalLoc 0 2) (LogicalLoc 0 1)
                                     (LogicalLoc 0 0)) 'black)))

(check-expect (identify-territory (vector '#(None None None)
                                          (vector (Some 'black)
                                                  (Some 'black)
                                                  (Some 'black))
                                          (vector (Some 'white) 'None 'None))
                                  (list (LogicalLoc 2 1))
                                  (list (LogicalLoc 2 1))
                                  '())
              'None)

;; identifies all territory for a given player
(: all-territory : Board (Listof LogicalLoc) (Listof Territory) ->
   (Listof Territory))
(define (all-territory board remaining-empty acc)
  (match remaining-empty
    ['() acc]
    [(cons h t) (match (identify-territory board (list h) (list h)'())
                  ['None (all-territory board t acc)]
                  [(Some terr)
                   (match terr
                     [(Territory checked-list color)
                      (all-territory board
                                     (remove* checked-list remaining-empty)
                                     (cons terr acc))])])]))

(check-expect (all-territory (vector '#(None None None)
                                     (vector (Some 'black) (Some 'black)
                                             (Some 'black))
                                     (vector (Some 'white) 'None 'None))
                             (empty-list (vector '#(None None None)
                                                 (vector (Some 'black)
                                                         (Some 'black)
                                                         (Some 'black))
                                                 (vector (Some 'white)
                                                         'None 'None)))
                             '())
              (list (Territory (list (LogicalLoc 0 2) (LogicalLoc 0 1)
                                     (LogicalLoc 0 0)) 'black)))
(check-expect (all-territory (vector '#(None None None)
                                     (vector (Some 'black) (Some 'white)
                                             (Some 'white))
                                     (vector (Some 'white) 'None 'None))
                             (empty-list (vector '#(None None None)
                                                 (vector (Some 'black)
                                                         (Some 'white)
                                                         (Some 'white))
                                                 (vector (Some 'white)
                                                         'None 'None)))
                             '())
              (list (Territory (list (LogicalLoc 2 2) (LogicalLoc 2 1))
                               'white)))

;; the area controlled by each player and the winner (or indicates a draw)
(: outcome : Go -> Outcome)
(define (outcome go)
  (match go
    [(Go board _ _ _ _ _ _)
     (local {(define len (vector-length board))
             (: count-stones : Integer Integer Integer Stone -> Integer)
             (define (count-stones row col acc stone)
               (cond
                 [(= col len) acc]
                 [(= row len) (count-stones 0 (add1 col) acc stone)]
                 [else (match (bd-ref board (LogicalLoc col row))
                         ['None (count-stones (add1 row) col acc stone)]
                         [(Some s)
                          (count-stones
                           (add1 row)
                           col
                           (if (symbol=? stone s) (add1 acc) acc) stone)])]))
             (: count-territories : (Listof Territory) Stone Integer -> Integer)
             (define (count-territories territories stone acc)
               (match territories
                 ['() acc]
                 [(cons h t) (match h
                               [(Territory terr-list s)
                                (count-territories t stone
                                                   (if (symbol=? stone s)
                                                       (+ (length terr-list)
                                                          acc) acc))])]))
             (define black (+ (count-stones 0 0 0 'black)
                              (count-territories
                               (all-territory board (empty-list board) '())
                               'black 0)))
             (define white (+ (count-stones 0 0 0 'white)
                              (count-territories
                               (all-territory board (empty-list board) '())
                               'white 0)))}
       (Outcome black white (cond
                              [(> black white) 'black]
                              [(> white black) 'white]
                              [else 'draw])))]))

(check-expect (outcome (Go (empty-board 10) 'black '() 'None '() '() 1))
              (Outcome 0 0 'draw))
(check-expect (outcome (Go (vector (vector (Some 'white) (Some 'black) 'None)
                                   (vector (Some 'white) 'None 'None )
                                   (vector 'None 'None 'None))
                           'black '() 'None '() '() 0))
              (Outcome 1 2 'white))
(check-expect (outcome (Go (vector (vector (Some 'black) (Some 'white) 'None)
                                   (vector (Some 'white) 'None 'None )
                                   (vector 'None 'None 'None))
                           'black '() 'None '() '() 0))
              (Outcome 1 8 'white))

(check-expect (outcome (Go (vector (vector (Some 'black) 'None 'None 'None
                                           'None)
                                   (vector (Some 'black) (Some 'black)
                                           (Some 'black) (Some 'black)
                                           (Some 'black))
                                   (vector 'None 'None 'None (Some 'white)
                                           'None 'None)
                                   (vector 'None 'None 'None (Some 'black)
                                           (Some 'black))
                                   (vector 'None 'None 'None (Some 'black)
                                           'None))
                           'black '() 'None '() '() 0))
              (Outcome 14 1 'black))

;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) 
        (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

(check-expect (string->integer "3") 3)
(check-expect (string->integer "55") 55)

;; board->string : converts a board to a string
(: board->string : Board -> String)
(define (board->string board)
  (local
    {(define len (vector-length board))
     (: convert : Integer Integer String -> String)
     (define (convert col row acc)
       (local
         {(define len (vector-length board))}
         (cond
           [(= col len) acc]
           [(= row len) (convert (add1 col) 0 (string-append acc "|"))]
           [else
            (convert col (add1 row)
                     (string-append acc (match (bd-ref board
                                                       (LogicalLoc col row))
                                          ['None  "_"]
                                          [(Some s) (if (symbol=? s 'black)
                                                        "*" "o")])))])))}
    (convert 0 0 "")))

(check-expect (board->string (vector (vector (Some 'black) (Some 'white) 'None)
                                     (vector (Some 'white) 'None 'None )
                                     (vector 'None 'None 'None)))
              "*o_|o__|___|")
(check-expect (board->string board1)
              "_*_|*oo|___|")
                             

;; string->board : converts a string to board
(: string->board : String -> Board)
(define (string->board board-string)
  (local {(define list-board (string-split board-string "|"))
          (define len (length list-board))
          (define new-board (empty-board len))
          (: lp : Integer Integer -> Board)
          (define (lp col row)
            (cond
              [(= col len) new-board]
              [(= row len)
               (lp (add1 col) 0)]
              [else (begin (bd-set! new-board
                                    (LogicalLoc col row)
                                    (match (string-ref
                                            (list-ref list-board col)
                                            row)
                                      [#\* (Some 'black)]
                                      [#\o (Some 'white)]
                                      [#\_ 'None]
                                      [_ (error
                                          (string-append
                                           "string->board: invalid "
                                           "symbol in board"))]))
                           (lp col (add1 row)))]))}
    (lp 0 0)))

(check-expect (string->board "*o_|o__|___|")
              (vector (vector (Some 'black) (Some 'white) 'None)
                      (vector (Some 'white) 'None 'None )
                      (vector 'None 'None 'None)))
(check-expect (string->board "_*_|*oo|___|")
              (vector (vector 'None (Some 'black) 'None)
                      (vector (Some 'black) (Some 'white) (Some 'white))
                      '#(None None None)))
(check-error (string->board "_*&|___|***|")
             "string->board: invalid symbol in board")

;; history->string : converts history to string
(: history->string : (Listof Board) -> String)
(define (history->string boards)
  (match boards
    ['() ""]
    [(cons h t) (string-append (board->string h) "!" (history->string t))]))

(check-expect (history->string
               (list
                (vector (vector (Some 'black) (Some 'white) 'None)
                        (vector (Some 'white) 'None 'None )
                        (vector 'None 'None 'None))
                (vector (vector (Some 'black) (Some 'white) 'None)
                        (vector (Some 'white) 'None 'None )
                        (vector 'None 'None 'None))
                (vector (vector (Some 'black) (Some 'white) 'None)
                        (vector (Some 'white) 'None 'None )
                        (vector 'None (Some 'white) (Some 'white)))))
              "*o_|o__|___|!*o_|o__|___|!*o_|o__|_oo|!")
(check-expect (history->string (Go-history go4))
              "o_o_|**o_|*oo*|*oo_|!**o_|**o_|*oo*|*oo_|!")

(: string->history : String -> (Listof Board))
(define (string->history history-string)
  (map string->board (string-split history-string "!")))

(check-expect (string->history "*o_|o__|___|!*o_|o__|___|!*o_|o__|_oo|!")
              (list
               (vector (vector (Some 'black) (Some 'white) 'None)
                       (vector (Some 'white) 'None 'None )
                       (vector 'None 'None 'None))
               (vector (vector (Some 'black) (Some 'white) 'None)
                       (vector (Some 'white) 'None 'None )
                       (vector 'None 'None 'None))
               (vector (vector (Some 'black) (Some 'white) 'None)
                       (vector (Some 'white) 'None 'None )
                       (vector 'None (Some 'white) (Some 'white)))))

(check-expect (string->history "o_o_|**o_|*oo*|*oo_|!**o_|**o_|*oo*|*oo_|!")
              (list (vector
                     (vector (Some 'white) 'None (Some 'white) 'None)
                     (vector (Some 'black) (Some 'black) (Some 'white) 'None)
                     (vector (Some 'black) (Some 'white) (Some 'white)
                             (Some 'black))
                     (vector (Some 'black) (Some 'white) (Some 'white) 'None))
                    (vector
                     (vector (Some 'black) (Some'black) (Some 'white) 'None)
                     (vector (Some 'black) (Some 'black) (Some 'white) 'None)
                     (vector (Some 'black) (Some 'white) (Some 'white)
                             (Some 'black))
                     (vector (Some 'black) (Some 'white) (Some 'white) 'None))))

;; go->string : converts go to string
(: go->string : Go -> String)
(define (go->string go)
  (match go
    [(Go board next history _ _ _ consec-pass)
     (string-append (if (symbol=? next 'black) "*" "o")
                    "~"
                    (board->string board)
                    "~"
                    (history->string history)
                    "~"
                    (number->string consec-pass))]))

(check-expect (go->string
               (Go (vector
                    (vector (Some 'black) 'None 'None 'None 'None)
                    (vector (Some 'black) (Some 'black) (Some 'black)
                            (Some 'black) (Some 'black))
                    (vector 'None 'None 'None (Some 'white) 'None 'None)
                    (vector 'None 'None 'None (Some 'black) (Some 'black))
                    (vector 'None 'None 'None (Some 'black) 'None))
                   'black (list (vector (vector 'None 'None 'None 'None 'None)
                                        (vector (Some 'black) (Some 'black)
                                                (Some 'black)
                                                (Some 'black) (Some 'black))
                                        (vector 'None 'None 'None (Some 'white)
                                                'None)
                                        (vector 'None 'None 'None (Some 'black)
                                                (Some 'black))
                                        (vector 'None 'None 'None (Some 'black)
                                                'None))
                                (vector (vector 'None 'None 'None 'None 'None)
                                        (vector 'None (Some 'black)
                                                (Some 'black)
                                                (Some 'black) (Some 'black))
                                        (vector 'None 'None 'None (Some 'white)
                                                'None 'None)
                                        (vector 'None 'None 'None (Some 'black)
                                                (Some 'black))
                                        (vector 'None 'None 'None (Some 'black)
                                                'None))) 'None '() '() 1))
              (string-append
               "*~*____|*****|___o_|___**|___*_|~_____|*****|___o"
               "_|___**|___*_|!_____|_****|___o_|___**|___*_|!~1"))

(check-expect (go->string
               (Go
                (vector
                 '#(None None None None None)
                 (vector 'None (Some 'black) 'None 'None 'None)
                 '#(None None None None None)
                 '#(None None None None None)
                 '#(None None None None None))
                'white
                (list
                 (vector
                  '#(None None None None None)
                  (vector 'None (Some 'black) 'None 'None 'None)
                  '#(None None None None None)
                  '#(None None None None None)
                  '#(None None None None None))
                 (vector
                  '#(None None None None None)
                  (vector 'None (Some 'black) 'None 'None 'None)
                  '#(None None None None None)
                  '#(None None None None None)
                  '#(None None None None None))
                 '#(#(None None None None None)
                    #(None None None None None)
                    #(None None None None None)
                    #(None None None None None)
                    #(None None None None None)))
                'None
                '()
                '()
                2))
              (string-append "o~_____|_*___|_____|_____|_____|~_____|_*___|"
                             "_____|_____|_____|!_____|_*___|_____|_____|_____|"
                             "!_____|_____|_____|_____|_____|!~2"))         
               

;; string->go : converts string to go
(: string->go : String -> Go)
(define (string->go go-string)
  (local {(define list-string (string-split go-string "~"))}
    (Go (string->board (list-ref list-string 1))
        (if (string=? (list-ref list-string 0) "*") 'black 'white)
        (string->history (list-ref list-string 2))
        'None
        '()
        '()
        (string->integer (list-ref list-string 3)))))

(check-expect (string->go
               (string-append "o~_____|_*___|_____|_____|_____|~_____|_*___|"
                              "_____|_____|_____|!_____|_*___|_____|_____|"
                              "_____|!_____|_____|_____|_____|_____|!~2"))
              (Go
               (vector
                '#(None None None None None)
                (vector 'None (Some 'black) 'None 'None 'None)
                '#(None None None None None)
                '#(None None None None None)
                '#(None None None None None))
               'white
               (list
                (vector
                 '#(None None None None None)
                 (vector 'None (Some 'black) 'None 'None 'None)
                 '#(None None None None None)
                 '#(None None None None None)
                 '#(None None None None None))
                (vector
                 '#(None None None None None)
                 (vector 'None (Some 'black) 'None 'None 'None)
                 '#(None None None None None)
                 '#(None None None None None)
                 '#(None None None None None))
                '#(#(None None None None None)
                   #(None None None None None)
                   #(None None None None None)
                   #(None None None None None)
                   #(None None None None None)))
               'None
               '()
               '()
               2))

;; world->string : converts a world to string
(: world->string : World -> String)
(define (world->string world)
  (match world
    [(World _ go _ b-tenths w-tenths _)
     (string-append
      (number->string b-tenths)
      "@"
      (number->string w-tenths)
      "@"
      (go->string go))]))

(check-expect (world->string world1) "5@10@*~_*_|*oo|___|~~0")

;; string->world : converts a string to a world
(: string->world : BoardSpec String -> World)
(define (string->world spec str)
  (match (string-split str "@")
    [(list b-tenths w-tenths go)
     (match (string-split go "~")
       [(list next board history consec-pass)
        (if (or (string=? next "*") (string=? next "o"))
            (match (string-split board "|")
              [brd (if (= (length brd) (string-length (first brd)))
                       (if (andmap (lambda ([b : String])
                                     (= (string-length b) (string-length board)))
                                   (string-split history "!"))
                           (World spec (string->go go)
                                  "Welcome to Go!"
                                  (string->integer b-tenths)
                                  (string->integer w-tenths)
                                  'None)
                           (error "string->world: invalid history"))
                       (error "string->world: invalid board"))])
            (error "string->world: incorrect player"))]
       [_ (error  "string->world: invalid go")])]
    [_ (error  "string->world: invalid world")]))

(check-error (string->world test-spec2 "36@44@o~___|_*_|___~___|___|___")
             "string->world: invalid go")

(check-expect (string->world test-spec2 "5@10@*~_*_|*oo|___|~~0")
              (World
               (BoardSpec 'moccasin 30 15 9)
               (Go (vector
                    (vector 'None (Some 'black) 'None)
                    (vector (Some 'black) (Some 'white) (Some 'white))
                    '#(None None None)) 'black '() 'None '() '() 0)
               "Welcome to Go!"
               5
               10
               'None))
(check-expect (string->world test-spec1 "5@9@*~o*_|oo_|_*_~~1")
              (World test-spec1
                     (Go (vector (vector (Some 'white) (Some 'black) 'None)
                                 (vector (Some 'white) (Some 'white) 'None)
                                 (vector 'None (Some 'black) 'None))
                         'black '() 'None '() '() 1) "Welcome to Go!" 5 9 'None))
(check-expect (string->world test-spec2 "5@9@*~_*_|o__|___~~1")
              (World test-spec2
                     (Go (vector (vector 'None (Some 'black) 'None)
                                 (vector (Some 'white) 'None 'None)
                                 '#(None None None)) 'black '() 'None '() '() 1)
                     "Welcome to Go!" 5 9 'None))
(check-error (string->world test-spec1 "@9@*~o*|oo_|*_~~1")
             "string->world: invalid world")
(check-error (string->world test-spec1 "10@20@**|o__|_~")
             "string->world: invalid go")
(check-error (string->world test-spec2 "5@10@!~_*_|*oo|___|~~0")
             "string->world: incorrect player")
(check-error (string->world test-spec2 "5@10@o~**_*_|*oo|___|~~0")
             "string->world: invalid board")
(check-error
 (string->world test-spec2
                (string-append "5@10@*~*____|*****|___o_|___**"
                               "|___*_|~_____|*****|___o_|___**"
                               "|___*_|!_____|_****|___o_|*o___**|"
                               "___*_|!~1"))
 "string->world: invalid history")
                

;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (local
          {(define op : Output-Port (open-output-file path))}
          (begin (write-string (world->string w) op)
                 (close-output-port op)))
        (void))))

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided BoardSpec to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : BoardSpec -> World)
(define (load-game bs)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
        (local
          {(define ip : Input-Port (open-input-file path))
           (define w : World
             (string->world bs (port->string (open-input-file path))))}
          (begin (close-input-port ip) w))
        (error "load-game: user cancelled"))))

;; outcome->string : converts an outcome to a string
(: outcome->string : Outcome -> String)
(define (outcome->string outcome)
  (match outcome
    [(Outcome black white winner)
     (string-append
      " Black Area: "
      (number->string black)
      " White Area: "
      (number->string white)
      " Winner: "
      (symbol->string winner))]))

(check-expect (outcome->string (Outcome 5 10 'white))
              " Black Area: 5 White Area: 10 Winner: white")
(check-expect (outcome->string (Outcome 5 5 'draw))
              " Black Area: 5 White Area: 5 Winner: draw")

;; react-to-click
(: react-to-click (World Integer Integer Mouse-Event -> World))
(define (react-to-click w x y e)
  (match e
    ["button-down"
     (match w
       [(World spec game status b-tenths w-tenths hover)
        (match game
          [(Go board next history _ _ _ consec-pass)
           (match (physical->logical (PhysicalLoc x y) (vector-length board)
                                     spec)
             ['None w]
             [(Some l)
              (cond
                [(legal-move? game l)
                 (World spec
                        (apply-move game l)
                        (string-append
                         (symbol->string next)
                         " moved to "
                         (logical->string l)
                         "\n It is "
                         (symbol->string
                          (if (symbol=? next 'black) 'white 'black))
                         "'s turn")
                        b-tenths
                        w-tenths
                        hover)]
                [(two-passes? game) (World spec (Go board next
                                                    history
                                                    'None
                                                    '()
                                                    '()
                                                    consec-pass)
                                           (string-append "GAME OVER:"
                                                          (outcome->string
                                                           (outcome game)))
                                           b-tenths
                                           w-tenths
                                           'None)]
                [else (World spec game "illegal move"
                             b-tenths
                             w-tenths
                             hover)])])])])]
    ["move"
     (match w
       [(World spec game status b-tenths w-tenths hover)
        (match game
          [(Go board next _ _ _ _ _)
           (match (physical->logical (PhysicalLoc x y) (vector-length board)
                                     spec)
             ['None w]
             [(Some l)
              (if (legal-move? game l)
                  (World spec game status b-tenths w-tenths (Some l))
                  w)])])])]
     
    [_ w]))

(check-expect (react-to-click world1 15 75 "button-down")
              (World
               (BoardSpec 'moccasin 30 15 9)
               (Go
                (vector (vector (Some 'black) (Some 'black) 'None)
                        (vector (Some 'black) (Some 'white) (Some 'white))
                        '#(None None None))
                'white
                (list (vector (vector 'None (Some 'black) 'None)
                              (vector (Some 'black) (Some 'white) (Some 'white))
                              '#(None None None)))
                (Some (LogicalLoc 0 0))
                '()
                '()
                0)
               "black moved to A1\n It is white's turn"
               5
               10
               'None))
(check-expect (react-to-click world1 15 75 "move")
              (World
               (BoardSpec 'moccasin 30 15 9)
               (Go
                (vector (vector (Some 'black) (Some 'black) 'None)
                        (vector (Some 'black) (Some 'white) (Some 'white))
                        '#(None None None))
                'black
                '()
                'None
                '()
                '()
                0)
               "test-message"
               5
               10
               'None))

;; react-to-keyboard : pass player's turn when user strikes "p"
(: react-to-keyboard (World String -> World))
(define (react-to-keyboard w key)
  (match key
    ["p" (match w
           [(World spec game status b-tenths w-tenths hover)
            (match game
              [(Go board next history _ l-o-cap l-s-cap consec-pass)
               (cond
                 [(= consec-pass 1)
                  (World spec (Go board next
                                  history
                                  'None
                                  '()
                                  '()
                                  (add1 consec-pass))
                         (string-append "GAME OVER:"
                                        (outcome->string (outcome game)))
                         b-tenths
                         w-tenths
                         'None)]
                 [(two-passes? game) (World spec (Go board next
                                  history
                                  'None
                                  '()
                                  '()
                                  consec-pass)
                         (string-append "GAME OVER:"
                                        (outcome->string (outcome game)))
                         b-tenths
                         w-tenths
                         'None)]
                 [else (World spec
                              (Go board
                                  (if (symbol=? next 'black) 'white 'black)
                                  (cons (board-copy board) history)
                                  'None
                                  '()
                                  '()
                                  (add1 consec-pass))
                              (string-append
                               (symbol->string next)
                               " passed"
                               "\n It is "
                               (symbol->string
                                (if (symbol=? next 'black) 'white 'black))
                               "'s turn")
                              b-tenths
                              w-tenths
                              hover)])])])]
    ["r" (match w
           [(World spec game status b-tenths w-tenths hover)
            (match game
              [(Go board next history _ l-o-cap l-s-cap consec-pass)
               (World spec
                      (Go (empty-board (vector-length board))
                          'black '() 'None '() '() 0)
                      "Welcome to Go! \n It is black's turn" 0 0 'None)])])]
    ["s" (begin (save-game! w) w)]
    ["l" (match w
           [(World spec game status b-tenths w-tenths hover)
            (local {(define new-world (load-game spec))}
            (if (= (vector-length (Go-board (World-game new-world)))
                   (vector-length (Go-board game)))
                new-world
                (World spec game "load game: attempt to load invalid game"
                        b-tenths w-tenths hover)))])]
    [_ w]))

(check-expect (react-to-keyboard world1 "p")
              (World
               (BoardSpec 'moccasin 30 15 9)
               (Go
                (vector (vector (Some 'black) (Some 'black) 'None)
                        (vector (Some 'black) (Some 'white) (Some 'white))
                        '#(None None None))
                'white
                (list (vector (vector (Some 'black) (Some 'black) 'None)
                              (vector (Some 'black) (Some 'white) (Some 'white))
                              '#(None None None)))
                'None
                '()
                '()
                1)
               "black passed\n It is white's turn"
               5
               10
               'None))

(check-expect (react-to-keyboard world2 "p")
              (World
               (BoardSpec 'moccasin 30 15 9)
               (Go
                (vector (vector 'None (Some 'black) 'None)
                        (vector (Some 'white) (Some 'white) (Some 'white))
                        '#(None None None))
                'white
                (list (vector (vector 'None (Some 'black) 'None)
                              (vector (Some 'white) (Some 'white) (Some 'white))
                              '#(None None None)))
                'None
                '()
                '()
                1)
               "black passed\n It is white's turn"
               30
               10
               (Some (LogicalLoc 0 0))))

;; increment the counter if in running state
(: react-to-tick (World -> World))
(define (react-to-tick w)
  (match w
    [(World spec game status b-tenths w-tenths hover)
     (if (two-passes? game) w
         (if (symbol=? (Go-next-to-play game) 'black)
             (World spec game status (+ 1 b-tenths) w-tenths hover)
             (World spec game status b-tenths (+ 1 w-tenths) hover)))]))

(check-expect (react-to-tick world1)
              (World
               (BoardSpec 'moccasin 30 15 9)
               (Go
                (vector
                 (vector (Some 'black) (Some 'black) 'None)
                 (vector (Some 'black) (Some 'white) (Some 'white))
                 '#(None None None))
                'black
                '()
                'None
                '()
                '()
                0)
               "test-message"
               6
               10
               'None))

(check-expect (react-to-tick world2)
              (World
               (BoardSpec 'moccasin 30 15 9)
               (Go
                (vector
                 (vector 'None (Some 'black) 'None)
                 (vector (Some 'white) (Some 'white) (Some 'white))
                 '#(None None None))
                'black
                '()
                (Some (LogicalLoc 1 0))
                (list (LogicalLoc 0 0))
                (list (LogicalLoc 1 1))
                0)
               "test-message"
               31
               10
               (Some (LogicalLoc 0 0))))
              

;; play: initiates a game 
(: play : Integer BoardSpec -> World)
(define (play dim spec)
  (if (or (< dim 2) (not (valid-board-spec? spec)))
      (error "play: incorrect dimensions or board")
      (big-bang
          (World spec
                 (Go (empty-board dim) 'black '() 'None '() '() 0)
                 "Welcome to Go! \n It is black's turn" 0 0 'None) : World
        [to-draw draw]
        [on-mouse react-to-click]
        [on-key react-to-keyboard]
        [on-tick react-to-tick 1/10])))

(check-error (play 1 test-spec1) "play: incorrect dimensions or board")
(check-error (play 12 (BoardSpec "blue" -10 5 3))
             "play: incorrect dimensions or board")    

(test)