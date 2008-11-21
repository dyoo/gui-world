;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |tic tac toe|) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp") (lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "world.ss" "teachpack" "htdp") (lib "image.ss" "teachpack" "htdp")))))

;; ==== PART #1 : Image practice ====

(define trace-width 20)
(define margin (* 2 trace-width))
(define square-width 100)

(define cross (overlay (rectangle trace-width (- square-width margin) 'solid 'red)
                       (rectangle (- square-width margin) trace-width 'solid 'red)))
cross

(define circ (overlay  (circle (/ (- square-width margin) 2) 'solid 'blue)
                       (circle (/ (- square-width margin (* 2 trace-width)) 2) 'solid 'white)))
circ

(define blank-square (overlay (rectangle square-width square-width 'solid 'SkyBlue)
                              (rectangle (- square-width trace-width)
                                         (- square-width trace-width)
                                         'solid 'white)))
blank-square

;; ==== PART #2 : Union practice (square) ====


;; TYPE DEFINITION
;; A square is either:
;; - 'blank
;; - 'cross
;; - 'circle

;; TEMPLATE
;(define (a-square-function a-square)
;  (cond [(symbol=? a-square 'blank) ...]
;        [(symbol=? a-square 'cross) ...]
;        [(symbol=? a-square 'circle) ...]))


;; draw-square : Given a square, return its image, filled with a cross,
;;               a circle or blank, depending on the input.
;; draw-square : square -> image
(check-expect (draw-square 'blank) blank-square)
(check-expect (draw-square 'cross) (overlay blank-square cross))
(check-expect (draw-square 'circle) (overlay blank-square circ))

(define (draw-square a-square)
  (cond [(symbol=? a-square 'blank) 
         blank-square]
        [(symbol=? a-square 'cross) 
         (overlay blank-square cross)]
        [(symbol=? a-square 'circle) 
         (overlay blank-square circ)]))

;; ==== PART #3 : Structure practice (row) ====

;; beside : Given two images, returns a single image with the two original
;;          images side-by-side.
;; beside : image image -> image
(check-expect
 (beside
  (circle 30 'solid 'red)
  (circle 10 'solid 'blue))
 (overlay/xy (circle 30 'solid 'red)
             40 0
             (circle 10 'solid 'blue)))
(define (beside image1 image2)
  (overlay/xy
   image1
   (+
    (- (image-width image1) 
       (pinhole-x image1))
    (pinhole-x image2))
   0
   image2))



;; TYPE DEFINITION
(define-struct row (left mid right))
;; make-row: square square square -> row

;; EXAMPLE DATA
(define row1 (make-row 'circle 'blank 'cross))
(define row2 (make-row 'circle 'cross 'blank))
(define row3 (make-row 'blank 'blank 'blank))
(define row4 (make-row 'blank 'blank 'circle))
(define row5 (make-row 'cross 'blank 'circle))

;; TEMPLATE
;(define (a-row-function a-row)
;  ... (row-left a-row)      ;; is a square
;  ... (row-mid a-row)       ;; is a square
;  ... (row-right a-row))    ;; is a square

;; draw-row : Given a row, return its image, namely the three square next to each other.
;; draw-row : row -> image
(check-expect (draw-row (make-row 'circle 'blank 'cross))
              (beside (draw-square 'circle)
                      (beside (draw-square 'blank)
                              (draw-square 'cross))))
(check-expect (draw-row (make-row 'circle 'cross 'blank))
              (beside (draw-square 'circle)
                      (beside (draw-square 'cross)
                              (draw-square 'blank))))
(check-expect (draw-row (make-row 'blank 'blank 'blank))
              (beside (draw-square 'blank)
                      (beside (draw-square 'blank)
                              (draw-square 'blank))))
(define (draw-row a-row)
  (beside (draw-square (row-left a-row))
          (beside (draw-square (row-mid a-row))
                  (draw-square (row-right a-row)))))
(draw-row (make-row 'circle 'blank 'cross))

;; ==== PART #4 : Practice of nested structures (board) ====

;; TYPE DEFINITION
(define-struct board (top-row center-row bottom-row))
;; make-board : row row row -> board

(define empty-row (make-row 'blank 'blank 'blank))
(define empty-board (make-board empty-row empty-row empty-row))


;; TEMPLATE
;(define (a-board-function a-board)
;  ... (board-top-row a-board)  ;; is a row
;  ... (board-middle-row a-board) ;; is a row
;  ... (board-lower-row a-board)) ;; is a row

;; on-top : Given two images, returns an image consisting of the
;;          first placed on top of the other.
;; on-top : image image -> image
(check-expect (on-top
               (put-pinhole (circle 30 'solid 'red) 0 0)
               (circle 10 'solid 'blue))
              (overlay/xy (put-pinhole (circle 30 'solid 'red) 0 0)
                          0 70
                          (circle 10 'solid 'blue)))
(define (on-top image1 image2)
  (overlay/xy
   image1
   0
   (+
    (- (image-height image1) 
       (pinhole-y image1))
    (pinhole-y image2))
   image2))


;; draw-board : Given a board, returns its image.
;; draw-board : board -> image
(check-expect (draw-board (make-board row1 row2 row3))
              (on-top (on-top (draw-row row1)
                              (draw-row row2))
                      (draw-row row3)))
(define (draw-board a-board)
  (on-top (on-top (draw-row (board-top-row a-board))  
                  (draw-row (board-center-row a-board))) 
          (draw-row (board-bottom-row a-board)))) 
(define example-board-image (draw-board (make-board row1 row2 row3)))

;; ==== PART #5 : Practice COND


;; which-column : Given the x coordinate of the mouse click  and returns 
;;                the symbol 'L, the symbol 'M, or the symbol 'R,
;;                depending on whether that X position falls on the right,
;;                the middle or the left of the board.
;; which-column : number -> symbol
(check-expect (which-column 50) 'L)
(check-expect (which-column 130) 'M)
(check-expect (which-column 230) 'R)
(define (which-column x-pos)
  (cond[(and (>= x-pos 0)(<= x-pos square-width))'L]
       [(and (>= x-pos (+ square-width 1))(<= x-pos (* 2 square-width)))'M]
       [(and (>= x-pos (+ (* 2 square-width) 1))(<= x-pos (* 3 square-width)))'R]))


;; which-row : Given the y coordinate of the mouse click and returns 
;;             the symbol 'T, the symbol 'C, or the symbol 'B,
;;             depending on whether that Y position falls on the top, 
;;             the center or the bottom of the board.
;; which-row : number -> symbol
(check-expect (which-row 50) 'T)
(check-expect (which-row 130) 'C)
(check-expect (which-row 230) 'B)
(define (which-row y-pos)
  (cond[(and (>= y-pos 0)(<= y-pos square-width))'T]
       [(and (>= y-pos (+ square-width 1))(<= y-pos (* 2 square-width)))'C]
       [(and (>= y-pos (+ (* 2 square-width) 1))(<= y-pos (* 3 square-width)))'B]))


;; ==== PART #6 : Practice structure update (make-row) ====

;; play-on-left: Update the left square of the row with the given play.
;; play-on-left: row square -> row
(check-expect (play-on-left (make-row 'blank 'cross 'circle) 'circle)
              (make-row 'circle 'cross 'circle))
(define (play-on-left row play)
  (make-row play (row-mid row) (row-right row)))

;; play-on-middle: Update the middle square of the row with the given play.
;; play-on-middle: row square -> row
(check-expect (play-on-middle (make-row 'blank 'cross 'circle) 'circle)
              (make-row 'blank 'circle 'circle))
(define (play-on-middle row play)
  (make-row (row-left row) play (row-right row)))

;; play-on-right: Update the right square of the row with the given play.
;; play-on-right: row square -> row
(check-expect (play-on-right (make-row 'blank 'cross 'circle) 'cross)
              (make-row 'blank 'cross 'cross))
(define (play-on-right row play)
  (make-row (row-left row) (row-mid row) play))

;; ==== PART #7 : Praticing COND dispatch ====

;; play-on-row : Update the row with the given play at the given location.
;; play-on-row : row square symbol -> row
(check-expect (play-on-row (make-row 'blank 'cross 'circle) 'circle 'L)
              (make-row 'circle 'cross 'circle))
(check-expect (play-on-row (make-row 'blank 'cross 'circle) 'circle 'M)
              (make-row 'blank 'circle 'circle))
(check-expect (play-on-row (make-row 'blank 'cross 'circle) 'cross 'R)
              (make-row 'blank 'cross 'cross))
(define (play-on-row row play a-hp)
  (cond [(symbol=? a-hp 'L) (play-on-left row play)]
        [(symbol=? a-hp 'M) (play-on-middle row play)]
        [(symbol=? a-hp 'R) (play-on-right row play)]))


;; ==== PART #8 : Pratice updating nested structures ====

;; play-on-board-at-top : Update the board with the play, at the given horz. position, on the top row.
;; play-on-board-at-top : board square symbol -> board
(check-expect (play-on-board-at-top empty-board 'cross 'R)
              (make-board (make-row 'blank 'blank 'cross)
                          empty-row empty-row))
(define (play-on-board-at-top a-board play a-hp)
  (make-board (play-on-row (board-top-row a-board) play a-hp)  
              (board-center-row a-board) 
              (board-bottom-row a-board))) 

;; play-on-board-at-middle : Update the board with the play, at the given horz. position, on the center row.
;; play-on-board-at-middle : board square symbol -> board
(check-expect (play-on-board-at-center empty-board 'cross 'L)
              (make-board empty-row (make-row 'cross 'blank 'blank) empty-row))
(define (play-on-board-at-center a-board play a-hp)
  (make-board (board-top-row a-board)  
              (play-on-row (board-center-row a-board) play a-hp)
              (board-bottom-row a-board))) 

;; play-on-board-at-bottom : Update the board with the play, at the given horz. position, on the bottom row.
;; play-on-board-at-bottom : board square symbol -> board
(check-expect (play-on-board-at-bottom empty-board 'circle 'M)
              (make-board empty-row empty-row (make-row 'blank 'circle 'blank)))
(define (play-on-board-at-bottom a-board play a-hp)
  (make-board (board-top-row a-board) 
              (board-center-row a-board) 
              (play-on-row (board-bottom-row a-board) play a-hp)))


;; ==== PART #9 : COND dispatch combined with updating nested structures ====

;; play-on-board : Update the board with the given play at the given horz. and vert. position.
;; play-on-board : board square symbol symbol -> board
(check-expect (play-on-board empty-board 'cross 'R 'T)
              (make-board (make-row 'blank 'blank 'cross)
                          empty-row
                          empty-row))
(check-expect (play-on-board empty-board 'circle 'M 'C)
              (make-board empty-row
                          (make-row 'blank 'circle 'blank)
                          empty-row))
(check-expect (play-on-board empty-board 'cross 'L 'B)
              (make-board empty-row
                          empty-row
                          (make-row 'cross 'blank 'blank)))
(define (play-on-board board play a-hp a-vp)
  (cond [(symbol=? a-vp 'T) (play-on-board-at-top board play a-hp)]
        [(symbol=? a-vp 'C) (play-on-board-at-center board play a-hp)]
        [(symbol=? a-vp 'B) (play-on-board-at-bottom board play a-hp)]))


;; ==== PART #10 : Animate the board with mouse clicks ====

(define width (image-width example-board-image))
(define height (image-height example-board-image))

;; board->scene : Draw a board.
;; board->scene : board -> scene
(define (board->scene board)
  (place-image (put-pinhole (draw-board board) 0 0)
               0 0
               (empty-scene width height)))

;; clack1 : Mouse handler. Plays where the mouse is clicked on button-up.
;; clack1 : board number number symbol -> board
(check-expect (clack1 empty-board 40 50 'button-down) empty-board)
(check-expect (clack1 empty-board 210 290 'button-up) 
              (make-board empty-row
                          empty-row
                          (make-row 'blank 'blank 'cross)))
(check-expect (clack1 empty-board 40 50 'button-up) 
              (make-board (make-row 'cross 'blank 'blank)
                          empty-row empty-row))
(define (clack1 board x y event)
  (cond [(symbol=? event 'button-up)
         (play-on-board board 'cross (which-column x) (which-row y))]
        [else board]))




;; ==== PART #11 : doubly-nested structure (track who's next) ====

;; other-player : Given the current play, return which player goes next.
;; other-player : square -> square
(check-expect (other-player 'cross) 'circle)
(check-expect (other-player 'circle) 'cross)
(define (other-player play)
  (cond [(symbol=? play 'cross) 'circle]
        [else 'cross]))

;; lookup-square : Given a horz. pos, finds the content of that square
;; lookup-square : row symbol -> square
(define (lookup-square row a-hp)
  (cond [(symbol=? a-hp 'L) (row-left row)]
        [(symbol=? a-hp 'M) (row-mid row)]
        [(symbol=? a-hp 'R) (row-right row)]))

;; lookup-square : Given a vert. pos, finds that row.
;; lookup-square : board symbol -> row
(define (lookup-row board a-vp)
  (cond [(symbol=? a-vp 'T) (board-top-row board)]
        [(symbol=? a-vp 'C) (board-center-row board)]
        [(symbol=? a-vp 'B) (board-bottom-row board)]))

;; lookup : Given a horz. and a vert. pos, finds that square.
;; lookup : board symbol symbol -> square
(define (lookup board a-hp a-vp)
  (lookup-square (lookup-row board a-vp) a-hp))

;; move-legal? : Return true if the square at horz. and vert position is blank.
;; move-legal? : board square symbol symbol -> boolean
(check-expect (move-legal? empty-board 'L 'C) true)
(check-expect (move-legal? (make-board empty-row 
                                       (make-row 'circle 'cross cross)
                                       empty-row)                                        
                           'M 'C) false)
(define (move-legal? board horizontal-pos vertical-pos)
  (symbol=? (lookup board horizontal-pos vertical-pos)
            'blank))

(define-struct game (next-player board move-count))
;; make-game : square board -> game
(define initial-game (make-game 'cross empty-board 0))

;; play : Given a game and a horz. and vert. position, the next player 
;;        plays in that square, if legal. The next player then switches hand.
;; play : game symbol symbol -> game
(check-expect (play-game initial-game 'L 'C)
              (make-game 'circle (make-board empty-row
                                             (make-row 'cross 'blank 'blank)
                                             empty-row)
                         1))
(define (play-game game horizontal-pos vertival-pos)
  (cond [(move-legal? (game-board game) horizontal-pos vertival-pos)
         (make-game (other-player (game-next-player game))
                    (play-on-board (game-board game)
                                   (game-next-player game) horizontal-pos vertival-pos)
                    (+ 1 (game-move-count game)))]
        [else game]))

;; clack2 : Mouse handler. Plays the game on button-up.
;; clack2 : game number number symbol -> game
(define (clack2 game x y event)
  (cond [(symbol=? event 'button-up)
         (play-game game (which-column x) (which-row y))]
        [else game]))

;; game->scene : Draw a game
;; game->scene : game -> scene
(define (game->scene game)
  (board->scene (game-board game)))

;;(big-bang width
;;          height
;;          1
;;          initial-game)
;;(on-redraw game->scene)
;;(on-mouse-event clack2)

;; ==== PART #12 : To check who wins ====

;; winning-triple? : Return true if a, b, and c are all the same symbol as player.
;; winning-triple? : symbol symbol symbol symbol -> boolean
(check-expect (winning-triple? 'circle 'circle 'circle 'circle) true)
(check-expect (winning-triple? 'circle 'cross 'circle 'circle) false)
(check-expect (winning-triple? 'cross 'cross 'circle 'cross) false)
(check-expect (winning-triple? 'cross 'cross 'cross 'cross) true)
(define (winning-triple? player a b c)
  (and (symbol=? player a)
       (symbol=? player b)
       (symbol=? player c)))

;; winning-row? : Returns true if the indicated row is a win for the given player.
;; winning-row? : board square symbol -> boolean
(check-expect (winning-row? (make-board (make-row 'circle 'circle 'circle) empty-row empty-row) 
                            'circle 'T) true)
(check-expect (winning-row? (make-board (make-row 'circle 'cross 'circle) empty-row empty-row) 
                            'circle 'T) false)
(check-expect (winning-row? empty-board
                            'circle 'B) false)
(check-expect (winning-row? (make-board empty-row (make-row 'circle 'circle 'circle) empty-row)
                            'circle 'C) true)
(define (winning-row? board player vertical-pos)
  (winning-triple? player 
                   (lookup board 'L vertical-pos)
                   (lookup board 'M vertical-pos)
                   (lookup board 'R vertical-pos)))

;; winning-column? : Return true if the indicated column is a win for the given player.
;; winnnig-column? : board square symbol -> boolean
(check-expect (winning-column? (make-board row1 row2 row2) 'circle 'L) true)
(check-expect (winning-column? (make-board row1 row2 row2) 'cross 'L) false)
(check-expect (winning-column? (make-board row1 row2 row2) 'cross 'M) false)
(check-expect (winning-column? (make-board row4 row5 row5) 'circle 'R) true)
(define (winning-column? board player horizontal-pos)
  (winning-triple? player 
                   (lookup board horizontal-pos 'T)
                   (lookup board horizontal-pos 'C)
                   (lookup board horizontal-pos 'B)))

;; winning-down-diagonal? : Return true if the top-left to bottom-right diagonal is a win.
;; winning-down-diagonal? : board square -> boolean
(check-expect (winning-down-diagonal? (make-board row5 row2 row1) 'cross) true)
(check-expect (winning-down-diagonal? (make-board row1 row2 row5) 'circle) false)
(check-expect (winning-down-diagonal? (make-board row5 row2 row2) 'cross) false)
(define (winning-down-diagonal? board player)
  (winning-triple? player (lookup board 'L 'T) (lookup board 'M 'C) (lookup board 'R 'B)))

;; winning-up-diagonal? : Return true if the bottom-left to top-right diagonal is a win.
;; winning-up-diagonal? : board square -> boolean
(check-expect (winning-up-diagonal? (make-board row1 row2 row5) 'cross) true)
(check-expect (winning-up-diagonal? (make-board row5 row2 row1) 'cross) false)
(check-expect (winning-up-diagonal? (make-board row1 row2 row5) 'circle) false)
(define (winning-up-diagonal? board player)
  (winning-triple? player (lookup board 'L 'B) (lookup board 'M 'C) (lookup board 'R 'T)))

;; winning-board? : Returns true if the given board is a win for the given player.
;; winning-board? : board square -> boolean
(check-expect (winning-board? empty-board 'cross) false)
(check-expect (winning-board? empty-board 'circle) false)
(check-expect (winning-board? (make-board row1 row2 row5) 'cross) true)
(check-expect (winning-board? (make-board row1 row2 row5) 'cross) true)
(define (winning-board? board player)
  (or (winning-row? board player 'T)
      (winning-row? board player 'C)
      (winning-row? board player 'B)
      (winning-column? board player 'L)
      (winning-column? board player 'M)
      (winning-column? board player 'R)
      (winning-down-diagonal? board player)
      (winning-up-diagonal? board player)))

;; ==== PART #13 : Draw a line over the winner ====

(define horz-line (line (* 2 square-width) 0 'orange))
(define vert-line (line 0 (* 2 square-width) 'orange))
(define down-diag-line (line (* 2 square-width) (* 2 square-width) 'orange))
(define up-diag-line (line (* 2 square-width) (* -2 square-width) 'orange))
(define half-square (/ square-width 2))

;; win->scene : Draws the board the a line highlight the triple of the winner, assuming 
;;              the given board is a win for the given player.
;; win->scene : board square -> scene
(define (win->scene board player)
  (cond [(winning-row? board player 'T) 
         (place-image horz-line half-square half-square (board->scene board))]
        [(winning-row? board player 'C) 
         (place-image horz-line half-square (* 3 half-square) (board->scene board))]
        [(winning-row? board player 'B)
         (place-image horz-line half-square (* 5 half-square) (board->scene board))]
        [(winning-column? board player 'L)
         (place-image vert-line half-square half-square (board->scene board))]
        [(winning-column? board player 'M)
         (place-image vert-line (* 3 half-square) half-square (board->scene board))]
        [(winning-column? board player 'R)
         (place-image vert-line (* 5 half-square) half-square (board->scene board))]
        [(winning-down-diagonal? board player)
         (place-image down-diag-line half-square half-square (board->scene board))]
        [(winning-up-diagonal? board player) 
         (place-image up-diag-line half-square (* 5 half-square) (board->scene board))]))

;; game->scene : Draw a game, and a winning line, if any.
;; game->scene : game -> scene
(define (game->scene-with-win game)
  (cond [(winning-board? (game-board game) 'cross)
         (win->scene (game-board game) 'cross)]
        [(winning-board? (game-board game) 'circle)
         (win->scene (game-board game) 'circle)]
        [else (board->scene (game-board game))]))

;; game-over? : Returns true when the game is over.
;; game-over? : game -> boolean
(check-expect (game-over? initial-game) false)
(check-expect (game-over? (make-game 'circle (make-board row1 row2 row5) 4)) true)
(check-expect (game-over? (make-game 'circle empty-board 9)) true)
(define (game-over? game)
  (or (winning-board? (game-board game) 'cross)
      (winning-board? (game-board game) 'circle)
      (= (game-move-count game) 9)))

;(big-bang width
;          height
;          1
;          initial-game)
;(on-redraw game->scene-with-win)
;(on-mouse-event clack2)
;(stop-when game-over?)


;; ==== PART #14 : The computer player ====

;; winning-game? : Return true if the game has just been won by the player who just played.
;; winning-game? : game -> boolean
(check-expect (winning-game? (make-game 'circle (make-board row1 row2 row5) 4)) true)
(check-expect (winning-game? (make-game 'cross (make-board row1 row2 row5) 4)) false)
(check-expect (winning-game? (make-game 'circle empty-board 9)) false)
(define (winning-game? game)
  (winning-board? (game-board game) (other-player (game-next-player game))))

;; try-to-win : Try all the possible moves, play the first one that wins right away.
;; try-to-win : game -> game
(check-expect (try-to-win (make-game 'circle (make-board row1 row1 row3) 3))
              (make-game 'cross (make-board row1 row1 (make-row 'circle 'blank 'blank)) 4))
(check-expect (try-to-win (make-game 'cross (make-board row1 row1 row3) 3))
              (make-game 'circle (make-board row1 row1 (make-row 'blank 'blank 'cross)) 4))
(check-expect (try-to-win initial-game) false)
(define (try-to-win game)
  (cond [(winning-game? (play-game game 'L 'T))
         (play-game game 'L 'T)]
        [(winning-game? (play-game game 'M 'T))
         (play-game game 'M 'T)]
        [(winning-game? (play-game game 'R 'T))
         (play-game game 'R 'T)]
        [(winning-game? (play-game game 'L 'C))
         (play-game game 'L 'C)]
        [(winning-game? (play-game game 'M 'C))
         (play-game game 'M 'C)]
        [(winning-game? (play-game game 'R 'C))
         (play-game game 'R 'C)]
        [(winning-game? (play-game game 'L 'B))
         (play-game game 'L 'B)]
        [(winning-game? (play-game game 'M 'B))
         (play-game game 'M 'B)]
        [(winning-game? (play-game game 'R 'B))
         (play-game game 'R 'B)]
        [else false]))

;; pass : Plays a pass (which is not legal is Tic Tac Toe, but we are using it as a helper function)
;; pass : game -> game
(define (pass game)
  (make-game (other-player (game-next-player game))
             (game-board game)
             (game-move-count game)))

;; try-to-win : Try all the possible moves for the opponent,
;;              play where the opponent would play to win.
;; try-to-win : game -> game
(check-expect (try-to-block (make-game 'circle (make-board row1 row1 row3) 3))
              (make-game 'cross (make-board row1 row1 (make-row 'blank 'blank 'circle)) 4))
(check-expect (try-to-block (make-game 'cross (make-board row1 row1 row3) 3))
              (make-game 'circle (make-board row1 row1 (make-row 'cross 'blank 'blank)) 4))
(check-expect (try-to-block initial-game) false)
(define (try-to-block game)
  (cond [(winning-game? (play-game (pass game) 'L 'T))
         (play-game game 'L 'T)]
        [(winning-game? (play-game (pass game) 'M 'T))
         (play-game game 'M 'T)]
        [(winning-game? (play-game (pass game) 'R 'T))
         (play-game game 'R 'T)]
        [(winning-game? (play-game (pass game) 'L 'C))
         (play-game game 'L 'C)]
        [(winning-game? (play-game (pass game) 'M 'C))
         (play-game game 'M 'C)]
        [(winning-game? (play-game (pass game) 'R 'C))
         (play-game game 'R 'C)]
        [(winning-game? (play-game (pass game) 'L 'B))
         (play-game game 'L 'B)]
        [(winning-game? (play-game (pass game) 'M 'B))
         (play-game game 'M 'B)]
        [(winning-game? (play-game (pass game) 'R 'B))
         (play-game game 'R 'B)]
        [else false]))

;
;(define row1 (make-row 'circle 'blank 'cross))
;(define row2 (make-row 'circle 'cross 'blank))
;(define row3 (make-row 'blank 'blank 'blank))
;(define row4 (make-row 'blank 'blank 'circle))
;(define row5 (make-row 'cross 'blank 'circle))

;; try-to-play-somewhere-good : Try to play in the center, then in the corners, then in the edges.
;; try-to-play-somewhere-good : game -> game
(check-expect (try-to-play-somewhere-good (make-game 'circle (make-board row1 row1 row3) 3))
              (make-game 'cross (make-board row1 (make-row 'circle 'circle 'cross) row3) 4))
(check-expect (try-to-play-somewhere-good (make-game 'cross (make-board row1 row2 row3) 3))
              (make-game 'circle (make-board row1 row2 (make-row 'cross 'blank 'blank)) 4))
(check-expect (try-to-play-somewhere-good initial-game) 
              (make-game 'circle (make-board empty-row (make-row 'blank 'cross 'blank) empty-row) 1))
(define (try-to-play-somewhere-good game) 
  (cond  ;; Try the center
    [(move-legal? (game-board game) 'M 'C)
     (play-game game 'M 'C)]
    ;; Try the corners
    [(move-legal? (game-board game) 'L 'T)
     (play-game game 'L 'T)]
    [(move-legal? (game-board game) 'R 'T)
     (play-game game 'R 'T)]
    [(move-legal? (game-board game) 'L 'B)
     (play-game game 'L 'B)]
    [(move-legal? (game-board game) 'R 'B)
     (play-game game 'R 'B)]
    ;; Try the edges
    [(move-legal? (game-board game) 'M 'T)
     (play-game game 'M 'T)]
    [(move-legal? (game-board game) 'M 'B)
     (play-game game 'M 'B)]
    [(move-legal? (game-board game) 'L 'C)
     (play-game game 'L 'C)]
    [(move-legal? (game-board game) 'R 'C)
     (play-game game 'R 'C)]))

;; computer-play : The computer player. Doesn't play if the game is over.
;;                 Otherwise, tries to win, tries to block, then tries to play well.
;; computer-play : game -> game
(define (computer-play game)
  (cond [(game-over? game) ;; TRICKY : program crashes without this case.
         game]
        [(game? (try-to-win game)) ;; TRICKY : this require a union contract.
         (try-to-win game)]
        [(game? (try-to-block game))
         (try-to-block game)]
        [else (try-to-play-somewhere-good game)]))

;; clack3 : Mouse handler. Plays where the mouse was clicked on button up,
;;          then adds the move by the computer.
;; clack3 : game number number symbol -> game
(define (clack3 game x y event)
  (cond [(symbol=? event 'button-up)
         (computer-play (play-game game (which-column x) (which-row y)))]
        [else game]))

(big-bang width
          height
          1
          initial-game)
(on-redraw game->scene-with-win)
(on-mouse-event clack3)
(stop-when game-over?)

















