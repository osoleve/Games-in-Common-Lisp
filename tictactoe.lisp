;;;; tictactoe.lisp
;;;; 
;;;; Andrew Levenson
;;;; 10/27/10
;;;;
;;;; Simple two player ASCII Tic Tac Toe Game

;;; First player is X, so initialize the marker to X
(setf *marker* :X)
(setf *player* "Player 1")

;;; Create the board in memory
(defun create-board ()
  ;; Board was initially a 3x3 2D Array, for realism.
  ;; Changed it to a 9 element Vector for simplification of referencing.
  (setf *board* (make-array 9
			    :initial-contents
			    '(1 2 3 4 5 6 7 8 9))))

;;; Greet the player and display the board.
(defun welcome-player ()
  (format t "Welcome to TicTacToe!~%~%")
  (create-board)
  (play nil))
	
;;; Switch the active player.
(defun switch-player ()
  (if (equal *marker* :X)
        (setf *marker* :O)
	(setf *marker* :X))
  (if (equal *player* "Player 1")
	(setf *player* "Player 2")
	(setf *player* "Player 1")))

;;; Draw the board
(defun draw-board ()
  ;; (cell-ref #) will be either 
  ;; 1. The number representing the space (before it is chosen), or
  ;; 2. A marker (:X or :O) once the space has been chosen.
  (format t "     |     |     ~%")
  (format t "   ~a |   ~a |   ~a  ~%" (cell-ref 1) (cell-ref 2) (cell-ref 3))
  (format t "     |     |     ~%")
  (format t "_________________~%")
  (format t "     |     |     ~%")
  (format t "   ~a |   ~a |   ~a  ~%" (cell-ref 4) (cell-ref 5) (cell-ref 6))
  (format t "     |     |     ~%")
  (format t "_________________~%")
  (format t "     |     |     ~%")
  (format t "   ~a |   ~a |   ~a  ~%" (cell-ref 7) (cell-ref 8) (cell-ref 9))
  (format t "     |     |     ~%~%"))

;;; Play a move
(defun play (&optional switch-p)
  ;; If the switch predicate is set to true, switch players.
  (when switch-p (switch-player))
  ;; Go through the motions for selecting a square 
  ;; and checking for correctness.
  (check-choice (read-choice))
  ;; If nobody has won and the board is not filled,
  ;; then recursively call this function with the switch predicate
  ;; so that the next player may go.
  (when (and 
	  (not (check-for-win-p)) 
	  (not (stalemate)))
    (play t))
  ;; If someone has won
  ;; 1. Alert the players as to which player won
  ;; 2. Ask if they want to play again.
  ;;    If yes, play again. If no, quit.
  (when (check-for-win-p)
    (progn
      (format t "~a has won! " *player*)
      (force-output nil)
      (if (y-or-n-p "Play again? ")
	  (play-again)
	  (quit))))
  ;; If there is a stalemate
  ;; Ask if the players wish to play again.
  ;; If yes, play again. If no, quit.
  (when (stalemate)
    (if (y-or-n-p "~%~%Stalemate! Play again? ")
	(play-again)
	(quit))))

;;; Play the game again
;;; Is only called after a win or a stalemate
(defun play-again ()
  ;; Reset the board to all numbers
  (create-board)
  ;; Switch players, so whoever ended 
  ;; the last game goes second this game.
  (switch-player)
  (format t "This game will be started by ~a.~%~%" *player*)
  (play))

;;; Allow the player to choose a square
;;; Or, if they wish, display the board with
;;; the numbers on it.
(defun read-choice ()
  ;; Show their options
  (draw-board)
  (format t "~a, select a number to choose a square.~%" *player*) 
  (force-output nil)
  ;; Read from STDIN, 
  ;; parsing an integer out, if there is any.
  (parse-integer (read-line *query-io*) :junk-allowed t))

;;; Check to make sure that the cell chosen
;;; actually exists on the board.
(defun check-choice (choice)
  (if
   ;; All of the following must be true
   (and
    ;; Must be a number (der)
    (numberp choice)
    ;; Must lie between 1 and 9, inclusive
    (> choice 0)
    (< choice 10))
   ;; If it's a valid cell, pass it to (select)
   (select choice)
   ;; Otherwise, warn the player they made an invalid
   ;; selection, and repeat the process.
   (progn
     (format t "~%Invalid choice.~%")
     (check-choice (read-choice)))))

;;; Select their cell, if it hasn't
;;; already been selected.
(defun select (choice)
  ;; If the cell contains a number 
  ;; (i.e. it has not yet been selected)
  (if (numberp (cell-ref choice))
      ;; Set the cell to their marker
      (set-cell choice)
      ;; Otherwise, warn them of an invalid selection
      (invalid-selection)))

;;; Warn the player if they choose a spot that is taken.
(defun invalid-selection ()
  (format t "That spot is taken. Please choose another spot.~%~%")
  (force-output nil)
  (check-choice (read-choice)))

;;; Check to see if there are any lines made
;;; (i.e. someone won)
;;;
;;; This function is called every time someone
;;; selects a cell, to make displaying the winner
;;; easier.
(defun check-for-win-p ()
  ;; If any of these sets of 3 cells form a line,
  ;; that means that the active player won the game.
  (or (is-line-p 1 2 3)
      (is-line-p 1 4 7)
      (is-line-p 1 5 9)
      (is-line-p 2 5 8)
      (is-line-p 3 6 9)
      (is-line-p 3 5 7)
      (is-line-p 4 5 6)
      (is-line-p 7 8 9)))

;;; If the three cell numbers passed to
;;; (is-line-p) contain identical values,
;;; then a line has been made.
(defun is-line-p (a b c)
  (and
   (equal
    (cell-ref a)
    (cell-ref b))
   (equal
    (cell-ref a)
    (cell-ref c))))

;;; Define what it means for the board to be filled.
;;; 
;;; This function is always called AFTER (check-for-win-p).
;;; This ensures that if the last move is a winning move,
;;; a stalemate is not reported.
(defun stalemate ()
  ;; If none of the spaces contain numbers
  ;; (i.e. all spaces contain a marker)
  ;; and nobody has won, then a stalemate has been reached.
  (notany #'numberp *board*))

;;; Mask the implementation of the board
;;; in terms of referencing a cell.
(defun cell-ref (cell)
  ;; (cell-ref cell) now calls an array reference
  ;; to the cell number minus one, to compensate
  ;; for the fact that the array index starts at 0.
  (aref *board* (1- cell)))

;;; Mask the implementation of the board
;;; in terms of setting a cell value.
(defun set-cell (cell)
  ;; (set-cell cell) now calls an array reference
  ;; to the cell number minus one, to compensate 
  ;; for the fact that the array index starts at 0,
  ;; and sets that reference to the marker.
  (setf (aref *board* (1- cell)) *marker*))

;;; Begin the game.
(welcome-player)
