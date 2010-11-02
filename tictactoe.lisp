
;;;; tictactoe.lisp
;;;; 
;;;; Andrew Levenson
;;;; 10/27/10
;;;;
;;;; Simple two player ASCII Tic Tac Toe Game

(defun create-board ()
  "Creates an array with the contents 1-9."
  (setf *board* (make-array 9
			    :initial-contents
			    '(1 2 3 4 5 6 7 8 9))))

(defun x-coordinate (choice)
  "Computes the given cell's x coordinate."
  (mod (1- choice) 3))

(defun y-coordinate (choice)
  "Computes the given cell's y coordinate."
  (floor (/ (1- choice) 3)))

(defun ref-cell (x y)
  "Returns the value in cell (x, y)."
  (aref *board* (+ (* y 3) x)))

(defun set-cell (x y)
  "Sets the value in cell (x, y)."
  (setf (aref *board* (+ (* y 3) x)) *marker*))

(defun welcome-player ()
  "Welcomes the player and begins a game."
  (format t "Welcome to TicTacToe!~%~%")
  (create-board)
  (play nil))
	
(defun switch-player ()
  "Switches the active player."
  (if (equal *marker* :X)
        (setf *marker* :O)
	(setf *marker* :X))
  (if (equal *player* "Player 1")
	(setf *player* "Player 2")
	(setf *player* "Player 1")))

(defun draw-board (&optional numbers-p)
  "Displays the board. If numbers-p is non-nil, 
displays numbers in vacant spaces." 
  (format t "     |     |     ~%")
  (if numbers-p 
      (format t "   ~a |   ~a |   ~a  ~%" 
	      (ref-cell 0 0) (ref-cell 1 0) (ref-cell 2 0))
      (format t "     |     |     ~%"))
  (format t "     |     |     ~%")
  (format t "_________________~%")
  (format t "     |     |     ~%")
  (if numbers-p 
      (format t "   ~a |   ~a |   ~a  ~%" 
	      (ref-cell 0 1) (ref-cell 1 1) (ref-cell 2 1))
      (format t "     |     |     ~%"))
  (format t "     |     |     ~%")
  (format t "_________________~%")
  (format t "     |     |     ~%")
  (if numbers-p 
      (format t "   ~a |   ~a |   ~a  ~%" 
	      (ref-cell 0 2) (ref-cell 1 2) (ref-cell 2 2))
      (format t "     |     |     ~%"))
  (format t "     |     |     ~%~%"))

(defun play (&optional switch-p)
  "If switch-p is true, switches the active player. 
Plays a move; Checks if the move is a win or a stalemate.
If neither is true, recursively calls itself. If either is true,
prompts for a new game."
  (when switch-p (switch-player))
  (check-choice (read-choice))
  (when (and 
	  (not (check-for-win-p)) 
	  (not (stalemate-p)))
    (play t))
  (when (check-for-win-p)
    (progn
      (format t "~a has won! " *player*)
      (force-output nil)
      (if (y-or-n-p "Play again? ")
	  (play-again)
	  (quit))))
  (when (stalemate-p)
    (if (y-or-n-p "~%~%Stalemate! Play again? ")
	(play-again)
	(quit))))

(defun play-again ()
  "Recreates the board, switches the active player, and starts a new game."
  (create-board)
  (switch-player)
  (format t "This game will be started by ~a.~%~%" *player*)
  (play))

(defun read-choice ()
  "Displays the board and prompts the player to enter a choice, or to
see their options, then reads in and returns their choice."
  (draw-board t)
  (format t "~a, select a number to choose a square.~%" *player*) 
  (force-output nil)
  (parse-integer (read-line *query-io*) :junk-allowed t))
 
(defun check-choice (choice)
  "Checks to ensure the user input is valid,
i.e. an integer value between 1 and 9, inclusive."
  (if (and (numberp choice)
           (<= 1 choice 9))
      (select choice)
    (progn (format t "~%Invalid choice.~%")
           (check-choice (read-choice)))))

(defun select (choice)
  "Checks if the cell has not been selected (contains a number).
If so, places the players marker in the cell.
Otherwise, prompts the player to try again."
  (if (numberp (ref-cell (x-coordinate choice) (y-coordinate choice)))
      (set-cell  (x-coordinate choice) (y-coordinate choice))
      (invalid-selection)))

(defun invalid-selection ()
  "Warns the player if they choose a spot that has been taken,
and repeats the entry process."
  (format t "That spot is taken. Please choose another spot.~%~%")
  (force-output nil)
  (check-choice (read-choice)))

(defun check-for-win-p ()
  "Checks to see if any lines have been made. Returns t or nil."
  (or (is-line-p 1 2 3)
      (is-line-p 1 4 7)
      (is-line-p 1 5 9)
      (is-line-p 2 5 8)
      (is-line-p 3 6 9)
      (is-line-p 3 5 7)
      (is-line-p 4 5 6)
      (is-line-p 7 8 9)))

(defun is-line-p (a b c)
  "Tests to see if the three cells fed to it as arguments
contain identical values. Returns t or nil."
  (and
   (equal
    (ref-cell (x-coordinate a) (y-coordinate a))
    (ref-cell (x-coordinate b) (y-coordinate b)))
   (equal
    (ref-cell (x-coordinate a) (y-coordinate a))
    (ref-cell (x-coordinate c) (y-coordinate c)))))

(defun stalemate-p ()
  "Checks to see if there is a stalemate,
i.e. none of the board's cells contain a number.
Returns t or nil."
  (notany #'numberp *board*))

(setf *marker* :X)
(setf *player* "Player 1")

(welcome-player)
