;;;; number-game.lisp
;;;;
;;;; Andrew Levenson
;;;; 10/25/2010
;;;;
;;;; Simple number guessing game. User has
;;;; five guesses to determine a number between
;;;; one and one hundred, inclusive (1-100).

(defun welcome-user ()
  "Welcomes the user."
  (format t "Welcome to the number guessing game!~%"))

(defun prompt-for-guess ()
  "Prompts the user for their guess."
  (format t "Please enter your guess (1-100): ")
  (finish-output nil) ; nil directs finish-output to standard IO
  (check-guess 'read-guess))

(defun read-guess ()
  "Reads in the users guess. Checks to make sure it is an integer,
and increases the guess counter if it is. Otherwise, prompts the user
for another guess."
  (let ((guess (parse-integer (read-line *query-io*) :junk-allowed t)))
    (if (numberp guess)
	(progn
	  (setq *number-of-guesses* (+ *number-of-guesses* 1))
	  guess)
	(prompt-for-guess))))

(defun check-guess (fn)
  "Checks if the guess is higher than, lower than, or equal to, the target."
  (let ((guess (funcall fn)))
    (cond ((= guess *target*) (equal-to))
	  ((> guess *target*) (greater-than guess))
	  ((< guess *target*) (less-than guess)))))

(defun equal-to ()
  "If the guess is equal to the target, the game is over. 
Prompts the user to play again."
  (format t "Congratulations! You have guessed the target number, ~a!~%" *target*)
  (y-or-n-p "Play again? "))

(defun greater-than (guess)
  "If the guess is greater than the target, informs the player.
If they still have guesses left, it prompts the player for another guess."
  (format t "Sorry, ~a is greater than the target.~%" guess)
  (if (< *number-of-guesses* 5)
      (prompt-for-guess)
      (game-over)))

(defun less-than (guess)
  "If the guess is less than the target, informs the player.
If they still have guesses left, it prompts the player for another guess."
  (format t "Sorry, ~a is less than the target.~%" guess)
  (if (< *number-of-guesses* 5)
      (prompt-for-guess)
      (game-over)))

(defun game-over ()
  "If the player has run out of guesses, inform them and
prompt them to play again."
  (y-or-n-p "You have run out of guesses. Play again? "))

(defun play ()
  "Plays through the game. If the game has just been loaded,
greets the user. Resets the number of guesses, and sets the target
value to an integer between 1-100, inclusive. After the game is
over, prompts the player to play a new game."
  (unless (> *number-of-guesses* 0)
    (welcome-user))
  (setq *number-of-guesses* 0)
  (setf *random-state* (make-random-state t))
  (setq *target* (1+ (random 100)))			
  (if (eql (prompt-for-guess) t) ; eql is evaluated after first game
      (play)
      (quit)))

(defparameter *target* nil)	
(defparameter *number-of-guesses* 0)

(play) ; Start the game.