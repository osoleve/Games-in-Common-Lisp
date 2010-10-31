;;;; number-game.lisp
;;;;
;;;; Andrew Levenson
;;;; 10/25/2010
;;;;
;;;; Simple number guessing game. User has
;;;; five guesses to determine a number between
;;;; one and one hundred, inclusive (1-100).

;;; Welcome the user
(defun welcome-user ()
  (format t "Welcome to the number guessing game!~%"))

;;; Prompt for a guess
(defun prompt-for-guess ()
  (format t "Please enter your guess (1-100): ")
  (finish-output nil) ; nil directs finish-output to standard IO
  (check-guess 'read-guess))

;;; Read in a guess
(defun read-guess ()
  (let ((guess (parse-integer(read-line *query-io*))))
    (if (numberp guess) ; If true, return guess. Else, call prompt-for-guess
	(progn
	  (setq *number-of-guesses* (+ *number-of-guesses* 1))
	  guess)
	(prompt-for-guess))))

;;; Check if the guess is higher than, lower than, or equal to, the target
(defun check-guess (fn)
  (let ((guess (funcall fn)))
    (cond ((= guess *target*) (equal-to))
	  ((> guess *target*) (greater-than guess))
	  ((< guess *target*) (less-than guess)))))

;;; If the guess is equal to the target, the game is over
(defun equal-to ()
  (format t "Congratulations! You have guessed the target number, ~a!~%" *target*)
  (y-or-n-p "Play again? "))

;;; If the guess is greater than the target, inform the player.
(defun greater-than (guess)
  (format t "Sorry, ~a is greater than the target.~%" guess)
  (if (< *number-of-guesses* 5)
      (prompt-for-guess)
      (game-over)))

;;; If the guess is less than the target, inform the player.
(defun less-than (guess)
  (format t "Sorry, ~a is less than the target.~%" guess)
  (if (< *number-of-guesses* 5)
      (prompt-for-guess)
      (game-over)))

;;; If the player has run out of guesses, give them the option
;;; of playing the game again.
(defun game-over ()
  (y-or-n-p "You have run out of guesses. Play again? "))


;;; Play the game
(defun play ()
  ;; If it's their first time playing this session,
  ;; make sure to greet the user.
  (unless (> *number-of-guesses* 0)
    (welcome-user))
  ;; Reset their remaining guesses
  (setq *number-of-guesses* 0)
  ;; Initialize the global random state by "some means"
  (setf *random-state* (make-random-state t))
  ;; Set the target value.
  ;; (random) can return float values,
  ;; so we must round the result to get
  ;; an integer value.
  ;; Add one to the result, because
  ;; (random 100) yields a number between
  ;; 0 and 99, whereas we want a number
  ;; from 1 to 100 inclusive.
  (setq *target* (round (+ (random 100) 1)))			
  (if (eql (prompt-for-guess) t) ; eql is evaluated after first game
      (play)
      (quit)))

;;; Set global variable for the target number:
(defparameter *target* nil)	

;;; Set the iterator so we may check the number of guesses
(defparameter *number-of-guesses* 0)

;;; Start the game
(play)