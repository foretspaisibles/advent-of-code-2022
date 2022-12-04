(ql:quickload "cl-ppcre")

(defpackage #:com.adventofcode/day2
  (:use #:common-lisp)
  (:export
   #:puzzle-1
   #:puzzle-2))

(in-package #:com.adventofcode/day2)

(defparameter *game*
  #(:rock :paper :cissors))

(defun game-previous (choice)
  (aref *game* (nth-value 1 (floor (1- (position choice *game*)) (length *game*)))))

(defun game-next (choice)
  (aref *game* (nth-value 1 (floor (1+ (position choice *game*)) (length *game*)))))

(defun game-outcome (opponent our)
  (let ((opponent-wins-to
	  (game-previous opponent))
	(opponent-loses-to
	  (game-next opponent)))
    (cond
      ((eq our opponent-wins-to)
       :loss)
      ((eq our opponent-loses-to)
       :win)
      ((eq our opponent)
       :draw)
      (t
       (error "Unknown game outcome: ~S against ~S" opponent our)))))

(defun game-move-from-outcome (opponent outcome)
  (ecase outcome
    (:loss
     (game-previous opponent))
    (:win
     (game-next opponent))
    (:draw
     opponent)))

(defparameter *example*
  "A Y
B X
C Z")

(defun read-strategy (stream &optional read-outcomes)
  (let ((round-scanner
	  (ppcre:create-scanner "^[ABC] [XYZ]$"))
	(strategy
	  nil))
    (labels
	((opponent-pick (line)
	   (ecase (char line 0)
	     (#\A :rock)
	     (#\B :paper)
	     (#\C :cissors)))
	 (our-pick (line)
	   (ecase (char line 2)
	     (#\X :rock)
	     (#\Y :paper)
	     (#\Z :cissors)))
	 (outcome (line)
	   (ecase (char line 2)
	     (#\X :loss)
	     (#\Y :draw)
	     (#\Z :win)))
	 (receive-line (line)
	   (cond
	     ((eq line nil)
	      nil)
	     ((ppcre:scan round-scanner line)
	      (push
	       (if read-outcomes
		   (list :opponent (opponent-pick line)
			 :our (game-move-from-outcome
			       (opponent-pick line)
			       (outcome line))
			 :outcome (outcome line))
		   (list :opponent (opponent-pick line)
			 :our (our-pick line)
			 :outcome (game-outcome (opponent-pick line) (our-pick line))))
	       strategy))
	     (t
	      (restart-case (error "Cannot handle line: ~S" line)
		(skip-line ()
		  t))))))
      (loop :while (receive-line (read-line stream nil nil))
	    :finally (return strategy)))))

(defun input-strategy (concrete-strategy-designator &optional read-outcomes)
  (typecase concrete-strategy-designator
    (string
     (with-input-from-string (stream concrete-strategy-designator)
       (read-strategy stream read-outcomes)))
    (stream
     (read-strategy concrete-strategy-designator read-outcomes))
    (pathname
     (with-open-file (stream concrete-strategy-designator)
       (read-strategy stream read-outcomes)))
    (list
     concrete-strategy-designator)))

(defun score (strategy)
  (labels
      ((score-outcome (outcome)
	 (ecase outcome
	   (:loss 0)
	   (:draw 3)
	   (:win 6)))
       (score-choice (our)
	 (ecase our
	   (:rock 1)
	   (:paper 2)
	   (:cissors 3)))
       (score-round (round)
	 (+ (score-outcome (getf round :outcome))
	    (score-choice (getf round :our)))))
    (reduce #'+ strategy :key #'score-round :initial-value 0)))

(defun puzzle-1 ()
  (score
   (input-strategy #p"../input/day2.txt" nil)))

(defun puzzle-2 ()
  (score
   (input-strategy #p"../input/day2.txt" t)))

