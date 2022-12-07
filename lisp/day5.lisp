(ql:quickload "cl-ppcre")

(defpackage #:com.adventofcode/day5
  (:use #:common-lisp)
  (:export
   #:puzzle-1
   #:puzzle-2))

(in-package #:com.adventofcode/day5)

(defparameter *input*
  #P"../input/day5.txt")

(defun list-dimensions (list &optional (depth 1))
  (loop :repeat depth
        :collect (length list)
        :do (setf list (car list))))

(defun list-to-array (list &optional (depth 1))
  (make-array (list-dimensions list depth)
              :initial-contents list))

(defun read-initial-state (stream &optional (size 9))
  "Read initial state from STREAM."
  (let ((initial-lines
	  (list-to-array
	   (loop :for line = (read-line stream nil nil)
		 :while (and line (not (string= "" line)))
		 :collect line)))
	(initial-state
	  (make-array size :initial-element nil)))
    (dotimes (i (length initial-lines))
      (dotimes (j size)
	(push (char (aref initial-lines i) (1+ (* 4 j) ))
	      (aref initial-state j))))
    (dotimes (j size)
      (setf
       (aref initial-state j)
       (rest
	(delete #\Space (aref initial-state j)))))
    initial-state))

(defparameter *move-scanner*
  (ppcre:create-scanner "^move ([0-9]+) from ([0-9]+) to ([0-9]+)$"))

(defun read-move (stream)
  (let ((line
	  (read-line stream nil nil)))
    (unless line
      (return-from read-move))
    (ppcre:register-groups-bind ((#'parse-integer count source destination))
	(*move-scanner* line)
      (list count source destination))))

(defun perform-move (state count source destination)
  (dotimes (_ count)
    (setf
     (aref state (1- destination))
     (nconc
      (aref state (1- destination))
      (last
       (aref state (1- source))))
     (aref state (1- source))
     (nbutlast (aref state (1- source))))))

(defun print-crates (crates)
  (format t "~&Crates: ~A" crates))

(defun process-moves (stream &optional (size 9))
  (loop :with state = (read-initial-state stream size)
	:for move = (read-move stream)
	:while move
	:do (apply #'perform-move state move)
	:finally (return state)))

(defparameter *example-1*
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defun example-1 ()
  (with-input-from-string (stream *example-1*)
    (process-moves stream 3)))

(defun puzzle-1 ()
  (with-open-file (stream *input*)
    (map 'string #'(lambda (crates) (car (last crates)))  (process-moves stream))))

