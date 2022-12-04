(ql:quickload "cl-ppcre")

(defpackage #:com.adventofcode/day4
  (:use #:common-lisp)
  (:export
   #:puzzle-1
   #:puzzle-2))

(in-package #:com.adventofcode/day4)

(defparameter *assignment-scanner*
  (ppcre:create-scanner "^([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)$"))


(defun read-assignment-pair (stream)
  (let ((line
	  (read-line stream nil nil)))
    (unless line
      (return-from read-assignment-pair))
    (ppcre:register-groups-bind ((#'parse-integer first second third fourth))
	(*assignment-scanner* line)
      (list first second third fourth))))

(defun one-assignment-fully-contains-the-other-p (assignment-pair)
  (destructuring-bind (first second third fourth) assignment-pair
    (or (and (<= first third) (>= second fourth))
	(and (>= first third) (<= second fourth)))))

(defun overlap-at-all-p (assignment-pair)
  (destructuring-bind (first second third fourth) assignment-pair
    (not
     (or (< second third)
	 (> first fourth)))))

(defun puzzle-1 ()
  (with-open-file (stream #p"../input/day4.txt")
    (loop :for assignment-pair = (read-assignment-pair stream)
	  :while assignment-pair
	  :counting (one-assignment-fully-contains-the-other-p assignment-pair))))

(defun puzzle-2 ()
  (with-open-file (stream #p"../input/day4.txt")
    (loop :for assignment-pair = (read-assignment-pair stream)
	  :while assignment-pair
	  :counting (overlap-at-all-p assignment-pair))))
