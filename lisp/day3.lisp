(ql:quickload "cl-ppcre")

(defpackage #:com.adventofcode/day3
  (:use #:common-lisp)
  (:export
   #:puzzle-1
   #:puzzle-2))

(in-package #:com.adventofcode/day3)

(defparameter *example*
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defclass rucksack nil
  ((first
    :initarg :first
    :accessor rucksack-first)
   (second
    :initarg :second
    :accessor rucksack-second)))

(defmethod print-object ((instance rucksack) stream)
  (print-unreadable-object (instance stream :type t)
    (with-slots (first second) instance
      (format stream " first: ~S" first)
      (format stream " second: ~S" second))))

(defmethod describe-object ((instance rucksack) stream)
  (with-slots (first second) instance
    (format stream "~&First Compartment: ~{~A~^, ~}" first)
    (format stream "~&Second Compartment: ~{~A~^, ~}" second))
  (values))

(defun make-rucksack (&rest initargs &key first second)
  "Make a rucksack."
  (declare (ignore first second))
  (apply #'make-instance 'rucksack initargs))

(defun rucksack-items (rucksack)
  (with-slots (first second) rucksack
    (append first second)))

(defun read-rucksack (stream)
  (let ((line
	  (read-line stream nil nil)))
    (unless line
      (return-from read-rucksack))
    (unless (evenp (length line))
      (error "Line of length ~A (~S) does not encode a rucksack." (length line) line))
    (let ((compartment-length
	    (/ (length line) 2)))
      (make-rucksack
       :first (coerce (subseq line 0 compartment-length) 'list)
       :second (coerce (subseq line compartment-length) 'list)))))

(defun read-rucksack-list (stream)
  (loop :for rucksack = (read-rucksack stream)
	:while rucksack
	:collect rucksack))

(defun input-rucksack-list (concrete-designator)
  (typecase concrete-designator
    (string
     (with-input-from-string (stream concrete-designator)
       (read-rucksack-list stream)))
    (stream
     (read-rucksack-list concrete-designator))
    (pathname
     (with-open-file (stream concrete-designator)
       (read-rucksack-list stream)))
    (list
     concrete-designator)))

(defun item-priority (item)
  (1+ (position item "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defun cumulative-priority-of-repeated-items (rucksack)
  (with-slots (first second) rucksack
    (let ((unique-repeated-items
	    (intersection
	     (remove-duplicates first)
	     (remove-duplicates second))))
      (reduce #'+ unique-repeated-items :key #'item-priority :initial-value 0))))

(defun badge (rucksack1 rucksack2 rucksack3)
  (let ((common-items
	  (intersection
	   (remove-duplicates (rucksack-items rucksack1))
	   (intersection
	    (remove-duplicates (rucksack-items rucksack2))
	    (remove-duplicates (rucksack-items rucksack3))))))
    (when (> (length common-items) 1)
      (error "A group has no univoque badge"))
    (first common-items)))

(defun puzzle-1 ()
  (reduce #'+ (mapcar #'cumulative-priority-of-repeated-items
		      (input-rucksack-list #p"../input/day3.txt"))
	  :initial-value 0))

(defun puzzle-2 ()
  (with-open-file (stream #p"../input/day3.txt")
    (loop :for rucksack1 = (read-rucksack stream)
	  :for rucksack2 = (read-rucksack stream)
	  :for rucksack3 = (read-rucksack stream)
	  :while (and rucksack1 rucksack2 rucksack3)
	  :sum (item-priority
		(badge rucksack1 rucksack2 rucksack3)))))
