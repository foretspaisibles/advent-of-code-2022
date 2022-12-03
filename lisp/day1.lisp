(ql:quickload "cl-ppcre")

(defpackage #:com.adventofcode/day1
  (:use #:common-lisp)
  (:export
   #:puzzle-1
   #:puzzle-2))

(in-package #:com.adventofcode/day1)

(defparameter *example*
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defclass elf nil
  ((name
    :initarg :name
    :accessor elf-name)
   (snacks
    :initarg :snacks
    :accessor elf-snacks)))

(defmethod print-object ((instance elf) stream)
  (print-unreadable-object (instance stream :type t)
    (with-slots (name snacks) instance
      (format stream "~A" name)
      (when snacks
	(format stream " carrying ~A calories in ~A snacks"
		(calories snacks) (length snacks))))))

(defmethod describe-object ((instance elf) stream)
  (with-slots (name snacks) instance
    (format stream "~&Name: ~A" name)
    (format stream "~&Snacks: ~{~A~^, ~}" snacks))
  (values))

(defun make-elf (&rest initargs &key name snacks)
  "Make an Elf."
  (declare (ignore name snacks))
  (apply #'make-instance 'elf initargs))

(defun read-inventory (stream)
  (let ((elf 1)
	(snacks nil)
	(inventory nil))
    (let ((number-scanner
	    (ppcre:create-scanner "^[0-9]+$"))
	  (empty-scanner
	    (ppcre:create-scanner "^[ \t]*$")))
      (labels
	  ((receive-number (n)
	     (push n snacks)
	     t)
	   (receive-empty-line ()
	     (push (make-elf :name elf :snacks snacks) inventory)
	     (incf elf)
	     (setf snacks nil)
	     t)
	   (receive-eof ()
	     (if snacks
		 (receive-empty-line))
	     nil)
	   (receive-line (line)
	     (cond
	       ((eq nil line)
		(receive-eof))
	       ((ppcre:scan number-scanner line)
		(receive-number (parse-integer line)))
	       ((ppcre:scan empty-scanner line)
		(receive-empty-line))
	       (t
		(restart-case (error "Cannot handle line: ~S" line)
		  (skip-line ()
		    nil))))))
	(loop :while (receive-line (read-line stream nil nil))
	      :finally (return inventory))))))

(defun input-inventory (designator)
  (typecase designator
    (string
     (with-input-from-string (stream designator)
       (read-inventory stream)))
    (stream
     (read-inventory designator))
    (pathname
     (with-open-file (stream designator)
       (read-inventory stream)))
    (list
     designator)))

(defgeneric calories (object)
  (:method ((instance elf))
    (calories (elf-snacks instance)))
  (:method ((instance integer))
    instance)
  (:method ((instance list))
    (reduce #'+ instance :initial-value 0 :key #'calories))
  (:method ((instance vector))
    (reduce #'+ instance :initial-value 0 :key #'calories)))

(defun find-elf-carrying-most-calories (inventory-designator)
  (loop :for elf :in (input-inventory inventory-designator)
	:for calories = (calories elf)
	:with max-elf = 1
	:with max-calories = 0
	:when (> calories max-calories)
	  :do (setf max-elf elf
		    max-calories calories)
	:finally (return (values max-elf max-calories))))

(defun find-elves-carrying-most-calories (inventory-designator &optional (count 3))
  (let ((inventory
	  (input-inventory inventory-designator)))
    (setf inventory
	  (stable-sort inventory #'>= :key #'calories))
    (subseq inventory 0 count)))

(defun calories-carried-by-elves (elves)
  (reduce  #'+ elves :key #'calories :initial-value 0))

(defun puzzle-1 ()
  (calories
   (find-elf-carrying-most-calories #p"../input/day1.txt")))

(defun puzzle-2 ()
  (calories
   (find-elves-carrying-most-calories #p"../input/day1.txt")))
