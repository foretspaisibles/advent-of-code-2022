(ql:quickload '("org.melusina.confidence" "cl-ppcre"))

(declaim (optimize (safety 3) (debug 3)))

(defpackage #:com.adventofcode/day8
  (:use #:common-lisp)
  (:import-from
   #:org.melusina.confidence
   #:define-testcase
   #:assert=)
  (:export
   #:puzzle-1
   #:puzzle-2))

(in-package #:com.adventofcode/day8)

(defparameter *input*
  #P"../input/day8.txt")

(defparameter *example*
  "30373
25512
65332
33549
35390")

(defun tree-height (digit)
  (declare (type character digit))
  (the
   (integer 0 9)
   (ecase digit
     (#\0 0)
     (#\1 1)
     (#\2 2)
     (#\3 3)
     (#\4 4)
     (#\5 5)
     (#\6 6)
     (#\7 7)
     (#\8 8)
     (#\9 9))))

(defun read-forest (stream)
  (let* ((lines
	   (loop :for line = (read-line stream nil nil)
		 :while line
		 :collect line))
	 (column-number
	   (length (first lines)))
	 (line-number
	   (length lines))
	 (forest
	   (make-array (list line-number column-number)
		       :element-type '(integer 0 9)
		       :initial-element 0)))
    (loop :for line :in lines
	  :for i = 0 :then (1+ i)
	  :do (dotimes (j column-number)
		(setf (aref forest i j)
		      (tree-height (aref line j)))))
    forest))

(defun count-visible-trees (forest)
  (let ((number-of-lines
	  (first (array-dimensions forest)))
	(number-of-columns
	  (second (array-dimensions forest)))
	(visibility
	  (make-array (array-dimensions forest)
		      :element-type 'bit
		      :initial-element 0)))
    (labels ((mark-visible-trees-in-line (i0 j0 delta-i delta-j)
	       (setf (aref visibility i0 j0) 1)
	       (loop :with max-height = 0
		     :for i = i0 :then (+ i delta-i)
		     :for j = j0 :then (+ j delta-j)
		     :while (is-in-forest-p i j)
		     :for current-height = (aref forest i j)
		     :when (> current-height max-height)
		     :do (progn (setf (aref visibility i j) 1)
				(setf max-height current-height))))
	     (is-in-forest-p (i j)
	       (and (<= 0 i)
		    (<= 0 j)
		    (< i number-of-lines)
		    (< j number-of-columns)))
	     (normal-i (delta-i delta-j)
	       (declare (ignore delta-i))
	       (- delta-j))
	     (normal-j (delta-i delta-j)
	       (declare (ignore delta-j))
	       delta-i)
	     (walkdown-line (i0 j0 delta-i delta-j)
	       (loop :with last-i = i0
		     :with last-j = j0
		     :for i = i0 :then (+ i delta-i)
		     :for j = j0 :then (+ j delta-j)
		     :while (is-in-forest-p i j)
		     :do (progn
			   (setf last-i i
				 last-j j)
			   (mark-visible-trees-in-line
			    i j (normal-i delta-i delta-j) (normal-j delta-i delta-j)))
		     :finally (return (values last-i last-j))))
	     (walkdown-forest ()
	       (let ((i0 0)
		     (j0 0)
		     (delta-i 1)
		     (delta-j 0))
		 (dotimes (whatever 4)
		   (multiple-value-bind (last-i last-j)
		       (walkdown-line i0 j0 delta-i delta-j)
		     (let ((last-delta-i delta-i)
			   (last-delta-j delta-j))
		       (setf i0 last-i
			     j0 last-j
			     delta-i (normal-i last-delta-i last-delta-j)
			     delta-j (normal-j last-delta-i last-delta-j))))))))
      (walkdown-forest)
      (loop :for i :from 0 :below number-of-lines
	    :sum (loop :for j :from 0 :below number-of-columns
		       :sum (aref visibility i j))))))

(defun find-highest-tree-scenic-score (forest)
  (let ((number-of-lines
	  (first (array-dimensions forest)))
	(number-of-columns
	  (second (array-dimensions forest))))
    (labels
	((is-in-forest-p (i j)
	   (and (<= 0 i)
		    (<= 0 j)
		    (< i number-of-lines)
		    (< j number-of-columns)))
	 (visible-trees-in-direction (i0 j0 delta-i delta-j)
	   (loop :with height = (aref forest i0 j0)
		 :for i = (+ i0 delta-i) :then (+ i delta-i)
		 :for j = (+ j0 delta-j) :then (+ j delta-j)
		 :while (is-in-forest-p i j)
		 :count t
		 :while (> height (aref forest i j))))
	 (scenic-score (i j)
	   (* (visible-trees-in-direction i j  0  1)
	      (visible-trees-in-direction i j  1  0)
	      (visible-trees-in-direction i j -1  0)
	      (visible-trees-in-direction i j  0 -1))))
      (loop :for i :from 0 :below number-of-lines
	    :maximize (loop :for j :from 0 :below number-of-columns
			    :maximize (scenic-score i j))))))

(defun example ()
  (with-input-from-string (stream *example*)
   (count-visible-trees (read-forest stream))))

(defun puzzle-1 ()
  (with-open-file (stream *input*)
    (count-visible-trees (read-forest stream))))

(defun puzzle-2 ()
  (with-open-file (stream *input*)
    (find-highest-tree-scenic-score (read-forest stream))))
