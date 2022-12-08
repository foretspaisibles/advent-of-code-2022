(ql:quickload "org.melusina.confidence")

(defpackage #:com.adventofcode/day6
  (:use #:common-lisp)
  (:import-from
   #:org.melusina.confidence
   #:define-testcase
   #:assert-eq)
  (:export
   #:puzzle-1
   #:puzzle-2))

(in-package #:com.adventofcode/day6)

(defparameter *input*
  #P"../input/day6.txt")

(defparameter *example*
  '(("mjqjpqmgbljsphdztnvjfqwrcgsmlb" .  7)
    ("bvwbjplbgvbhsrlpgdmjqwftvncz" . 5)
    ("nppdvjthqldpwncqszvftbrmjlhg" . 6)
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" . 10)
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" . 11)))

(defun find-start-of-packet (stream &optional (marker-size 4))
  (let ((buffer
	  (make-array marker-size :initial-element nil)))
    (declare (type simple-vector buffer))
    (labels
	((receive-char ()
	   (let ((c (read-char stream nil nil)))
	     (unless c
	       (return-from receive-char))
	     (setf
	      (subseq buffer 0 (1- marker-size))
	      (subseq buffer 1 marker-size))
	     (setf (aref buffer (1- marker-size)) c)))
	 (marker-received-p ()
	   (and (not (position nil buffer))
		(= marker-size
		   (loop :for c :across buffer
			 :sum (count c buffer))))))
      (loop :for c = (receive-char)
	    :for position = 1 :then (1+ position)
	    :while c
	    :when (marker-received-p)
	    :return position
	    :finally (return nil)))))

(define-testcase validate-find-start-of-packet (input expected)
  (with-input-from-string (stream input)
    (assert-eq expected (find-start-of-packet stream))))

(define-testcase testsuite ()
  (loop :for (input . expected) :in *example*
	:do (validate-find-start-of-packet input expected)))

(defun puzzle-1 ()
  (with-open-file (stream *input*)
    (find-start-of-packet stream)))

(defun puzzle-2 ()
  (with-open-file (stream *input*)
    (find-start-of-packet stream 14)))
