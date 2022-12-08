(ql:quickload '("org.melusina.confidence" "cl-ppcre"))

(defpackage #:com.adventofcode/day7
  (:use #:common-lisp)
  (:import-from
   #:org.melusina.confidence
   #:define-testcase
   #:assert-eq)
  (:export
   #:puzzle-1
   #:puzzle-2))

(in-package #:com.adventofcode/day7)

(defparameter *input*
  #P"../input/day7.txt")

(defparameter *example*
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defparameter *command-cd-back-scanner*
  (ppcre:create-scanner "^[$] cd [.][.]$"))

(defparameter *command-cd-scanner*
  (ppcre:create-scanner "^[$] cd (.*)$"))

(defparameter *command-ls-scanner*
  (ppcre:create-scanner "^[$] ls$"))

(defparameter *command-ls-dir-scanner*
  (ppcre:create-scanner "^dir (.*)$"))

(defparameter *command-ls-file-scanner*
  (ppcre:create-scanner "^([0-9]+) (.*)$"))

(defun read-terminal-output (stream)
  (labels ((cd-back (line)
	     (ppcre:register-groups-bind ()
		 (*command-cd-back-scanner* line)
	       (list :cd-back)))
	   (cd (line)
	     (ppcre:register-groups-bind (dir)
		 (*command-cd-scanner* line)
	       (list :cd dir)))
	   (ls (line)
	     (ppcre:register-groups-bind ()
		 (*command-ls-scanner* line)
	       (list :ls)))
	   (ls-dir (line)
	     (ppcre:register-groups-bind (name)
		 (*command-ls-dir-scanner* line)
	       (list :dir name)))
	   (ls-file (line)
	     (ppcre:register-groups-bind ((#'parse-integer size) name)
		 (*command-ls-file-scanner* line)
	       (list :file name size))))
    (let ((line
	    (read-line stream nil nil)))
      (unless line
	(return-from read-terminal-output))
      (or (cd-back line)
	  (cd line)
	  (ls line)
	  (ls-dir line)
	  (ls-file line)))))


(defclass node nil
  ((name
    :initarg :name
    :initform (error "A filesystem NODE must have a name.")
    :documentation "The name of a filesystem NODE.")    
   (kind
    :initarg :kind
    :initform (error "A filesystem NODE must have a kind.")
    :documentation "The kind of a file is either :REGULAR or :DIRECTORY.")
   (size
    :initarg :size
    :initform nil
    :documentation "The size of a file.")))

(defclass file (node)
  ((kind
    :initform :regular
    :allocation :class
    :documentation "The kind of a file is either :REGULAR or :DIRECTORY.")))

(defmethod print-object ((instance file) stream)
  (print-unreadable-object (instance stream :type t)
    (with-slots (name size) instance
      (format stream "~S" name)
      (format stream " (~A)" size))))

(defmethod describe-object ((instance file) stream)
  (with-slots (name kind size) instance
    (format stream "~&Name: ~A" name)
    (format stream "~&Kind: ~A" kind)
    (format stream "~&Size: ~A" size))
  (values))

(defun make-file (&rest initargs &key name size)
  "Make a file."
  (declare (ignore name size))
  (apply #'make-instance 'file initargs))

(defclass dir (node)
  ((kind
    :initform :directory
    :allocation :class
    :documentation "The kind of a directory is either :REGULAR or :DIRECTORY.")
   (contents
    :initarg :contents
    :initform nil
    :documentation "The list of files contained by the DIRECTORY.")))

(defmethod print-object ((instance dir) stream)
  (print-unreadable-object (instance stream :type t)
    (with-slots (name size) instance
      (format stream "~S" name)
      (when size
	(format stream " (~A)" size)))))

(defmethod describe-object ((instance dir) stream)
  (with-slots (name kind size contents) instance
    (format stream "~&Name: ~A" name)
    (format stream "~&Kind: ~A" kind)
    (format stream "~&Size: ~A" size)
    (format stream "~&Contents: ~A" (length contents)))
  (values))

(defun make-dir (&rest initargs &key name size contents)
  "Make a directory."
  (declare (ignore name size contents))
  (apply #'make-instance 'dir initargs))

(defun make-filesystem-tree (stream)
  (labels ((next ()
	     (read-terminal-output stream))
	   (lookup (name current-dir)
	     (or
	      (find-if (lambda (node)
			 (and (eq :directory (slot-value node 'kind))
			      (string= name (slot-value node 'name))))
		       (slot-value current-dir 'contents))
	      (error "Current directory ~A has no subdirectory ~A" current-dir name)))
	   (update-total-size (current-dir)
	     (with-slots (size contents) current-dir
	       (setf size
		     (loop :for node :in contents
			   :sum (slot-value node 'size))))
	     current-dir)
	   (read-root-directory ()
	     (unless (equal '(:cd "/") (next))
	       (error "Expect navigating to the root."))
	     (read-ls-command (make-directory :name "/")))
	   (read-ls-command (current-dir)
	     (unless (equal '(:ls) (next))
	       (error "Expect ls command."))
	     (read-ls-output current-dir))
	   (read-ls-output (current-dir)
	     (loop :for next = (next)
		   :while next
		   :do (ecase (first next)
			 (:cd
			  (read-ls-command (lookup (second next) current-dir)))
			 (:ls
			  (read-ls-output (lookup (second next) current-dir)))
			 (:dir
			  (push (make-dir :name (second next))
				(slot-value current-dir 'contents)))
			 (:file
			  (push (make-file :name (second next) :size (third next))
				(slot-value current-dir 'contents)))
			 (:cd-back
			  (return (update-total-size current-dir))))
		   :finally
		       (return (update-total-size current-dir)))))
    (read-root-directory)))

(defun total-size-of-small-directories (node &optional (maximum-size 100000))
  (labels ((compute-1 (node)
	     (unless (eq :directory (slot-value node 'kind))
	       (return-from compute-1 0))
	     (with-slots (size contents) node
	       (+ (if (<= size maximum-size) size 0)
		  (compute contents))))
	   (compute (nodes)
	     (loop :for node :in nodes
		   :sum (compute-1 node))))
    (compute-1 node)))

(defun directory-freeing-enough-space (node &key (total-space 70000000) (required-space 30000000))
  (let* ((used-space
	   (slot-value node 'size))
	 (available-space
	   (- total-space used-space))
	 (minimal-size-to-delete
	   (max 0 (- required-space available-space))))		
    (labels ((delete-would-free-enough-space-p (node)
	       (with-slots (kind size) node
		 (and (eq kind :directory)
		      (>= size minimal-size-to-delete))))
	     (pick-smaller (node-1 node-2)
	       (cond
		 ((not (delete-would-free-enough-space-p node-2))
		  node-1)
		 ((< (slot-value node-2 'size) (slot-value node-1 'size))
		  node-2)
		 (t
		  node-1)))
	     (find-smaller (node)
	       (unless (eq :directory (slot-value node 'kind))
		 (return-from find-smaller node))
	       (reduce #'pick-smaller (slot-value node 'contents)
		       :key #'find-smaller
		       :initial-value node)))
      (find-smaller node))))

  
(defun example ()
  (with-input-from-string (stream *example*)
   (total-size-of-small-directories (make-filesystem-tree stream))))

(defun puzzle-1 ()
  (with-open-file (stream *input*)
    (total-size-of-small-directories
     (make-filesystem-tree stream))))

(defun puzzle-2 ()
  (with-open-file (stream *input*)
    (slot-value
     (directory-freeing-enough-space
      (make-filesystem-tree stream))
     'size)))
