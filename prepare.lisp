;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; 
;;;; Copyright (c) 2009 Steve Knight <stkni@gmail.com>
;;;; 
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package "CL-MYSQL-SYSTEM")

(defparameter *default-sequence-length* 1024
  "This is the maximum length that a sequence sent as a bound parameter can be
   It's a bit lame really.   How it should really work is that 'bind' gives you
   a binding and re-binds (if that's possible) when the buffer is too small. 

   In practice, though, I doubt it matters very much.")


(defclass statement ()
  ((pointer :reader pointer :initarg :pointer :initform (cffi:null-pointer))
   (database :reader database :initarg :database :initform nil)
   (nargs :accessor nargs :initarg :nargs :initform nil)
   (args :accessor args :initarg :args :initform (cffi:null-pointer))
   (bound-map :accessor bound-map :initarg :bound-map :initform nil)
   (fully-bound :accessor fully-bound :initform nil))
  (:documentation "A holder for a MYSQL_STMT structure"))

(defmethod bind-arg ((self statement) index)
  (if (> index (1- (nargs self)))
     (error 'cl-mysql-error 
        :message (format nil "Index: ~D is out of range on this statement." index)))
  (cffi:mem-aref (args self) 'mysql-bind index))

(defmethod configure-bindings ((self statement) nargs) 
  "Sets up a statement object ready to receive nargs bindings"
  (setf 
   (slot-value self 'nargs)
     nargs
   (slot-value self 'args) 
     (cffi:foreign-alloc 'mysql-bind :count nargs)
   (slot-value self 'bound-map)
     (make-array nargs :initial-element nil)))

(defun prepare (query &key (database *last-database*))
  "Prepare a query and return a statement object.  Use execute to access it"
  (with-connection (conn database)
    (let ((stmt (mysql-stmt-init (pointer conn))))
      (error-if-null conn stmt)
      (let ((stmt-object (make-instance 'statement 
			    :pointer stmt 
			    :database database)))
	(error-if-non-zero stmt-object 
	  (mysql-stmt-prepare stmt query (length query)))
	(configure-bindings stmt-object (param-count stmt-object))
	(values stmt-object)))))
  
(defmethod sqlstate ((self statement))
  "Returns the ANSI / ODBC SQL status"
  (mysql-stmt-sqlstate (pointer self)))

(defmethod param-count ((self statement))
  "The number of required parameters that must be bound to this statement."
  (mysql-stmt-param-count (pointer self)))

(defparameter *stmt-ctype-map* (make-hash-table))

(eval-when (:load-toplevel)
  (mapcar (lambda (map)
            (setf (gethash (first map) *stmt-ctype-map*) (second map)))
	  '((:TINY :char)
	    (:SMALLINT :int)
	    (:INT :long)
	    (:LONG :long)
	    (:BIGINT :longlong)
	    (:STRING :string)
	    (:FLOAT :float)
	    (:DOUBLE :double))))

(defmethod next-index ((self statement))
  "Returns the next unbound index or throws an error if there isn't one.   This
   is just a convenience method to allow bind to be called on a simple list of types:
      CL-USER> mapcar (lambda (x) (bind a-statement x)) (:LONG :STRING :FLOAT))"
  (loop for i from 0 to (nargs self)
		       for x across (bound-map self)
		       if (null x) do (return-from next-index i))
  (error 'cl-mysql-error :message "All the parameters on this query are bound"))

(defmethod release-binding ((self statement) index)
  "Deallocates the memory that we attached to this binding."
  (when (bound-parameter-p self index)
    (let ((arg (bind-arg self index)))
      (dolist (slot '(buffer is-null length error))
	(foreign-free (foreign-slot-value arg 'mysql-bind slot))))))
  
(defmethod close-statement ((self statement))
  "Close a statement and free all the allocated memory."
  (error-if-non-zero self (mysql-stmt-close (pointer self)))
  (dotimes (i (nargs self))
    (release-binding self i))
  (foreign-free (args self)))

(defun repeat-char (s n)
  (cond ((= n 0) nil)
	(t (concatenate 'string s (repeat-char s (1- n))))))

(defmethod bind ((self statement) sql-type &optional supplied-index (max-len *default-sequence-length*))
  "Set up the bind structure for later use"
  (let ((index (or supplied-index (next-index self))))
    (if (> index (1- (nargs self)))
	(error 'cl-mysql-error 
	       :message (format nil "Index: ~D is out of range on this statement." index)))
    ;; TODO: Later, when we are able to bind on the fly this should only release if the
    ;;       buffer type has changed.
    (release-binding self index)
    (let ((arg (bind-arg self index))
	  (c-type (gethash sql-type *stmt-ctype-map*)))
      (setf (foreign-slot-value arg 'mysql-bind 'buffer) 
	    (cond ((eq :string c-type)
		   (foreign-alloc :char :count max-len))
		  (t (foreign-alloc c-type))))

      (setf 
       (foreign-slot-value arg 'mysql-bind 'buffer-type) 
       (foreign-enum-value 'enum-field-types sql-type)

       (foreign-slot-value arg 'mysql-bind 'length)
       (foreign-alloc :int)

       (foreign-slot-value arg 'mysql-bind 'is-null)
       (foreign-alloc :char)

       (foreign-slot-value arg 'mysql-bind 'error)
       (foreign-alloc :char)
       
       ;; Mark this argument as bound
       (elt (bound-map self) index) t)
      ;; If all elements are now bound we assume we can dispatch
      ;; the arguments to the server
      (if (and (not (cffi:null-pointer-p (pointer self))) 
	       (notany #'null (bound-map self)))
	  (error-if-non-zero self 
			     (mysql-stmt-bind-param 
			      (pointer self) 
			      (args self)))))))

(defmethod bound-unbound-to-string ((self statement))
  "If the user didn't bind all the arguments bind those unbound ones now."
  (loop for i from 0 to (nargs self)
        for b across (bound-map self)
        do (if (not b) 
	      (bind self :string i))))

(defmethod bound-parameter-p ((self statement) index)
  "Returns T if the argument at index is bound."
  (elt (bound-map self) index))

(defmethod assign-bound-argument ((self statement) index value)
  "Take the supplied argument and try to bind it"
  
  (let* ((arg (bind-arg self index))
	 (buffer-type (foreign-enum-keyword 'enum-field-types
					    (foreign-slot-value arg 'mysql-bind 'buffer-type)))
	 (buffer-c-type (gethash buffer-type *stmt-ctype-map*))
 	 (type-adjusted-value (typecase value
 	 		        (string (format nil "~A" value))
			        (t value)))
	 (is-null (if value 0 1))
	 (length (typecase value 
		   (string (length type-adjusted-value))
		   (t 0))))
    (if (eq :string buffer-c-type)
	(lisp-string-to-foreign type-adjusted-value
				(foreign-slot-value arg 'mysql-bind 'buffer)
				*default-sequence-length*)
	(setf (mem-ref (foreign-slot-value arg 'mysql-bind 'buffer)
		       buffer-c-type) type-adjusted-value))

    (setf (mem-ref (foreign-slot-value arg 'mysql-bind 'is-null) :char) 
	    is-null
	  (mem-ref (foreign-slot-value arg 'mysql-bind 'length) :int) 
	    (cffi-utf8-length (foreign-slot-value arg 'mysql-bind 'buffer))
	  (foreign-slot-value arg 'mysql-bind 'buffer-length)
            (cffi-utf8-length (foreign-slot-value arg 'mysql-bind 'buffer)))))
    
(defmethod execute ((self statement) &rest args)
  (let ((nsupplied-args (length args))
	(nstatement-args (nargs self)))
    (if (not (eql nsupplied-args nstatement-args))
     (error 'cl-mysql-error 
        :message (format nil "You need to specify ~D arguments, not ~D." nstatement-args nsupplied-args)))
    ;; Lazily bind the remaining arguments to string
    (if (not (fully-bound self))
	(bound-unbound-to-string self))
    ;; Assign the supplied arguments to the statement
    (loop for i from 0 to nstatement-args
	  for arg in args
	  do (assign-bound-argument self i arg))
    (error-if-non-zero self
                     (mysql-stmt-execute (pointer self)))
    (mysql-stmt-affected-rows (pointer self))))