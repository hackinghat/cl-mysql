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
;;;;
(in-package "CL-MYSQL-SYSTEM")

(defparameter *type-map* (make-hash-table))

(defclass connectable ()
  ()
  (:documentation "The base class of connectability.   CL-MYSQL functions operate on a 
   connectable which is then subclassed into a single connection and a connection pool.  
   Note that the connectable itself has no state."))

(defgeneric acquire (connectable &optional keyword)
  (:documentation "Calling aquire on a single connection returns itself, on a connection-pool it will return
   the first available connection.   Note that the pool implementation of this method could  block"))

(defgeneric release (connectable &optional other-args)
  (:documentation "Calling release will place the connection back into the pool.   If the pool has more
  connections than max-connections then releasing the connection will close it and deallocate it."))

(defclass connection (connectable)
  ((pointer :type t :initform (null-pointer) :accessor pointer :initarg :pointer)
   (result-set :type t :initform (null-pointer) :accessor result-set)
   (in-use :type (or null t) :initform nil :accessor in-use :initarg :in-use)
   (owner-pool :type t :reader owner-pool :initarg :owner-pool)
   (result-set-fields :type list :initform nil :accessor result-set-fields)
   (use-query-active :type (or null t) :initform nil :accessor use-query-active))
  (:documentation "The slots necessary to manage a MySQL database connection."))

(defmethod (setf result-set) ((result-set t) (conn connection))
  (setf (slot-value conn 'result-set) result-set)
  (set-field-names-and-types conn))

(defmethod process-result-set ((self connection) type-map)
  "Returns a CONS of all the data in the result set.   Note that this method
   should only be called by the client if you originally sent :store NIL to 
   query but then set :store to T when calling next-result-set."

  (declare (optimize (speed 3)))
  (cond ((null-pointer-p (result-set self))
	 (cons (mysql-affected-rows (pointer self)) nil))
	(t
	 (result-data self type-map))))

(defmethod set-field-names-and-types ((self connection))
  "Retrieve from a MYSQL_RES a list of cons ((<field name> <field type>)*) "
  (let ((mysql-res (result-set self)))
    (if (null-pointer-p mysql-res)
	(setf (result-set-fields self)
	      (append (list nil) (result-set-fields self)))
	(let* ((num-fields (1- (mysql-num-fields mysql-res)))
	       (fields (mysql-fetch-fields mysql-res))
	       (extracted-fields
		(loop for i from 0 to num-fields
		   collect (let ((mref (mysql-fetch-field mysql-res)))
			     (list
			      (foreign-slot-value mref 'mysql-field 'name)
			      (foreign-enum-keyword
			       'enum-field-types
			       (foreign-slot-value mref 'mysql-field 'type))
			      (foreign-slot-value mref 'mysql-field 'flags))))))
	  (setf (result-set-fields self)
		(append
		 (list extracted-fields)
		 (result-set-fields self)))))))

(defmethod result-data ((self connection) type-map)
  "Internal function that processes a result set and returns all the data.
   If field-names-and-types is NIL the raw (string) data is returned"
  (declare (optimize (speed 3))
	   (ftype (function (t t fixnum list (or t null)) list) process-row))
  (let* ((mysql-res (result-set self))
	 (num-fields (mysql-num-fields mysql-res)))
    (loop for row = (mysql-fetch-row mysql-res)
       until (null-pointer-p row)
       collect (process-row mysql-res
			    row
			    num-fields
			    (car (result-set-fields self))
			    type-map))))

(defmethod next-result-set ((self connection) &key dont-release store)
  "Position for the the next result set.  Returns T if there is a result
   set to process and NIL when the last result set has been passed.

   sets.  Use this method with (query \"SELECT\" :store NIL).   Call 
   next-result-set to position on the first result set then use next-row
   to fetch all the individual rows.   

   Use dont-release if you don't want cl-mysql to release all the resources
   when the last result set has been processed.   This might be useful, for
   instance, if you need access to the fields through result-set-fields after
   the result set has been processed.

   You can, if you wish elect to not process each individual row of a result
   set by setting :store T.   However, you cannot then use next-row because
   you must process the entire result set in one go.
 
   <pre><code>CL-USER> (query \"SELECT ...\" :store nil)
   CL-USER> (next-result-set *)
   T
   CL-USER> (next-row **)
   ...</code></pre>
"
  (let ((last-result (result-set self))
	(affected-rows 0))
    ;; Firstly free any prior results
    (unless (null-pointer-p last-result)
      (setf affected-rows (mysql-affected-rows (pointer self)))
      (mysql-free-result last-result)
      (setf (slot-value self 'result-set) (null-pointer)))
    ;; Now check if this is not the first result whether there are
    ;; more results
    (if (and (> (length (result-set-fields self)) 0)
	     (not (eql 0 (mysql-next-result (pointer self)))))
	(progn (unless dont-release
		 (return-or-close (owner-pool self) self))
	       (return-from next-result-set
		 (values nil affected-rows))))
    ;; Now advance into the next result set.
    (let ((result-set (if store
			   (mysql-store-result (pointer self))
			   (mysql-use-result (pointer self)))))
      (error-if-null-with-fields self result-set)
      (setf (result-set self) result-set)
      (values t affected-rows))))

(defmethod next-row ((self connection) &key (type-map *type-map*))
  "Retrieve and decode (according to the type map) the next row in the query.   This
   function will return NIL when the last row has been retrieved."
  (unless (null-pointer-p (result-set self))
    (let* ((fields-and-names (car (result-set-fields self)))
	   (row (mysql-fetch-row (result-set self))))
      (if (null-pointer-p row)
	  (error-if-set self)
	  (process-row (result-set self) row
		       (length fields-and-names)
		       fields-and-names type-map)))))

(defmethod connection-equal ((self t) (other t))
  nil)

(defmethod connection-equal ((self connection) (other connection))
  "Two connections are equal if they point to the same memory location."
  (and self other
       (pointer-eq (pointer self) (pointer other))))

(defmethod connected ((self connection))
  (not (null-pointer-p (pointer self))))

(defmethod available ((self connection))
  (and (connected self) (not (in-use self))))

(defmethod toggle ((self connection))
  (setf (in-use self) (not (in-use self))))

