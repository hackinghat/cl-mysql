;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Author: Steve Knight <stknig@gmail.com>
;;;;
(in-package "CL-MYSQL-SYSTEM")

(defun connect (&key host user password database port socket
		(client-flag (list +client-compress+
				   +client-multi-statements+
				   +client-multi-results+)))
  "Connect will present to MySQL sensible defaults for all the connection items.    
   The following code will attach you to a MySQL instance running on localhost, 
   as the current user with no password.   It will automatically turn on compression 
   between client-and-server and also enable multiple-result sets if possible.

   CL-USER> (connect)

   If unsuccesful connect will raise a SIMPLE-ERROR, otherwise it will place the 
   connection into a stack.   This value will be used in all subsequent operations 
   where the :database key argument is omitted."
  (flet ((internal-connect (mysql host user password database port socket flags)
	   (error-if-null mysql
			  (mysql-real-connect mysql
					      host
					      user
					      password
					      database
					      port
					      socket
					      flags))
	   (cl-mysql-pool:add-connection mysql host user password database port socket flags)))
    (let* ((mysql (mysql-init (null-pointer))))
      (internal-connect mysql
			(or host "localhost")
			(or user (null-pointer))
			(or password (null-pointer))
			(or database (null-pointer))
			(or port 0)
			(or socket (null-pointer))
			(or (reduce #'logior (or client-flag '(0)))))
      ;; To ensure proper string decoding between CL & MySQL we better set the connection to be UTF-8 ...
      (error-if-non-zero mysql (set-character-set "UTF8" :database mysql))
      (values mysql))))

(defvar *connection-pool* (make-hash-table))
(eval-when (:compile-toplevel :load-toplevel)
  (defvar *mutex*
    #+sbcl (sb-thread:make-mutex :name "Pool Lock")
    #-sbcl nil))

(defclass connectable ()
  "The base class of connectability.   CL-MYSQL functions operate on a 
   connectable which attempts to provide the "
  ())

(defclass lock ()
  ((state :accessor state :initform
	  #+sbcl (sb-thread:make-mutex)
	  #-sbcl 0
	  )))

(defclass connectable ()
  ())

(defclass connection (connectable)
  ((hostname :type string :reader hostname :initarg :hostname :initform nil)
   (username :type string :reader username :initarg :username :initform nil)
   (password :type string :reader password :initarg :password :initform nil)
   (database :type string :reader database :initarg :database :initform nil)
   (port :type integer :reader port :initarg :port :initform 0)
   (socket :type string :reader socket :initarg :socket :initform nil)
   (flags :type integer :reader flags :initarg :flags :initform 0)
   (pointer :type t :initform (null-pointer) :accessor pointer :initarg :pointer)
   (result-set :type t :initform (null-pointer) :accessor result-set)
   (in-use :type (or null t) :initform nil :accessor in-use)))

(defclass connection-pool (connection)
  ((min-connections :type integer :accessor min-connections :initarg :min-connections :initform 1)
   (max-connections :type integer :accessor max-connections :initarg :max-connections :initform 0)
   (available-connections :type array :accessor available-connections :initform nil)
   (connections :type array :accessor connections :initform nil)))

(defmethod establish ((self connection))
  (let* ((mysql (mysql-init (null-pointer)))
	 (connection (mysql-real-connect mysql
					 (or (hostname self) "localhost")
					 (or (username self) (null-pointer))
					 (or (password self) (null-pointer))
					 (or (database self) (null-pointer))
					 (port self)
					 (or (socket self) (null-pointer))
					 (flags self))))
    (error-if-null mysql connection)
    (setf (pointer self) connection)))

(defmethod count-connections ((self connection-pool))
  (loop for conn across (connections self) unless (null-pointer-p conn) count conn))

(defmethod connect ((self connection-pool))
  ;; First allocate and establish min-connection objects
  (loop for i from 0 to (1- (- (min-connections self)
			       (fill-pointer (connections self))))
     do (let ((raw-pointer (establish self)))
	  (vector-push-extend raw-pointer (connections self))
	  (vector-push-extend raw-pointer (available-connections self)))))

(defmethod dispose ((self connection-pool) connection)
  (mysql-close connection)
  (flet ((clean-references (array connection)
	   (map-into array
		     (lambda (x)
		       (if (pointer-eq connection x)
			   (null-pointer)
			   x))
		     array)))
    (clean-references (connections self) connection)
    (clean-references (available-connections self) connection)))
  
  

(defmethod disconnect ((self connection-pool))
  "Disconnects all the members of the pool.   They will be reallocated back to the level of 
   min-connections if a client attempts to retrieve a connection from the pool."
  (setf (fill-pointer (available-connections self)) 0)
  (loop for conn across (connections self)
     do (unless (null-pointer-p conn)
	  (mysql-close conn)))
  (setf (fill-pointer (connections self)) 0))

(defmethod initialize-instance :after ((self connection-pool) &rest initargs)
  (declare (ignore initargs))
  (setf (connections self) (make-array (max-connections self) :fill-pointer 0 :adjustable t))
  (setf (available-connections self) (make-array (max-connections self) :fill-pointer 0 :adjustable t))
  (establish self))

(defmethod get-connection ((self connection))
  "Retrieve the CFFI pointer to the actual connection, establish a connection if none exists"
  (if (null-pointer-p (pointer self))
      (establish self)
      (pointer self)))

(defmethod clean-connections ((self connection-pool))
  "Housekeeping to remove null connections from the connections array"
  (flet ((clean-trailing-nulls (array)
	   (do ((i (1- (fill-pointer array)) (decf i)))
	       ((not (null-pointer-p (elt array i))))
	     (vector-pop array))))
    (clean-trailing-nulls (connections self))
    (clean-trailing-nulls (available-connections self))))

(defmethod get-connection ((self connection-pool))
  ())

(defmethod drop-connection ((self connection-pool))
  ()

(defmacro with-pool-lock (&body body)
  #+sbcl `(sb-thread:with-recursive-lock (*mutex*)
	   ,@body)
  #-sbcl `(progn ,@body))

(defparameter *min-connections* 0
  "The minimum number of allowed connections.   ")

(defparameter *min-connections* 1
  "The minimum number of allowed connections.   If a connection is released
   it will be returned to the pool only if the number of available connections 
   is less than min-connections.   Setting this value to zero will have the
   effect of releasing of releasing a connection after a query.")

(defparameter *max-connections* 128
  "The maximum number of allowed connections.   Any thread wishing to acquire
   a connection when max-connections is reached will have to wait.")

;;; Connections
;;;
(defvar *connection-pool* (make-hash-table))

(defun add-connection (mysql host user password database port socket flags)
  "Adds a connection to the pool"
  (with-pool-lock
    (setf (gethash (pointer-address mysql) *connection-pool*)
	  (make-instance 'connection-holder :hostname host :username user
			 :password password :database database :port port
			 :socket socket :flags flags :pointer mysql))))

(defun get-connection (&optional no-error)
  "Adds a connection to the pool"
  (with-pool-lock
    (loop for k being the hash-keys of *connection-pool*
       if (not (in-use (gethash k *connection-pool*)))
       do (return-from get-connection (pointer (gethash k *connection-pool*))))
    (unless no-error
      (error 'cl-mysql-error
	     :message "There are no connections available!"))))
  
(defun remove-connection (mysql)
  (with-pool-lock
    (remhash (pointer-address mysql) *connection-pool*)))


;(defun add-connection (mysql)
;  (with-pool-lock 
;    (setf (gethash (pointer-address mysql) *connection-pool*) nil)))

;(defun drop-connection (mysql)
;  (with-pool-lock
;    (remhash (pointer-address mysql) *connection-pool*)))

;(defun free-connection-count ()
;  (with-pool-lock
;    (loop for i being the hash-keys of *connection-pool*
;	 if (not (null (gethash i *connection-pool*)))
;	 count i)))

;(defun disconnect-all ()
;  (with-pool-lock
;    (loop for i being the hash-keys of *connection-pool*
;       do (drop-connection (make-pointer i)))))

;(defun release-connection (mysql)
;  (with-pool-lock
;    ;; If we already have enough minimum connections then disconnect this one
;    (if (>= (free-connection-count) *min-connections*)
;	(drop-connection (pointer-address mysql)))))