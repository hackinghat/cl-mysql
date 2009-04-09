;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Author: Steve Knight <stknig@gmail.com>
;;;;
(in-package "CL-MYSQL-SYSTEM")

(defparameter *last-database* nil
  "The last allocated connection-pool.   Note that this special is a default argument
   to a lot of the higher level API functions.")

(defparameter *debug* t)

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

(defmethod next-result-set ((self connection) &optional dont-release)
  "Retrieve the next result set.  Returns NIL when there are no more result sets."
  (let ((last-result (result-set self))
	(affected-rows 0))
    ;; Firstly free any prior results
    (unless (null-pointer-p last-result)
      (setf affected-rows (mysql-affected-rows (pointer self)))
      (mysql-free-result last-result))
    ;; Now check if this is not the first result whether there are
    ;; more results
    (if (and (> (length (result-set-fields self)) 0)
	     (not (eql 0 (mysql-next-result (pointer self)))))
	(progn (setf (result-set self) (null-pointer))
	       (setf (result-set-fields self) nil)
	       (setf (use-query-active self) nil)
	       (unless dont-release
		   (return-or-close (owner-pool self) self))
	       (return-from next-result-set
		 (values nil affected-rows))))
    ;; Now advance into the next result set.
    (let ((next-result (mysql-use-result (pointer self))))
      (error-if-null self next-result)
      (setf
       (result-set self) next-result
       (result-set-fields self) (append
				 (list (field-names-and-types next-result))
				 (result-set-fields self)))
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

(defun make-lock (name)
  #+sbcl (sb-thread:make-mutex :name name))

(defun make-wait-resource ()
  #+sbcl (sb-thread:make-waitqueue))

(defmacro with-lock (lock &body body)
  #+sbcl `(sb-thread:with-recursive-lock (,lock) ,@body)
  #-sbcl body)

(defmacro pool-wait (pool &body body)
  #+sbcl `(sb-thread:with-mutex ((wait-queue-lock ,pool))
	    (loop until (can-aquire ,pool)
	       do (sb-thread:condition-wait (wait-queue ,pool)
					    (wait-queue-lock ,pool)))
	    ,@body))

(defmacro pool-notify (pool &body body)
  #+sbcl `(sb-thread:with-mutex ((wait-queue-lock ,pool))
	    (sb-thread:condition-notify (wait-queue ,pool))))

(defclass connection-pool (connectable)
  ((hostname :type string :reader hostname :initarg :hostname :initform nil)
   (username :type string :reader username :initarg :username :initform nil)
   (password :type string :reader password :initarg :password :initform nil)
   (database :type string :reader database :initarg :database :initform nil)
   (port :type integer :reader port :initarg :port :initform 0)
   (socket :type string :reader socket :initarg :socket :initform nil)
   (flags :type integer :reader flags :initarg :flags :initform 0)
   (min-connections :type integer :accessor min-connections :initarg :min-connections :initform 1)
   (max-connections :type integer :accessor max-connections :initarg :max-connections :initform 1)
   (available-connections :type array :accessor available-connections :initform nil)
   (connections :type array :accessor connections :initform nil)
   ;; We need two locks per pool, one to keep the internal state of the pool
   ;; safe and another to allow us to block other threads from trying to aquire more
   ;; connections than the pool contains ...
   (pool-lock :accessor pool-lock :initform (make-lock "Pool Lock"))
   (wait-queue-lock :accessor wait-queue-lock :initform (make-lock "Queue Lock"))
   (wait-queue :accessor wait-queue :initform (make-wait-resource)))
  (:documentation "All connections are initiated through a pool. "))

(defmethod add-connection ((self connection-pool) (conn connection))
  (vector-push-extend conn (connections self))
  (vector-push-extend conn (available-connections self)))

(defmethod remove-connection-from-array ((self connection-pool) array conn)
  "Returns a new array with the given connection object removed (set to NIL)
   The pool should be locked before this method is called."
  (unless (null conn)
    (map-into array
	      (lambda (x)
		(if (connection-equal conn x)
		    nil
		    x))
	      array))
  (clean-connections self array))

(defmethod connect-to-server ((self connection-pool))
  "Create a new single connection and add it to the pool."
  (let* ((mysql (mysql-init (null-pointer)))
	 (connection (mysql-real-connect mysql
					 (or (hostname self) "localhost")
					 (or (username self) (null-pointer))
					 (or (password self) (null-pointer))
					 (or (database self) (null-pointer))
					 (or (port self) 0)
					 (or (socket self) (null-pointer))
					 (flags self))))
    (error-if-null mysql connection)
    (add-connection self (make-instance 'connection
					:pointer  connection
					:owner-pool self
					:in-use nil))))

(defmethod disconnect-from-server ((self connection)  conn)
  (disconnect-from-server (owner-pool self) (or conn self)))

(defmethod disconnect-from-server ((self connection-pool) (conn connection))
  "Internal method.   Pool should be locked before-hand. "
  (remove-connection-from-array self (available-connections self) conn)
  (remove-connection-from-array self (connections self) conn)
  (mysql-close (pointer conn)))


(defmethod count-connections ((self connection-pool))
  "Count the number of connections in the pool.   If you are dynamically changing
   the size of the pool after it is created this number could be greater or less than 
   max/min connections.   Set :available-only if you only want to know how many
   connections are currently ready to use."
  ;; Mutex
  (do ((i 0 (incf i))
       (available 0)
       (total 0))
      ((= i (fill-pointer (connections self))) (values total available))
    (when (available (elt (connections self) i))
      (incf available))
    (when (connected (elt (connections self) i))
      (incf total))))

(defmethod can-aquire ((self connection-pool))
  (multiple-value-bind (total available)
      (count-connections self)
    (declare (ignore total))
    (> available 0)))

(defmethod connect-upto-minimum ((self connection-pool) n min)
  "We use this method to allocate up to the minimum number of connections.
   It is called once after initialize-instance and will be called again every time 
   a connection is acquired from the pool."
  ;; Mutex
  (loop for i from 0 to (1- (- min n))
     do (connect-to-server self)))

(defmethod initialize-instance :after ((self connection-pool) &rest initargs)
  "The connection arrays need to be set-up after the pool is created."
  (declare (ignore initargs))
  (setf (connections self) (make-array (max-connections self)
				       :fill-pointer 0
				       :adjustable t))
  (setf (available-connections self) (make-array
				      (max-connections self)
				      :fill-pointer 0
				      :adjustable t))
  (connect-upto-minimum self  0 (min-connections self)))


(defmethod take-first ((self connection-pool))
  "Take the first available connection from the pool.   If there are none, NIL is returned."
  (pool-wait self
    (let ((first (loop for conn across (connections self)
		    if (and conn (available conn))
		    return conn)))
      (toggle first)
      (remove-connection-from-array self (available-connections self) first)
      (clean-connections self (available-connections self))
      (values first))))

(defmethod aquire ((self t) (block t))
  (error 'cl-mysql-error :message "There is no available pool to aquire from!"))

(defmethod aquire ((self connection-pool) block)
  "Aquire from the pool a single connection object that can be passed to higher level
   API functions like QUERY.   

  On implementations that support threading this method will block if :block is T,  and
  available connections is 0  and there are already max-connections .   On implementations 
  that do not support threading this method will always return NIL."
  ;; First we need to make sure that we aren't under-allocated.   This can happen if a previous
  ;; pool was disconnected or if the number of min connections has changed
  ;; Mutex
  (with-lock (pool-lock self)
    (multiple-value-bind (total available) (count-connections self)
      ;; After this call we should have enough available connections to take one, otherwise we will
      ;; need to block ...
      (connect-upto-minimum self total
			    (if (> available 0)
				(min-connections self)
				(min 
				 (max-connections self)
				 (1+ total))))
      (let ((candidate (take-first self)))
	(when (not  candidate)
	  (error 'cl-mysql-error :message "Can't allocate any more connections!"))
	(values candidate)))))

(defmethod aquire ((self connection) block)
  (declare (ignore block))
  (unless (in-use self)
    ;; Block on in-use?
    self))

(defmethod contains ((self connection-pool) array conn)
  (loop for c across array
       if (connection-equal c conn)
       return t))

(defmethod return-to-available ((self connection) &optional conn)
  (declare (ignore conn))
  ;; Deal with the pool
  (return-to-available (owner-pool self) self)
  ;; Now clean up any stateful data that could be hanging around
  (setf (result-set self) (null-pointer)
	(result-set-fields self) nil
	(in-use self) nil))
  
(defmethod return-to-available ((self connection-pool) &optional conn)
;  (inspect self)
  (if (or (not (in-use conn))
	  (contains self (available-connections self) conn))
      (error 'cl-mysql-error :message "Inconsistent state! Connection is not currently in use."))
  (vector-push-extend conn (available-connections self)))

(defmethod clean-connections ((self connection-pool) array)
  "Housekeeping to remove null connections from the end of the connections array.   Pool should be locked."
  (setf (fill-pointer array)
	(do ((i (1- (fill-pointer array)) (decf i)))
	    ((or (< i 0) (elt array i)) (1+ i)))))

(defmethod consume-unused-results ((self connection))
  "If a client attempts to release a connection without consuming all the results then we take care of that
   for them.  Because we are probably being called from release don't also auto-release when we reach the 
   last result!"
  (loop while (next-result-set self t)))

(defmethod return-or-close ((self connection-pool) (conn connection))
  "Given a pool and a connection, close it if there are more than min-connections or
   return it to the pool if we have less than or equal to min-connections"
  (with-lock (pool-lock self)
    (let ((total (count-connections self)))
      (if (> total (min-connections self))
	  (disconnect-from-server self conn)
	  (return-to-available conn))
      (clean-connections self (connections self))
      (clean-connections self (available-connections self)))
    (pool-notify self)))

(defmethod release ((self connection) &optional conn)
  "Convenience method to allow the release to be done with a connection"
  (release (owner-pool self) (or conn self)))

(defmethod release ((self connection-pool) &optional conn)
  "Release a connection back into the pool."
  (if (null conn)
      (error 'cl-mysql-error :message "Internal Error: Connection must be supplied when releasing a pool object!"))
  (if (use-query-active conn)
      (consume-unused-results conn))
  (return-or-close self conn)
  (values))

(defun connect (&key host user password database port socket
		(client-flag (list +client-compress+
				   +client-multi-statements+
				   +client-multi-results+))
		(min-connections 1) (max-connections 1))
  "Connect will present to MySQL sensible defaults for all the connection items.    
   The following code will attach you to a MySQL instance running on localhost, 
   as the current user with no password.   It will automatically turn on compression 
   between client-and-server and also enable multiple-result sets if possible.

   CL-USER> (connect)

   If unsuccesful connect will raise a MYSQL-ERROR, otherwise it will place the 
   connection into a pool, note that all connections are pool-able, a single connection
   is simple the special case of a pool with a single connection.  

   The pool will have min-connections and max-connections.   Should another 
   attempt be made to access the connection while it is in use the pool will
   either block (if your CL implementation has threads) or return NIL.  The last
   allocated pool object is placed into a special variable *last-database* which 
   is defaulted from many of the higher level API functions."
  (setf *last-database* (make-instance 'connection-pool
				       :hostname host
				       :username user
				       :password password
				       :database database
				       :port port
				       :socket socket
				       :flags (reduce #'logior client-flag)
				       :min-connections min-connections
				       :max-connections max-connections)))

(defun disconnect (&optional (database *last-database*))
  (disconnect-all database))

(defmethod disconnect-all ((self connection-pool))
  "Disconnects all the connections in the pool from the database."
  ;; Mutex
  (let ((array (subseq (connections self) 0)))
    (loop for conn across array
       do (disconnect-from-server self conn))))