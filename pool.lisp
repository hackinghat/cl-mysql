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

(defparameter *last-database* nil
  "The last allocated connection-pool.   Note that this special is a default 
   argument to a lot of the higher level API functions.")

(defparameter *debug* t)

(defclass connection-pool (connectable)
  ((hostname :reader hostname :initarg :hostname :initform nil)
   (username :reader username :initarg :username :initform nil)
   (password :reader password :initarg :password :initform nil)
   (database :reader database :initarg :database :initform nil)
   (port :reader port :initarg :port :initform 0)
   (socket :reader socket :initarg :socket :initform nil)
   (flags :reader flags :initarg :flags :initform 0)
   (min-connections :reader min-connections :initarg :min-connections :initform 1)
   (max-connections :reader max-connections :initarg :max-connections :initform 1)
   (available-connections :type (or array null) :accessor available-connections :initform nil)
   (connections :type (or array null) :accessor connections :initform nil)
   ;; We need two locks per pool, one to keep the internal state of the pool
   ;; safe and another to allow us to block other threads from trying to aquire more
   ;; connections than the pool contains ...
   (pool-lock :accessor pool-lock :initform (make-lock "Pool Lock"))
   (wait-queue-lock :accessor wait-queue-lock :initform (make-lock "Queue Lock"))
   (wait-queue :accessor wait-queue :initform (make-wait-resource)))
  (:documentation "All connections are initiated through a pool. "))

(defmethod (setf max-connections) ((max-connect number) (pool connection-pool))
  "Change the maximum number of connections available in the pool.   Note
   that you can change this value whilst the pool is waiting for a connection
   to become available."
  (with-lock (pool-lock pool)
    (setf (slot-value pool 'max-connections) max-connect))
  (pool-notify pool))

(defmethod (setf min-connections) ((min-connect number) (pool connection-pool))
  "Change the minimum number of connections available in the pool.   Note
   that you can change this value whilst the pool is waiting for a connection
   to become available."
  (with-lock (pool-lock pool)
    (setf (slot-value pool 'min-connections) min-connect))
  (pool-notify pool))

(defmethod add-connection ((self connection-pool) (conn connection))
  "Add a connection into the pool."
  (vector-push-extend conn (connections self))
  (vector-push-extend conn (available-connections self))
  (pool-notify self))

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
  "Count the number of connections in the pool.   If you are dynamically 
   changing the size of the pool after it is created this number could be 
   greater or less than  max/min connections.   Set :available-only if you 
   only want to know how many connections are currently ready to use."
  ;; Mutex
  (values
   (count-if #'identity (connections self))
   (count-if #'identity (available-connections self))))

(defmethod can-aquire ((self connection-pool))
  "Returns true if a call to aquire would result in a connection being allocated"
  (multiple-value-bind (total available)
      (count-connections self)
    (or (> available 0) (< total (max-connections self)))))

(defmethod can-aquire-lock ((self connection-pool))
  (with-lock (pool-lock self)
    (can-aquire self)))

(defmethod connect-upto-minimum ((self connection-pool) n min)
  "We use this method to allocate up to the minimum number of connections.
   It is called once after initialize-instance and will be called again every 
   time a connection is acquired from the pool."
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
  "Take the first available connection from the pool.   If there are none, 
   NIL is returned."
  (with-lock (pool-lock self)
    ;; If we can't aquire a connection return nil 
    (if (not (can-aquire self))
        (return-from take-first nil))
    
    ;; We can aquire but it might be because the max-number of connections
    ;; has changed so connect up to the minimum required to service this
    ;; request.
    (multiple-value-bind (total available) (count-connections self)
      (connect-upto-minimum self total
                            (if (> available 0)
                                (min-connections self)
                              (min 
                               (max-connections self)
                               (1+ total)))))
    ;; There now must be a connection available in the pool, so find the
    ;; first one and lock it.
    (let ((first (loop for conn across (connections self)
                     if (and conn (available conn))
                     return conn)))
      (toggle first)
      (remove-connection-from-array self (available-connections self) first)
      (clean-connections self (available-connections self))
      (values first))))

(defmethod aquire ((self t) (block t))
  (error 'cl-mysql-error :message "There is no available pool to aquire from!"))

(defmethod aquire ((self connection-pool) (block t))
  "Aquire from the pool a single connection object that can be passed to higher 
   level API functions like QUERY.   

   On implementations that support threading this method will block if :block 
   is T, and available connections is 0  and there are already max-connections.   
   On implementations that do not support threading this method will always 
   return NIL."
  (let ((candidate (take-first self)))
    (if (not  candidate)
        (if block
            (loop until candidate
                  do (progn
                    ;; The exact behaviour of pool-wait is implementation
                    ;; dependent.   Some implementations will sleep some
                    ;; will wait on a condition variable.
                    (pool-wait self)
                    (setf candidate (take-first self))))
            (error 'cl-mysql-error :message "Can't allocate any more connections!")))
    (values candidate)))

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
  "If the connection is not in the expected state raise an error."
  (if (or (not (in-use conn))
	  (contains self (available-connections self) conn))
      (error 'cl-mysql-error :message
	     "Inconsistent state! Connection is not currently in use."))
  (vector-push-extend conn (available-connections self)))

(defmethod clean-connections ((self connection-pool) array)
  "Housekeeping to remove null connections from the end of the connections 
   array.   Pool should be locked."
  (setf (fill-pointer array)
	(do ((i (1- (fill-pointer array)) (decf i)))
	    ((or (< i 0) (elt array i)) (1+ i)))))

(defmethod consume-unused-results ((self connection))
  "If a client attempts to release a connection without consuming all the 
   results then we take care of that for them.  Because we are probably 
   being called from release don't also auto-release when we reach the 
   last result!"
  (loop while (next-result-set self :dont-release t)))

(defmethod return-or-close ((self connection-pool) (conn connection))
  "Given a pool and a connection, close it if there are more than 
   min-connections or return it to the pool if we have less than or equal 
   to min-connections"

  ;; These don't strictly need to be locked because we make no guarantees
  ;; about the thread safety of a single connection
  (unless (null-pointer-p (result-set conn))
    (mysql-free-result (result-set conn)))
  (setf (slot-value conn 'result-set) (null-pointer))
  (setf (result-set-fields conn) nil)
  (setf (use-query-active conn) nil)
  (with-lock (pool-lock self)
    (let ((total (count-connections self)))
      (if (> total (min-connections self))
	  (disconnect-from-server self conn)
	  (return-to-available conn))
      (clean-connections self (connections self))
      (clean-connections self (available-connections self))))
  (pool-notify self))


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
   as the current user with no password.   It will automatically turn on 
   compression between client-and-server and also enable multiple-result sets 
   if possible.

   <pre><code>CL-USER> (connect)</code></pre>

   If unsuccesful connect will raise a <strong>CL-MYSQL-ERROR</strong>, otherwise it will place 
   the connection into a pool, note that all connections are pool-able, 
   a single connection is simply the special case of a pool with only one 
   connection.  

   The pool has two slots, <strong>min-connections</strong> and <srong>max-connections</strong>. There will 
   always be min-connections available in the pool.   If you are using all
   min-connections and max-connections is greater than min-connections,
   the pool will continue to allocate connections until max-connections are
   used.   After this an attempt to aquire more connections from the pool
   should block (if your implementation supports it).   When a connection is
   released (this is done automatically unless you explicity disable it) the 
   connection to the server is closed if we have allocated more connections
   than min-connections.

   The last  allocated pool object is placed into a special variable 
   <strong>*last-database*</strong> which is defaulted from the higher level API functions."
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
  "This method will disconnect all the connections in the pool.  Note
   that if you attempt to use the pool again after calling this method
   it will re-allocate upto min-connections before servicing your request."
  (disconnect-all database))

(defmethod disconnect-all ((self connection-pool))
  "Disconnects all the connections in the pool from the database."
  (let ((array (subseq (connections self) 0)))
    (loop for conn across array
       do (disconnect-from-server self conn))))
