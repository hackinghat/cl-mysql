;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Author: Steve Knight <stknig@gmail.com>
;;;;
(defpackage com.hackinghat.cl-mysql-pool
  (:use :cl :cffi :cl-mysql-system)
  (:nicknames "CL-MYSQL-POOL")
  (:export #:connection-holder))

#+sbcl (use-package "SB-THREAD")

(in-package "CL-MYSQL-POOL")

(defvar *connection-pool* (make-hash-table))
(defvar *mutex*
  #+sbcl (sb-thread:make-mutex :name "Pool Lock")
  #-sbcl nil)

(defclass connection-holder ()
  ((hostname :type string :reader hostname)
   (username :type string :reader username)
   (password :type string :reader password)
   (database :type string :reader database)
   (socket :type string :reader socket)
   (client-flags :type integer :reader client-flags)
   (pointer :type t :initform (null-pointer) :accessor pointer)
   (in-use :type (or null t) :initform nil :accessor in-use)))

(defmacro with-pool-lock (&body body)
  (if *mutex*
	`(with-recursive-lock (*mutex*)
	   ,@body)
	`(progn ,@body)))

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

(defun add-connection (mysql)
  (with-pool-lock 
    (setf (gethash (pointer-address mysql) *connection-pool*) nil)))

(defun drop-connection (mysql)
  (with-pool-lock
    (remhash (pointer-address mysql) *connection-pool*)))

(defun free-connection-count ()
  (with-pool-lock
    (loop for i being the hash-keys of *connection-pool*
	 if (not (null (gethash i *connection-pool*)))
	 count i)))

(defun disconnect-all ()
  (with-pool-lock
    (loop for i being the hash-keys of *connection-pool*
       do (drop-connection (make-pointer i)))))

(defun release-connection (mysql)
  (with-pool-lock
    ;; If we already have enough minimum connections then disconnect this one
    (if (>= (free-connection-count) *min-connections*)
	(drop-connection (pointer-address mysql)))))