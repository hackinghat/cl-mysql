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
(in-package "CL-MYSQL-TEST")

(defvar *conn* nil)

(defsuite* test-pool)

(deftest test-connection ()
  (let ((test-conn (make-instance 'connection :pointer (cffi:make-pointer 1) :in-use t)))
          ;; Test an in-use connection
	  (is (not (available test-conn)))
	  (is (connected test-conn))
	  ;; Now make it available
	  (setf (in-use test-conn) nil)
	  (is  (available test-conn))
	  (is (connected test-conn))
	  ;; Now 'disconnect' the connection 
	  (setf (pointer test-conn) (cffi:null-pointer))
	  (is (not (connected test-conn)))
	  (is (not (available test-conn)))
	  ;; Finally test an invalid connection does something sensible
	  (setf (in-use test-conn) t)
	  (is (not (connected test-conn)))
	  (is (not (available test-conn)))))

(deftest test-connection-equal ()
  (is (not (connection-equal
	    (make-instance 'connection :pointer (cffi:null-pointer))
	    nil)))
  (is (not (connection-equal
	    nil
	    (make-instance 'connection :pointer (cffi:null-pointer)))))
  (is (connection-equal
       (make-instance 'connection :pointer (cffi:null-pointer))
       (make-instance 'connection :pointer (cffi:null-pointer))))
  (is (not (connection-equal
	    (make-instance 'connection :pointer (cffi:make-pointer 1))
	    (make-instance 'connection :pointer (cffi:null-pointer))))))

(deftest test-aquire-connection ()
  (is (null (aquire (make-instance 'connection :in-use t) nil)))
  (is (aquire (make-instance 'connection :in-use nil) nil))
  (is (handler-case (progn (aquire nil nil) nil)
	(cl-mysql-error (c) t)
	(error (c) nil))))

(deftest test-count-connections-directly ()
  "There will, from time-to-time be NILs in the arrays so we better make sure
   that we can handle them and they don't interfere with the count"
  (let ((pool (connect :host *host* :user *user* :password *password*
                       :min-connections 1 :max-connections 1)))
    (setf (available-connections pool)
	  (make-array 3 :fill-pointer t :initial-contents (list NIL (aref (available-connections pool) 0) NIL)))
    (setf (connections pool)
	  (make-array 3 :fill-pointer t :initial-contents (list NIL (aref (connections pool) 0) NIL)))
    (is (eql 1 (count-connections pool)))
    ;; Now set the number of available to the empty vector, this simulates us
    ;; not having any available connections.
    (setf (available-connections pool)
	  (make-array 0))
    (is (eql 1 (count-connections pool)))))

(deftest test-count-connections ()
  (let ((pool (connect :host *host* :user *user* :password *password*
                       :min-connections 1 :max-connections 1)))
    (multiple-value-bind (total available) (count-connections pool)
      (is (eql 1 total))
      (is (eql 1 available)))
    (let ((c (aquire pool nil)))
      (multiple-value-bind (total available) (count-connections pool)
	(is (eql 1 total))
	(is (eql 0 available)))
      (release c)
      (multiple-value-bind (total available) (count-connections pool)
	(is (eql 1 total))
	(is (eql 1 available))))))

(deftest test-pool-expand-contract ()
  (let ((pool (connect :host *host* :user *user* :password *password*
                       :min-connections 1 :max-connections 3)))
    (multiple-value-bind (total available) (count-connections pool)
      (is (eql 1 total))
      (is (eql 1 available)))
    (let ((a (query "USE mysql" :store nil))
	  (b (query "USE mysql" :store nil)))
      (multiple-value-bind (total available) (count-connections pool)
	(is (eql 2 total))
	(is (eql 0 available)))
      (let ((c (query "USE mysql" :store nil)))
	(multiple-value-bind (total available) (count-connections pool)
	  (is (eql 3 total))
	  (is (eql 0 available)))
	;; We need to resurrect this test when allow non-blocking waits ..
	;(is (handler-case (progn (query "USE mysql" :store nil) nil)
	;      (cl-mysql-error (co) t)
	;      (error (co) nil)))
	(release c))
      (multiple-value-bind (total available) (count-connections pool)
	(is (eql 2 total))
	(is (eql 0 available)))
      (release b)
      (release a)
      (multiple-value-bind (total available) (count-connections pool)
	(is (eql 1 total))
	(is (eql 1 available))))))

(deftest test-can-aquire ()
  (let* ((pool (connect :host *host* :user *user* :password *password*
                        :min-connections 1 :max-connections 1))
	 (conn (query "USE mysql" :store nil)))
    (is (not (can-aquire pool)))
    (release conn)
    (is (can-aquire pool))))

(deftest test-contains ()
  (let* ((pool (connect :host *host* :user *user* :password *password*
                        :min-connections 1 :max-connections 1))
	 (conn (aquire pool nil)))
    (is (not (contains pool (available-connections pool) conn)))
    (is (contains pool (connections pool) conn))
    (release conn)
    (is (contains pool (available-connections pool) conn))
    (is (contains pool (connections pool) conn))))



#+thread-support
(deftest test-thread-1 ()
  "Testing threading is always a bit suspect but we can test a little bit of it to
   make sure we have the general idea correct."
  (let ((pool (connect :host *host* :user *user* :password *password*
                       :min-connections 1 :max-connections 1))
        (conn (query "SELECT 1" :store nil)))
    ;; Now we have a pool of 1 connection that is allocated, so start a thread
    ;; that will in 2 seconds increas the pool size to 2.
    (start-thread-in-nsecs (lambda ()
                             (setf (max-connections pool) 2)) 1)
    ;; This line will block until the thread above changes the max pool size
    (list-processes :database pool)
    (release conn)
    ;; Because we increased the max connection count we should have closed
    ;; the extra connection we opened so we should be back to the initial state.
    (multiple-value-bind (total available)
	(count-connections pool)
      (is (eql 1 total))
      (is (eql 1 available)))))

#+thread-support
(deftest test-thread-2 ()
  "This is the reverse of test-thread-1 in as much as we have a long running 
   query and our main thread waits until the long running query is completed.
   We use a closure to set a flag to indicate success."
  (let ((pool (connect :host *host* :user *user* :password *password*
                       :min-connections 1 :max-connections 1))
        (result nil))
    ;; Now we have a pool of 1 connection that is allocated, so start a thread
    ;; that will in 2 seconds increas the pool size to 2.
    (start-thread-in-nsecs (lambda ()
                             (let ((conn (query "SELECT 1" :store nil :database pool)))
                               (sleep 1)
                               (setf result 1)
                               (release conn))) 0 )
    (sleep 0.5)
    ;; This line will block until the thread above completes
    (list-processes :database pool)
    (is (eql 1 result))
    ;; Because we increased the max connection count we should have closed
    ;; the extra connection we opened so we should be back to the initial state.
    (multiple-value-bind (total available)
	(count-connections pool)
      (is (eql 1 total))
      (is (eql 1 available)))))

#+thread-support
(deftest test-thread-3 ()
  "The killer thread test.   Start 100 threads to run in the next 1-3 seconds
   that will insert the numbers  from 1 to 100 into a table.   Join the threads
   and then run a query  to verify that all was well.   This should demonstrate
   whether we have a problem with locking or not."
  (setf *conn* (or *conn* (connect :host *host* :user *user* :password *password*
                        :min-connections 1 :max-connections 1)))
  (query "DROP DATABASE IF EXISTS cl_mysql_test; CREATE DATABASE cl_mysql_test; 
                  GRANT ALL ON cl_mysql_test.* TO USER(); FLUSH PRIVILEGES;" :database *conn*)
  (use "cl_mysql_test" :database *conn*)
  (query "CREATE TABLE X ( X INT )" :database *conn*)
  (let ((threads (loop for i from 1 to 50
                     collect (start-thread-in-nsecs
                              (lambda ()
                                (query
                                 (format nil "USE cl_mysql_test; INSERT INTO X VALUES (~D)" i)
                                 :database *conn*))
                              (1+ (random 2))))))
    (cl-mysql-system:wait-on-threads threads)
    (is (eql 50 (caaaar (query "SELECT COUNT(*) FROM X" :database *conn*))))))
