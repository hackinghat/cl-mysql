;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Author: Steve Knight <stknig@gmail.com>
;;;;
(in-package "CL-MYSQL-TEST")

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

(deftest test-count-connections ()
  (let ((pool (connect :min-connections 1 :max-connections 1)))
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
  (let ((pool (connect :min-connections 1 :max-connections 3)))
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
	(is (handler-case (progn (query "USE mysql" :store nil) nil)
	      (cl-mysql-error (co) t)
	      (error (co) nil)))
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
  (let* ((pool (connect :min-connections 1 :max-connections 3))
	 (conn (query "USE mysql" :store nil)))
    (is (not (can-aquire pool)))
    (release conn)
    (is (can-aquire pool))))

(deftest test-contains ()
  (let* ((pool (connect :min-connections 1 :max-connections 1))
	 (conn (aquire pool nil)))
    (is (not (contains pool (available-connections pool) conn)))
    (is (contains pool (connections pool) conn))
    (release conn)
    (is (contains pool (available-connections pool) conn))
    (is (contains pool (connections pool) conn))))