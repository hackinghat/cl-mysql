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
(in-package cl-mysql-test)

(defparameter *z* 1)
(defparameter *z-mutex* (cl-mysql-system:make-lock nil))

(defun reset-z ()
  (cl-mysql-system:with-lock *z-mutex*
    (setf *z* 0)))

(defun z ()
  (cl-mysql-system:with-lock *z-mutex*
    (incf *z*)))

(defun getz ()
  (cl-mysql-system:with-lock *z-mutex*
    *z*))

(defun setup-test-database (min max)
  (reset-z)
  (setf (min-connections *conn*) min)
  (setf (max-connections *conn*) max)
  (query "DROP DATABASE IF EXISTS cl_mysql_test; CREATE DATABASE cl_mysql_test; 
                  GRANT ALL ON cl_mysql_test.* TO USER(); FLUSH PRIVILEGES;"
	 :database *conn*)
  (use "cl_mysql_test" :database *conn*)
  (query "CREATE TABLE X ( X INT, T TIMESTAMP DEFAULT CURRENT_TIMESTAMP )" :database *conn*))

(defun long-test (n)
  "Loop from 1 to"
  (let ((last-t (get-universal-time)))
    (setup-test-database 1 2)
    (cl-mysql-system:wait-on-threads
     (loop for i from 1 to n
         collect (progn
                   ;(sleep  0.02)
                   (if (= 0 (mod (1+ (getz)) 1000))
                       (progn
                         (format t "Processed 1000 entries in: ~Ds" (-
                                                                     (get-universal-time)
                                                                     last-t))
                         (setf last-t (get-universal-time)))
                     (princ "."))
                   (if (= 0 (mod (getz) 80))
                       (format t "~%"))
                   
                   (start-thread-in-nsecs
                    (lambda ()
                      (query
                       (format nil  "USE cl_mysql_test; INSERT INTO X (X)  VALUES (~D)" (z))
                       :database *conn*)) 0 ;(random 1)
                    )))))
  (let ((result
	 (first (nth-row (query "SELECT AVG(X) FROM X" :database *conn*) 0))))
    (format t "~%Database average result = ~D ~[OK~;FAIL~]" result
	    (if (equalp (/ (+ n 1) 2) result) 0 1)))
  t)
