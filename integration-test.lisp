(in-package cl-mysql-test)

(defparameter *z* 1)
(defparameter *z-mutex* (sb-thread:make-mutex))

(defun reset-z ()
  (sb-thread:with-mutex (*z-mutex*)
    (setf *z* 0)))

(defun z ()
  (sb-thread:with-mutex (*z-mutex*)
    (incf *z*)))

(defun setup-test-database (min max)
  (reset-z)
  (setf (min-connections *conn*) min)
  (setf (max-connections *conn*) max)
  (query "DROP DATABASE IF EXISTS cl_mysql_test; CREATE DATABASE cl_mysql_test; 
                  GRANT ALL ON cl_mysql_test.* TO USER(); FLUSH PRIVILEGES;"
	 :database *conn*)
  (use "cl_mysql_test" :database *conn*)
  (query "CREATE TABLE X ( X INT, T TIMESTAMP DEFAULT CURRENT_TIMESTAMP )" :database *conn*))

(defun long-test ()
  "Loop from 1 to"
  (setup-test-database 1 2)
  (loop for i from 1 to 20
     do (progn
	  (sleep 0.25)
	  (generic-start-thread-in-nsecs 
	   (lambda ()
	     (query
	      (format nil  "USE cl_mysql_test; INSERT INTO X (X)  VALUES (~D)" (z))
	      :database *conn*)) (random 5))
	  (princ "."))))