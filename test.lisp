;; $Id:

(defpackage :cl-mysql-test
  (:use :cl :stefil :cl-mysql-system))

(in-package :cl-mysql-test)

(in-root-suite)

(defsuite* test)

(deftest test-string-to-fixnum () 
    (is (eql 12345678 (string-to-fixnum "12345678"))))
