;; $Id:

(defpackage com.hackinghat.cl-mysql-test
  (:nicknames "CL-MYSQL-TEST")
  (:use :cl :stefil :cl-mysql-system))

(in-package :cl-mysql-test)

(in-root-suite)

(defsuite* test)

(deftest test-string-to-fixnum ()
  (is (eq nil (string-to-fixnum "")))
  (is (eq nil (string-to-fixnum nil)))
  (is (eql 12345678 (string-to-fixnum "12345678")))
  (is (eql 12345678 (string-to-fixnum "12345678.0")))
  (is (eql 123 (string-to-fixnum "123x"))))

(deftest test-string-to-float ()
  (is (eq nil (string-to-float "")))
  (is (eq nil (string-to-float nil)))
  (is (eql 12345678.123d0 (string-to-float "12345678.123")))
  (is (eql 12345678d0 (string-to-float "12345678"))))

(deftest test-string-to-bit ()
  (is (eq 0 (string-to-bit "0")))
  (is (eq 1 (string-to-bit "1")))
  (is (eq nil (string-to-bit nil)))
  (is (eq nil (string-to-bit ""))))

(deftest test-string-to-date ()
  (is (eq nil (string-to-date nil)))
  (is (eq nil (string-to-date "")))
  (multiple-value-bind (h m s day mon year)
      (decode-universal-time (string-to-date "2009-01-01"))
    (is (and (eq 0 h) (eq 0 m) (eq 0 s) (eq 1 day) (eq 1 mon) (eq 2009 year))))
  ;; Not sure how much it is worth investing in testing MySQL return values
  (is (eq nil (string-to-date "2009-1-1"))))

(deftest test-string-to-seconds ()
  (is (eq nil (string-to-seconds nil)))
  (is (eq nil (string-to-seconds "")))
  (is (eq -1  (string-to-seconds "-00:00:01")))
  (is (eq -3601 (string-to-seconds "-01:00:01")))
  (is (eq 3023999 (string-to-seconds "839:59:59")))
  (is (eq -3023999 (string-to-seconds "-839:59:59"))))

