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
(defpackage com.hackinghat.cl-mysql-test
  (:nicknames "CL-MYSQL-TEST")
  (:use :cl :stefil :cl-mysql-system)
  (:export *host* *user* *password*))

(in-package "CL-MYSQL-TEST")

(in-root-suite)

(defparameter *host* "localhost")
(defparameter *user* nil)
(defparameter *password* nil)

(defsuite* test)

(defsuite* test-base)

(deftest test-string-to-integer ()
  (is (eq nil (string-to-integer "" 0)))
  (is (eq nil (string-to-integer nil 0)))
  (is (eql 12345678 (string-to-integer "12345678" 1)))
  (is (eql 12345678 (string-to-integer "12345678.0" 1)))
  (is (eql 123 (string-to-integer "123x" 1))))

(deftest test-string-to-float ()
  (is (eq nil (string-to-float "" 0)))
  (is (eq nil (string-to-float nil 0)))
  (is (eql -12345678.123d0 (string-to-float "-12345678.123" 1)))
  (is (eql 12345678.123d0 (string-to-float "12345678.123" 1)))
  (is (eql 2.356d0 (string-to-float "2.356" 1)))
  (is (eql 12345678d0 (string-to-float "12345678" 1)))
  (is (eql 1.23456789012345678d308 (string-to-float "1.23456789012345678e+308" 1)))
  (is (eql 1.234567890123457d-308 (string-to-float "1.23456789012345678e-308" 1))))

(deftest test-string-to-date ()
  (is (eq nil (string-to-date nil)))
  (is (eq nil (string-to-date "")))
  (multiple-value-bind (h m s day mon year)
      (decode-universal-time (string-to-date "2009-01-01"))
    (is (and (eql 0 h) (eql 0 m) (eql 0 s) (eql 1 day) (eql 1 mon) (eql 2009 year))))
  ;; Not sure how much it is worth investing in testing MySQL return values
  (is (eq nil (string-to-date "2009-1-1"))))

(deftest test-string-to-seconds ()
  (is (eq nil (string-to-seconds nil)))
  (is (eq nil (string-to-seconds "")))
  (is (eq -1  (string-to-seconds "-00:00:01")))
  (is (eql -3601 (string-to-seconds "-01:00:01")))
  (is (eql 3023999 (string-to-seconds "839:59:59")))
  (is (eql -3023999 (string-to-seconds "-839:59:59"))))

(deftest test-extract-field ()
  (cffi:with-foreign-object (ptr :pointer)
    (cffi:with-foreign-object (int :int)
      (setf (cffi:mem-ref int :int) (char-code #\1))
      (setf (cffi:mem-ref ptr :pointer) int)
      (is (string= "1" (extract-field ptr 0 1 *type-map* '("one" :VARCHAR 0))))
      (is (string= "1" (extract-field ptr 0 1 *type-map* '("one" :BLOB 16))))
      (is (equalp (make-array 1 :initial-element (char-code #\1))
		  (extract-field ptr 0 1 *type-map* '("bit" :BIT 0))))
      (is (equalp (make-array 1 :initial-element (char-code #\1))
		  (extract-field ptr 0 1 *type-map* '("bit" :BLOB 128))))
      (setf (cffi:mem-ref ptr :pointer) (cffi:null-pointer))
      (is (null (extract-field ptr 0 0 *type-map* '("space" :VARCHAR 0))))
      (is (null (extract-field ptr 0 0 *type-map* '("bit" :BIT 0)))))))

(deftest test-string-to-universal-time ()
  (is (eq nil (string-to-universal-time nil)))
  (is (eq nil (string-to-universal-time "")))
  (is (eq nil (string-to-universal-time "0000-00-00 00:00:00")))
  (is (eql 1
	  (- (string-to-universal-time "2009-01-01 00:00:00")
	     (string-to-universal-time "2008-12-31 23:59:59")))))


(deftest test-string-to-ratio  ()
  (is (eq nil (string-to-ratio nil 1)))
  (is (eq nil (string-to-ratio "" 0)))
  (is (equal (/ 123123123 100000000)
	     (string-to-ratio "1.23123123" 1)))
  (is (equal -1.23123123d0 
	     (coerce (string-to-ratio "-1.23123123" 1) 'double-float)))
  (is (eql 99999 (string-to-ratio "99999" 1)))
  (is (eql (/ 1 10) (string-to-ratio "0.1" 1))))

(deftest test-cffi-utf8-length ()
  (cffi:with-foreign-string (s "€")
    #-windows(is (eql 3 (cffi-utf8-length s)))
    #+windows(is (eql 7 (cffi-utf8-length s)))))


(defsuite* test-with-connection)

(deftest test-setup ()
  (connect :host *host* :user *user* :password *password*)
  (query "DROP DATABASE IF EXISTS cl_mysql_test; CREATE DATABASE cl_mysql_test; 
                  GRANT ALL ON cl_mysql_test.* TO USER(); FLUSH PRIVILEGES;")
  (use "cl_mysql_test")
  (query "CREATE TABLE test_table ( 
                -- Integer numerics
                bt BIT(6),
                ti TINYINT UNSIGNED,
                si SMALLINT SIGNED,
                mi MEDIUMINT ZEROFILL,
                i  INT(5) UNSIGNED,
                bi BIGINT UNSIGNED,
                -- Approximate numerics
                f  FLOAT(7,4),
                r  REAL(10,2),
                dp DOUBLE PRECISION(15,5),
                -- Precision numerics
                d  DECIMAL(28,18),
                n  NUMERIC(28,1),
                bg NUMERIC(65,0),
                -- Date and time
                dt DATETIME,
                da DATE,
                tm TIME,
                yr YEAR,
                ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                -- String types
                ch CHAR(10),
                vc VARCHAR(15),
                bn BINARY(7),
                vb VARBINARY(10),
                bb BLOB,
                tb TINYBLOB,
                mb MEDIUMBLOB,
                lb LONGBLOB,
                tt TINYTEXT,
                tx TEXT,
                mt MEDIUMTEXT,
                lt LONGTEXT,
                en ENUM ('small','medium','large'),
                st SET ('one','two'),
                -- Geomerty types
                ge GEOMETRY)")
  (is (eql 1 (length (list-tables))))
  (query "INSERT INTO test_table (bt, ti, si, mi, i, bi, f, r, dp, d, n, 
                                  dt, da, tm, yr, ch, vc, bn, vb, bb, tb,
                                  mb, lb, tt, tx, mt, lt, en, st, ge, bg) 
          VALUES (b'100000', 255, -32768, 1, 4294967295, 18446744073709551615,  999.9999, 12312312.12, SQRT(2.0), 1.0/9.0, 1.0/9.0,
                  '2009-12-31 00:00:00', '2009-12-31', '00:00:00', 2009, 'TEST1', 'TEST2', 'TEST3', 'TEST4', 'TEST5', 'TEST6', 
                  'TEST7', 'TEST8', 'TEST9', 'TEST10', 'TEST11', 'TEST12', 'small','one,two',GeomFromText('POINT(1 1)'),
                  12345678901234567890123456789012345678901234567890123456789012345)")
  ;; Now confirm that the decoding via the type-maps is as we expect ... this
  ;; pretty much completes our integration test at a broad level
  (let ((result (caaar (query "SELECT * FROM test_table"))))
    (is (equalp #(32) (first result)))
    (is (eql 255 (second result)))
    (is (eql -32768 (third result)))
    (is (eql 1 (fourth result)))
    (is (eql 4294967295 (fifth result)))
    (is (eql 18446744073709551615 (sixth result)))
    (is (eql 999.9999d0 (seventh result)))
    (is (eql 12312312.12d0 (eighth result)))
    (is (eql 1.41421d0 (ninth result)))
    (is (eql (/ 111111111111111111 1000000000000000000) (tenth result)))
    (is (eql (/ 1 10) (nth 10 result)))
    (is (eql 12345678901234567890123456789012345678901234567890123456789012345 (nth 11 result)))
    ;; note that these values are time zone specific, so we only test that their
    ;; accurate to +- 1/2 day
    (is (< (abs (- 3471192000 (nth 12 result))) 43200))
    (is (< (abs (- 3471192000 (nth 13 result))) 43200))
    (is (eql 0 (nth 14 result)))
    (is (eql 2009 (nth 15 result)))
    (is (>= (nth 16 result) 3447985347))
    (is (string= "TEST1" (nth 17 result)))
    (is (string= "TEST2" (nth 18 result)))
    (is (string= "TEST3" (nth 19 result)))
    (is (string= "TEST4" (nth 20 result)))
    (is (equalp #(84 69 83 84 53) (nth 21 result)))
    (is (equalp #(84 69 83 84 54) (nth 22 result)))
    (is (equalp #(84 69 83 84 55) (nth 23 result)))
    (is (equalp #(84 69 83 84 56) (nth 24 result)))
    (is (equalp "TEST9" (nth 25 result)))
    (is (equalp "TEST10" (nth 26 result)))
    (is (equalp "TEST11" (nth 27 result)))
    (is (equalp "TEST12" (nth 28 result)))
    (is (string= "small" (nth 29 result)))
    (is (string= "one,two" (nth 30 result)))
    (is (equalp #(0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 240 63 0 0 0 0 0 0 240 63)
		(nth 31 result))))
  (query "DROP DATABASE cl_mysql_test")
  (disconnect))
 
(deftest test-escape-string ()
  (connect :host *host* :user *user* :password *password*)
  (is (eq nil (escape-string nil)))
  (is (string= "" (escape-string "")))
  (is (string= "\\\"" (escape-string "\"")))
  (is (string= "\\\'" (escape-string "'")))
  (is (string= "\\n\\r" (escape-string (format nil "~C~C"
					    (code-char 10)
					    (code-char 13)))))
  (disconnect))

(deftest test-nth-row ()
  (is (equalp '(100) (nth-row '((((100)))) 0)))
  (is (null (nth-row nil 10 10))))

(deftest test-use-result-1 ()
  "Test out the self-service result set stuff.  It works but it's a bit tricky to build
   a working result set/row processing loop ..."
  (connect :host *host* :user *user* :password *password*)
  (query "DROP DATABASE IF EXISTS cl_mysql_test; CREATE DATABASE cl_mysql_test; 
                  GRANT ALL ON cl_mysql_test.* TO USER(); FLUSH PRIVILEGES;")
  (use "cl_mysql_test")
  (query "CREATE TABLE X ( X INT ); INSERT INTO X (X) VALUES (1); INSERT INTO X (X) VALUES (2)")
  (let ((conn (query "SELECT * FROM X; SELECT * FROM X" :store nil)))
    (let ((total-rows 0))
      (do ((result-set (next-result-set conn) (next-result-set conn)))
	  ((null result-set))
	(incf total-rows
	      (do ((row (next-row conn) (next-row conn))
		   (nrows 0 (incf nrows)))
		  ((null row) nrows))))
      (is (eql 4 total-rows))))
    ;; Now do it again using a loop style, the code is equivalent.   This just documents
    ;; the two idioms for processing result sets.
    (let ((conn (query "SELECT * FROM X; SELECT * FROM X" :store nil)))
      (unwind-protect 
	   (is (eql 4 (loop while (next-result-set conn)
			 summing (loop for row = (next-row conn)
				    until (null row)
				    count row))))))
    ;; Now do it once more to verify we haven't got any result sets left open ...
    (is (eql 2  (length (query "SELECT * FROM X; SELECT * FROM X" :store t))))
  (query "DROP DATABASE cl_mysql_test")
  (disconnect)
  (values))

(deftest test-use-result-2 ()
  "We should be able to use two result sets simultaneously."
  (connect :host *host* :user *user* :password *password*
           :min-connections 2 :max-connections 2)
  (query "DROP DATABASE IF EXISTS cl_mysql_test; CREATE DATABASE cl_mysql_test; 
                  GRANT ALL ON cl_mysql_test.* TO USER(); FLUSH PRIVILEGES;")
  (use "cl_mysql_test")
  (query "CREATE TABLE X ( X INT ); INSERT INTO X (X) VALUES (10)")
  (let ((a (query "USE cl_mysql_test; SELECT * FROM X" :store nil))
	(b (query "USE cl_mysql_test; SELECT * FROM X" :store nil)))
    (next-result-set a) (next-result-set a)
    (next-result-set b) (next-result-set b)
    (is (eql 100 (* (car (next-row a))
		    (car (next-row b)))))
    ;; Early release!
    (release a)
    (release b)
    (query "DROP DATABASE cl_mysql_test")
    (disconnect)))
