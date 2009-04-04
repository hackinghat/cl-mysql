;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Author: Steve Knight <stknig@gmail.com>
;;;;
(defpackage com.hackinghat.cl-mysql-test
  (:nicknames "CL-MYSQL-TEST")
  (:use :cl :stefil :cl-mysql-system))

(in-package "CL-MYSQL-TEST")

(in-root-suite)

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
  (is (eql 1.23456789012345678d-308 (string-to-float "1.23456789012345678e-308" 1))))

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
      (is (equalp (make-array 1 :initial-element (char-code #\1))
		  (extract-field ptr 0 1 *type-map* '("bit" :BIT 0)))))
      (setf (cffi:mem-ref ptr :pointer) (cffi:null-pointer))
      (is (null (extract-field ptr 0 0 *type-map* '("space" :VARCHAR 0))))
      (is (null (extract-field ptr 0 0 *type-map* '("bit" :BIT 0))))))

(deftest test-string-to-universal-time ()
  (is (eq nil (string-to-universal-time nil)))
  (is (eq nil (string-to-universal-time "")))
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
    (is (eql 3 (cffi-utf8-length s)))))

(in-root-suite)

(defsuite* test-with-connection)

(deftest test-setup ()
  (connect)
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
                bn BINARY(3),
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
  (is (eq 2 (length (list-tables))))
  (query "INSERT INTO test_table (bt, ti, si, mi, i, bi, f, r, dp, d, n, 
                                  dt, da, tm, yr, ch, vc, bn, vb, bb, tb,
                                  mb, lb, tt, tx, mt, lt, en, st, ge, bg) 
          VALUES (b'100000', 255, -32768, 1, 4294967295, 18446744073709551615,  999.9999, 12312312.12, SQRT(2.0), 1.0/9.0, 1.0/9.0,
                  '2009-12-31 00:00:00', '2009-12-31', '00:00:00', 2009, 'TEST1', 'TEST2', 'TEST3', 'TEST4', 'TEST5', 'TEST6', 
                  'TEST7', 'TEST8', 'TEST9', 'TEST10', 'TEST11', 'TEST12', 'small','one,two',GeomFromText('POINT(1 1)'),
                  12345678901234567890123456789012345678901234567890123456789012345)")
  ;; Now confirm that the decoding via the type-maps is as we expect ... this
  ;; pretty much completes our integration test at a broad level
  (query "SELECT * FROM test_table")
  (query "DROP DATABASE cl_mysql_test")
  (disconnect))
 
(deftest test-escape-string ()
  (connect)
  (is (eq nil (escape-string nil)))
  (is (string= "" (escape-string "")))
  (is (string= "\\\"" (escape-string "\"")))
  (is (string= "\\\'" (escape-string "'")))
  (is (string= "\\n\\r" (escape-string (format nil "~C~C"
					    (code-char 10)
					    (code-char 13)))))
  (disconnect))