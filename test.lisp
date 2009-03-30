;; $Id:

(defpackage com.hackinghat.cl-mysql-test
  (:nicknames "CL-MYSQL-TEST")
  (:use :cl :stefil :cl-mysql-system))

(in-package :cl-mysql-test)

(in-root-suite)

(defsuite* test)

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
  (is (eql 1.23456789012345678d308 (string-to-float "1.23456789012345678d308e+308" 1))))

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

(deftest test-string-to-universal-time ()
  (is (eq nil (string-to-universal-time nil)))
  (is (eq nil (string-to-universal-time "")))
  (is (eq 1
	  (- (string-to-universal-time "2009-01-01 00:00:00")
	     (string-to-universal-time "2008-12-31 23:59:59")))))


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
  ;; We aren't so much worried about the results as that they are decoded ...
  (query "SELECT * FROM test_table")
  (query "DROP DATABASE cl_mysql_test")
  (disconnect))
 

