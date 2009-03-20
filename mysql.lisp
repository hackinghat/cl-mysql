(defpackage com.hackinghat.cl-mysql
  (:use :cffi :cl)
  (:nicknames "CL-MYSQL"))

(in-package cl-mysql)

(define-foreign-library libmysqlclient
  (t (:default "libmysqlclient_r")))

(use-foreign-library libmysqlclient)

;;
;; Client options
(defconstant +client-compress+ 32)
(defconstant +client-found-rows+ 2)
(defconstant +client-ignore-sigpipe+ 4096)
(defconstant +client-ignore-space+ 256)
(defconstant +client-interactive+ 1024)
(defconstant +client-local-files+ 128)
(defconstant +client-multi-statements+  (ash 1 16))
(defconstant +client-multi-results+ (ash 1 17))
(defconstant +client-no-schema+ 16)
(defconstant +client-ssl+ (ash 1 11))
(defconstant +client-remember-options+ (ash 1 31))
;;
;;
;; Error codes
;;
(defconstant +commands-out-of-sync+ 2014)
(defconstant +error-first+ 2000)
(defconstant +unknown-error+ 2000)
(defconstant +socket-create-error+ 2001)
(defconstant +connection-error+ 2002)
(defconstant +conn-host-error+ 2003)
(defconstant +ipsock-error+ 2004)
(defconstant +unknown-host+ 2005)
(defconstant +server-gone-error+ 2006)
(defconstant +version-error+ 2007)
(defconstant +out-of-memory+ 2008)
(defconstant +wrong-host-info+ 2009)
(defconstant +localhost-connection+ 2010)
(defconstant +tcp-connection+ 2011)
(defconstant +server-handshake-err+ 2012)
(defconstant +server-lost+ 2013)
(defconstant +commands-out-of-sync+ 2014)
(defconstant +namedpipe-connection+ 2015)
(defconstant +namedpipewait-error+ 2016)
(defconstant +namedpipeopen-error+ 2017)
(defconstant +namedpipesetstate-error+ 2018)
(defconstant +cant-read-charset+ 2019)
(defconstant +net-packet-too-large+ 2020)
(defconstant +embedded-connection+ 2021)
(defconstant +probe-slave-status+ 2022)
(defconstant +probe-slave-hosts+ 2023)
(defconstant +probe-slave-connect+ 2024)
(defconstant +probe-master-connect+ 2025)
(defconstant +ssl-connection-error+ 2026)
(defconstant +malformed-packet+ 2027)
(defconstant +wrong-license+ 2028)
(defconstant +null-pointer+ 2029)
(defconstant +no-prepare-stmt+ 2030)
(defconstant +params-not-bound+ 2031)
(defconstant +data-truncated+ 2032)
(defconstant +no-parameters-exists+ 2033)
(defconstant +invalid-parameter-no+ 2034)
(defconstant +invalid-buffer-use+ 2035)
(defconstant +unsupported-param-type+ 2036)
(defconstant +shared-memory-connection+ 2037)
(defconstant +shared-memory-connect-request-error+ 2038)
(defconstant +shared-memory-connect-answer-error+ 2039)
(defconstant +shared-memory-connect-file-map-error+ 2040)
(defconstant +shared-memory-connect-map-error+ 2041)
(defconstant +shared-memory-file-map-error+ 2042)
(defconstant +shared-memory-map-error+ 2043)
(defconstant +shared-memory-event-error+ 2044)
(defconstant +shared-memory-connect-abandoned-error+ 2045)
(defconstant +shared-memory-connect-set-error+ 2046)
(defconstant +conn-unknow-protocol+ 2047)
(defconstant +invalid-conn-handle+ 2048)
(defconstant +secure-auth+ 2049)
(defconstant +fetch-canceled+ 2050)
(defconstant +no-data+ 2051)
(defconstant +no-stmt-metadata+ 2052)
(defconstant +no-result-set+ 2053)
(defconstant +not-implemented+ 2054)
(defconstant +server-lost-extended+ 2055)

(defcfun ("mysql_init" mysql-init) :pointer
  (mysql :pointer))

(defcfun ("mysql_close" mysql-close) :pointer
  "http://dev.mysql.com/doc/refman/5.0/en/mysql-close.html"
  (mysql :pointer))

(defcfun ("mysql_error" mysql-error) :string
  (mysql :pointer))

(defcfun ("mysql_errno" mysql-errno) :unsigned-int 
  (mysql :pointer))

(defcfun ("mysql_real_connect" mysql-real-connect) :pointer
  (mysql :pointer)
  (host :string)
  (user :string)
  (password :string)
  (database :string)
  (port :int)
  (unix-socket :string)
  (client-flag :unsigned-long))

(defcfun ("mysql_affected_rows" mysql-affected-rows) :unsigned-long-long
  "http://dev.mysql.com/doc/refman/5.0/en/mysql-affected-rows.html"
  (mysql :pointer))

(defcfun ("mysql_character_set_name" mysql-character-set-name) :string
  "http://dev.mysql.com/doc/refman/5.0/en/mysql-character-set-name.html"
  (mysql :pointer))

(defcfun ("mysql_ping" mysql-ping) :int
  "http://dev.mysql.com/doc/refman/5.0/en/mysql-ping.html"
  (mysql :pointer))

(defcfun ("mysql_query" mysql-query) :int
  (mysql :pointer)
  (statement :string))

(defcfun ("mysql_field_count" mysql-field-count) :unsigned-int
  (mysql :pointer))

(defcfun ("mysql_get_client_version" mysql-get-client-version) :unsigned-long
  "http://dev.mysql.com/doc/refman/5.0/en/mysql-get-client-version.html")

(defcfun ("mysql_get_server_version" mysql-get-server-version) :unsigned-long
  "http://dev.mysql.com/doc/refman/5.0/en/mysql-get-client-version.html"
  (mysql :pointer))

(defcfun ("mysql_select_db" mysql-select-db) :int
  (mysql :pointer)
  (db :string))

(defcfun ("mysql_store_result" mysql-store-result) :pointer
  (mysql :pointer))

(defcfun ("mysql_num_rows" mysql-num-rows) :unsigned-long-long
  (mysql-res :pointer))

(defcfun ("mysql_list_dbs" mysql-list-dbs) :pointer
  "http://dev.mysql.com/doc/refman/5.0/en/mysql-list-dbs.html"
  (mysql :pointer)
  (wild :string))

(defcfun ("mysql_list_tables" mysql-list-tables) :pointer
  "http://dev.mysql.com/doc/refman/5.0/en/mysql-list-dbs.html"
  (mysql :pointer)
  (wild :string))

(defcfun ("mysql_list_fields" mysql-list-fields) :pointer
  "http://dev.mysql.com/doc/refman/5.0/en/mysql-list-fields.html"
  (mysql :pointer)
  (table :string)
  (wild :string))

(defcfun ("mysql_list_processes" mysql-list-processes) :pointer
  "http://dev.mysql.com/doc/refman/5.0/en/mysql-list-processes.html"
  (mysql :pointer))

(defcfun ("mysql_fetch_row" mysql-fetch-row) :pointer
  (mysql-res :pointer))

(defcfun ("mysql_free_result" mysql-free-result) :void
  (mysql-res :pointer))

(defcfun ("mysql_fetch_lengths" mysql-fetch-lengths) :pointer
  (mysql-res :pointer))

(defcfun ("mysql_num_fields" mysql-num-fields) :unsigned-int
  (mysql-res :pointer))

(defcfun ("mysql_next_result" mysql-next-result) :int
  (mysql :pointer))

(defcenum enum-field-types
  :decimal :tiny :short :long :float :double :null :timestamp :longlong
  :date :time :datetime :year :newdate :varchar :bit
  (:newdecimal 246)
  (:enum 247)
  (:set 248)
  (:tiny-blob 249)
  (:medium-blob 250)
  (:long-blob 251)
  (:blob 252)
  (:var-string 253)
  (:string 254)
  (:geometry 255))

(defcstruct mysql-field
  (name :string)
  (org-name :string)
  (table :string)
  (org-table :string)
  (db :string)
  (catalog :string)
  (def :string)
  (length :unsigned-long)
  (max-length :unsigned-long)
  (name-length :unsigned-int)
  (org-name-length :unsigned-int)
  (table-length :unsigned-int)
  (org-table-length :unsigned-int)
  (db-length :unsigned-int)
  (catalog-length :unsigned-int)
  (def-length :unsigned-int)
  (flags :unsigned-int)
  (decimals :unsigned-int)
  (charsetnr :unsigned-int)
  (type :int))

(defcfun ("mysql_fetch_fields" mysql-fetch-fields) :pointer
  (mysql-res :pointer))

(defun field-names-and-types (mysql-res)
  "Retrieve from a MYSQL_RES a list of cons ((<field name> <field type>)*) "
  (let ((num-fields (1- (mysql-num-fields mysql-res)))
	(fields (mysql-fetch-fields mysql-res)))
    (loop for i from 0 to num-fields
       collect (let ((mref (mem-aref fields 'mysql-field i)))
		 (cons
		  (foreign-slot-value mref 'mysql-field 'name)
		  (foreign-enum-keyword
		   'enum-field-types
		   (foreign-slot-value mref 'mysql-field 'type)))))))

(defun result-data-raw (mysql-res)
  "Internal function that processes a result set and returns all the data"
  (let ((num-fields (1- (mysql-num-fields mysql-res))))
    (loop for row = (mysql-fetch-row mysql-res)
       until (null-pointer-p row)
       collect (loop for i from 0 to num-fields
		  collect (mem-aref row :string i)))))

(defun process-result-set (database mysql-res)
  "Result set will be NULL if the command did not return any results.   In this case we return a cons
   the rows affected."
  (cond ((null-pointer-p mysql-res)
	 (cons (mysql-affected-rows database) nil))
	(t
	 (cons
	  (field-names-and-types mysql-res)
	  (result-data-raw mysql-res)))))

(defmacro error-if-non-zero (database &body command)
  `(let ((return-value ,@command))
     (if (not (eq 0 return-value))
	 (error (format nil "MySQL error: \"~A\" (errno = ~D)."
			(mysql-error ,database)
			(mysql-errno ,database)))
	 return-value)))

(defmacro error-if-null (database &body command)
  `(let ((return-value ,@command))
     (if (null-pointer-p return-value)
	 (error (format nil "MySQL error: \"~A\" (errno = ~D)."
			(mysql-error ,database)
			(mysql-errno ,database)))
	 return-value)))


(defun use (database name)
  "Equivalent to \"USE <name>\""
  (error-if-non-zero database (mysql-select-db database name)))

(defun query (database query &optional callback)
  (error-if-non-zero database (mysql-query database query))
  (loop for result-set = (mysql-store-result database)
       collect (process-result-set database result-set)
       until (progn
	       (mysql-free-result result-set)
	       (not (eq 0 (mysql-next-result database))))))

(defun ping (database)
  (error-if-non-zero database (mysql-ping database))
  (values t))

(defun connect (&key host user password database port socket (client-flag (list +client-compress+
										+client-multi-statements+
										+client-multi-results+)))
  "By default we turn on CLIENT_COMPRESS, CLIENT_MULTI_STATEMENTS and CLIENT_MULTI_RESULTS."
  (let* ((mysql (mysql-init (null-pointer))))
    (mysql-real-connect mysql
			(or host "localhost")
			(or user (null-pointer))
			(or password (null-pointer))
			(or database (null-pointer))
			(or port 0)
			(or socket (null-pointer))
			(or (reduce #'logior (or client-flag '(0)))))
    (values mysql)))

(defun disconnect (database)
  (mysql-close database))

(defun decode-version (int-version)
  "Return a list of version details <major> <release> <version>"
  (let* ((version (mod int-version 100))
	 (major-version (floor int-version 10000))
	 (release-level (mod (floor int-version 100) 10)))
    (values (list major-version release-level version))))

(defun client-version ()
  (decode-version (mysql-get-client-version)))

(defun server-version (database)
  (decode-version (mysql-get-server-version database)))

(defun list-dbs (database)
  (let ((result (error-if-null database (mysql-list-dbs database (null-pointer)))))
    (process-result-set database result)))

(defun list-tables (database)
  (let ((result (error-if-null database (mysql-list-tables database (null-pointer)))))
    (process-result-set database result)))

(defun list-fields (database table)
  (let ((result (error-if-null database (mysql-list-fields database table (null-pointer)))))
    (process-result-set database result)))

(defun list-processes (database)
  (let ((result (error-if-null database (mysql-list-processes database))))
    (process-result-set database result)))

;(defparameter *m* (mysql-init (null-pointer)))
;(mysql-real-connect *m* "localhost" "stkni" (null-pointer) "dpbsdk" 0 (null-pointer) 0)
;(mysql-query *m* "SELECT COUNT(*) FROM price")
;(defparameter *r* (mysql-store-result *m*))
;(mysql-num-fields *r*)
;(defparameter *f* (mysql-fetch-fields *r*))
;(foreign-slot-value *f* 'mysql-field 'name)
;(foreign-enum-keyword 'enum-field-types (foreign-slot-value *f* 'mysql-field 'type))
;(defparameter *row* (mysql-fetch-row *r*))
;(mem-ref *row* :string)
;(mysql-free-result *f*)
;(mysql-next-result *r*)
;