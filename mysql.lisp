(defpackage com.hackinghat.cl-mysql
  (:use :cl)
  (:nicknames "CL-MYSQL")
  (:export #:connect #:query #:use #:disconnect #:ping #:option
	   #:client-version #:server-version
	   #:list-dbs #:list-tables #:list-processes #:list-fields))

(in-package cl-mysql)

(defpackage com.hackinghat.cl-mysql-system
  (:use :cl :cffi)
  (:nicknames "CL-MYSQL-SYSTEM")
  (:export ;; Public functions
           #:connect #:query #:use #:disconnect #:ping #:option
	   #:client-version #:server-version
	   #:list-dbs #:list-tables #:list-processes #:list-fields
	   ;; Constants
	   ;; Internal functions
	   #:string-to-fixnum #:string-to-float #:string-to-bit
	   #:string-to-date #:string-to-seconds))

(in-package cl-mysql-system)

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
;; Field flags
(defconstant +field-not-null+ 1
  "Field can't be null") 
(defconstant +field-primary-key+ 2
  "Field is part of a primary key")
(defconstant +field-unique-key+ 4
  "Field is part of a unique key")
(defconstant +field-multiple-key+ 8
  "Field is part of a key")
(defconstant +field-blob+ 16
  "Field is a blob")
(defconstant +field-unsigned+ 32
  "Field is unsigned")
(defconstant +field-zerofill+ 64
  "Field is zerofill")
(defconstant +field-binary+ 128
  "Field is binary")
(defconstant +field-enum+ 256
  "Field is an enum")
(defconstant +field-auto-increment+ 512
  "Field is auto increment")
(defconstant +field-timestamp+ 1024
  "Field is a timestamp")
(defconstant +field-set+ 2048
  "Field is a set")
(defconstant +field-no-default+ 4096
  "Field doesn't have a default value")
(defconstant +field-num+ 32768
  "Field is num")
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

;; These special variables determine how the CFFI wrapping is done
(eval-when (:compile-toplevel)
  (defparameter *generate-alt-fns* t
    "Compile the library with this value equal to T to get the indirection 
   facility.   For more performance (because there is no wrapper around
   the CFFI wrapper!) set this value to NIL")
  (defparameter *mysql-dev-api-url* "http://dev.mysql.com/doc/refman/5.0/en/~A.html"
    "MySQL uses a standard page naming convention that matches our function names!"))

(defmacro defmysqlfun ((name internal-name) return-type &body args)
  "Takes a mysql function name as a string and registers the 
   appropriate CFFI function as internal-name.  

   If *generate-alt-fns* is T internal-name that denotes T a wrapper function
   that sits around the function lib<internal-name>.

   This function will call the lib<internal-name>, unless there is an 'alt-fn
   property set on the function's symbol.   If such a function exists it is called
   instead.

   e.g.  
   CL-USER> (connect)
   CL-USER> (setf (get 'mysql-close 'alt-fn) (lambda (db) 
                                               (print \"Closing! \") 
                                               (libmysql-close db))) 
   CL-USER> (disconnect)
   Closing!"
  (let* ((n name)
	 (int-name internal-name)
	 (int-libname (intern (string-upcase
			    (format nil "lib~A" int-name))))
	 (docstring (format nil "Library wrapper for MySQL function: ~A" name))
	 (mysql-doc-ref (format nil *mysql-dev-api-url* (string-downcase  int-name)))
	 (arg-names (mapcar #'car args)))
    (if *generate-alt-fns* 
	`(progn  (defcfun (,n ,int-libname) ,return-type
		   ,mysql-doc-ref
		   ,@args)
		 (defun ,int-name ,arg-names
		   ,docstring
		   (let ((alt-fn (get ',int-name 'alt-fn)))
		     (if alt-fn
			 (funcall alt-fn ,@arg-names)
			 (,int-libname ,@arg-names)))))
	`(defcfun (,n ,int-name) ,return-type
	   ,mysql-doc-ref
	   ,@args))))
  
(defmysqlfun ("mysql_init" mysql-init) :pointer
  (mysql :pointer))

(defmysqlfun ("mysql_close" mysql-close) :pointer
  (mysql :pointer))

(defmysqlfun ("mysql_error" mysql-error) :string
  (mysql :pointer))

(defmysqlfun ("mysql_errno" mysql-errno) :unsigned-int 
  (mysql :pointer))

(defmysqlfun ("mysql_real_connect" mysql-real-connect) :pointer
  (mysql :pointer)
  (host :string)
  (user :string)
  (password :string)
  (database :string)
  (port :int)
  (unix-socket :string)
  (client-flag :unsigned-long))

(defmysqlfun ("mysql_affected_rows" mysql-affected-rows) :unsigned-long-long
  (mysql :pointer))

(defmysqlfun ("mysql_character_set_name" mysql-character-set-name) :string
  (mysql :pointer))

(defmysqlfun ("mysql_ping" mysql-ping) :int
  (mysql :pointer))

(defmysqlfun ("mysql_query" mysql-query) :int
  (mysql :pointer)
  (statement :string))

(defmysqlfun ("mysql_field_count" mysql-field-count) :unsigned-int
  (mysql :pointer))

(defmysqlfun ("mysql_get_client_version" mysql-get-client-version) :unsigned-long)

(defmysqlfun ("mysql_get_server_version" mysql-get-server-version) :unsigned-long
  (mysql :pointer))

(defmysqlfun ("mysql_select_db" mysql-select-db) :int
  (mysql :pointer)
  (db :string))

(defmysqlfun ("mysql_store_result" mysql-store-result) :pointer
  (mysql :pointer))

(defmysqlfun ("mysql_num_rows" mysql-num-rows) :unsigned-long-long
  (mysql-res :pointer))

(defmysqlfun ("mysql_list_dbs" mysql-list-dbs) :pointer
  (mysql :pointer)
  (wild :string))

(defmysqlfun ("mysql_list_tables" mysql-list-tables) :pointer
  (mysql :pointer)
  (wild :string))

(defmysqlfun ("mysql_list_fields" mysql-list-fields) :pointer
  (mysql :pointer)
  (table :string)
  (wild :string))

(defmysqlfun ("mysql_list_processes" mysql-list-processes) :pointer
  (mysql :pointer))

(defmysqlfun ("mysql_fetch_row" mysql-fetch-row) :pointer
  (mysql-res :pointer))

(defmysqlfun ("mysql_free_result" mysql-free-result) :void
  (mysql-res :pointer))

(defmysqlfun ("mysql_fetch_lengths" mysql-fetch-lengths) :pointer
  (mysql-res :pointer))

(defmysqlfun ("mysql_num_fields" mysql-num-fields) :unsigned-int
  (mysql-res :pointer))

(defmysqlfun ("mysql_next_result" mysql-next-result) :int
  (mysql :pointer))

(defmysqlfun ("mysql_fetch_fields" mysql-fetch-fields) :pointer
  (mysql-res :pointer))

(defmysqlfun ("mysql_options" mysql-options) :int
  (mysql :pointer)
  (option :int)
  (arg :pointer))

(defcenum enum-field-types
  :decimal :tiny :short :long :float :double :null :timestamp :longlong
  :int24 :date :time :datetime :year :newdate :varchar :bit
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

(defcenum enum-options
  :opt-connect-timeout :opt-compress :opt-named-pipe
  :init-command :read-default-file :read-default-group
  :set-charset-dir :set-charset-name :opt-local-infile
  :opt-protocol :shared-memory-base-name :opt-read-timeout
  :opt-write-timeout :opt-use-result
  :opt-use-remote-connection :opt-use-embedded-connection
  :opt-guess-connection :set-client-ip :secure-auth
  :report-data-truncation :opt-reconnect
  :opt-ssl-verify-server-cert)

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

;;
;; Decoders
;;
(defun string-to-fixnum (string)
  (when (and string (> (length string) 0)) 
    (parse-integer string :junk-allowed t)))

(defun string-to-float (string)
  (when (and string (> (length string) 0))
    (let* ((radix-position (position #\. string))
	   (int-part (coerce (parse-integer string :start 0 :end radix-position) 'long-float)))
      (cond ((null radix-position) int-part)
	    (t (let* ((float-part (coerce (parse-integer string :start (1+ radix-position)) 'long-float))
		     (float-log-10 (log float-part 10))
		     (float-size (ceiling float-log-10)))
	       (+ int-part (expt 10 (- float-log-10 float-size)))))))))

(defun string-to-bit (string)
  (when (and string (> (length string) 0))
    (let ((result (string-to-fixnum string)))
      (if (and result (> (abs result) 1))
	  (error (format nil "Bit value ~A is not 0/1 or NULL!" result))
	  (values result)))))

(defun string-to-date (string)
  (when (and string (> (length string) 9))
    (let ((y (parse-integer string :start 0 :end 4))
	  (m (parse-integer string :start 5 :end 7))
	  (d (parse-integer string :start 8 :end 10)))
      (encode-universal-time 0 0 0 d m y))))

(defun string-to-seconds (string)
  "Fairly ugly function to turn MySQL TIME duration into an integer representation. 
   It's complicated because of ... well, read this:  http://dev.mysql.com/doc/refman/5.0/en/time.html"
  (when string
    (let* ((strlen (length string))
	   (offset (- strlen 8)))
      (when (and (>= offset 0) (< offset 3)) 
	(let* ((start (if (eq #\- (elt string 0)) 1 0))
	       (h (parse-integer string :start start :end (+ 2 offset)))
	       (m (parse-integer string :start (+ 3 offset) :end (+ 5 offset)))
	       (s (parse-integer string :start (+ 6 offset) :end (+ 8 offset)))
	       (time (+ (* h 3600) (* m 60) s)))
	  (if (eq start 1)
	      (values (* -1 time))
	      (values time)))))))

(defun string-to-universal-time (string)
  (when string
    (+ (string-to-date (subseq string 0 10))
       (string-to-seconds (subseq string 11)))))

(defparameter *type-map*
  `((:DECIMAL . ,#'string-to-float)
    (:TINY . ,#'string-to-fixnum)
    (:SHORT . ,#'string-to-fixnum)
    (:LONG . ,#'string-to-fixnum)
    (:FLOAT . ,#'string-to-float)
    (:DOUBLE . ,#'string-to-float)
    (:NULL . ,(lambda (string)
		      (declare (ignore string))
		      (values nil)))
    (:TIMESTAMP . ,#'string-to-universal-time)
    (:LONGLONG . ,#'string-to-fixnum)
    (:INT24 . ,#'string-to-fixnum)
    (:DATE . ,#'string-to-universal-time)
    (:TIME . ,#'string-to-seconds)
    (:DATETIME . ,#'string-to-universal-time)
    (:YEAR . ,#'string-to-fixnum)
    (:NEWDATE . ,#'string-to-universal-time)
    (:BIT . ,#'string-to-bool)
    (:NEWDECIMAL . ,#'string-to-float)))

;;;
;;; Error handling
;;;
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

;;;
;;; Connections
;;;
(defparameter *connection-stack* nil)

(defun add-connection (mysql)
  "Adds a connection to the pool"
  (push mysql *connection-stack*))

(defun get-connection ()
  "Adds a connection to the pool"
  (unless *connection-stack*
    (error "There are no connections established!"))
  (car *connection-stack*))

(defun drop-connection ()
  (pop *connection-stack*))

(defmacro with-connection ((var &optional database) &body body)
  `(let ((,var (or ,database (get-connection))))
     ,@body))

;;;
;;; High level API
;;;
(defun use (name &key database)
  "Equivalent to \"USE <name>\""
  (with-connection (conn database)
    (error-if-non-zero conn (mysql-select-db conn name))
    (values)))

(defun decode-version (int-version)
  "Return a list of version details <major> <release> <version>"
  (let* ((version (mod int-version 100))
	 (major-version (floor int-version 10000))
	 (release-level (mod (floor int-version 100) 10)))
    (values (list major-version release-level version))))

(defun client-version ()
  (decode-version (mysql-get-client-version)))

(defun server-version (&key database)
  (with-connection (conn database)
    (decode-version (mysql-get-server-version conn))))

(defun list-dbs (database)
  (with-connection (conn database)
    (let ((result (error-if-null conn (mysql-list-dbs conn (null-pointer)))))
      (process-result-set result :database conn))))

(defun list-tables (&key database)
  (with-connection (conn database)
    (let ((result (error-if-null conn (mysql-list-tables conn (null-pointer)))))
      (process-result-set result :database conn))))

(defun list-fields (table &key database)
  (with-connection (conn database)
    (let ((result (error-if-null conn (mysql-list-fields conn table (null-pointer)))))
      (process-result-set result :database conn))))

(defun list-processes (&key database)
  (with-connection (conn database)
    (let ((result (error-if-null conn (mysql-list-processes conn))))
      (process-result-set result :database conn))))

;;
;; Result set functions
;;
(defun field-names-and-types (mysql-res)
  "Retrieve from a MYSQL_RES a list of cons ((<field name> <field type>)*) "
  (let ((num-fields (1- (mysql-num-fields mysql-res)))
	(fields (mysql-fetch-fields mysql-res)))
    (loop for i from 0 to num-fields
       collect (let ((mref (mem-aref fields 'mysql-field i)))
		 (list
		  (foreign-slot-value mref 'mysql-field 'name)
		  (foreign-enum-keyword
		   'enum-field-types
		   (foreign-slot-value mref 'mysql-field 'type))
		  (foreign-slot-value mref 'mysql-field 'flags))))))

(defun result-data (mysql-res field-names-and-types)
  "Internal function that processes a result set and returns all the data.
   If field-names-and-types is NIL the raw (string) data is returned"
  (let ((num-fields (1- (mysql-num-fields mysql-res))))
    (loop for row = (mysql-fetch-row mysql-res)
       until (null-pointer-p row)
       collect (if field-names-and-types
		   (loop
		     for i from 0 to num-fields
		     for f in field-names-and-types
		     collect
			(let ((fn (cdr (assoc (second f) *type-map*))))
			   (funcall (or fn #'identity) (mem-aref row :string i))))
		   (loop
		      for i from 0 to num-fields
		      collect (mem-aref row :string i))))))

(defun process-result-set (mysql-res &key raw database)
  "Result set will be NULL if the command did not return any results.   In this case we return a cons
   the rows affected."
  (cond ((null-pointer-p mysql-res)
	 (cons (mysql-affected-rows (or database (get-connection))) nil))
	(t
	 (let ((fields (field-names-and-types mysql-res)))
	   (cons
	    (mapcar #'car fields)
	    (result-data mysql-res (unless raw fields)))))))

(defun query (query &key raw database)
  "Queries the connection.  Set raw to T if you don't want CL-MYSQL to decode
   the return data (this will leave them as strings)"
  (with-connection (conn database)
    (error-if-non-zero conn (mysql-query conn query))
    (loop for result-set = (mysql-store-result conn)
       collect (process-result-set result-set :database conn :raw raw)
       until (progn
	       (mysql-free-result result-set)
	       (not (eq 0 (mysql-next-result conn)))))))

(defun ping (&key database)
  (with-connection (conn database)
    (error-if-non-zero conn (mysql-ping conn))
    (values t)))

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
    (add-connection mysql)
    (values mysql)))

(defun disconnect (&key database)
  (with-connection (conn database)
    (mysql-close conn)))

(defun %set-string-option (option value &key database)
  (let ((retval 0))
    (with-connection (conn database)
      (with-foreign-pointer-as-string (str 255)
	(setq retval (mysql-options conn
				    (foreign-enum-value 'enum-options option)
				    (lisp-string-to-foreign value str 255)))))
    (values retval)))

(defun %set-int-option (option value &key database)
  (let ((retval 0))
    (with-connection (conn database)
      (with-foreign-object (int-value :int)
	(setf (mem-ref int-value :int) value)
	(setq retval (mysql-options conn
				    (foreign-enum-value 'enum-options option)
				    int-value))))
    (values retval)))

(defun option (option value &key database)
  (typecase value
    (string (%set-string-option option value :database database))
    (null   (%set-int-option option 0 :database database))
    (t      (%set-int-option option value :database database))))
