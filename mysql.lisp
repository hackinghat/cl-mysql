;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Author: Steve Knight <stknig@gmail.com>
;;;;

;;; Decoders
;;;
(in-package "CL-MYSQL-SYSTEM")

(defun string-to-integer (string &optional len)
  (declare (optimize (speed 3) (safety 3))
	   (type (or null simple-string) string)
	   (type (or null (integer 0 128)) len))
  (when (and string (> (or len (length string)) 0))
    (parse-integer string :junk-allowed t)))

(defun string-to-float (string len)
  "Convert a MySQL float representation into a double.   Note that we could do better on DECIMAL/NUMERICs
   that have 0 places after the decimal."
  (declare (optimize (speed 3) (safety 3))
	   (type fixnum len)
	   (type (or null simple-string) string))
  (when (and string (> len 0))
    (let ((sign 1)
	  (integer-part 0)
	  (decimal-part 0)
	  (mantissa-part 0)
	  (decimal-length 1)
	  (mantissa-sign 1)
	  (passed-decimal nil)
	  (passed-mantissa nil))
      (declare (type integer integer-part decimal-part)
	       (type (integer 0 310) mantissa-part)
	       (type (integer -1 1) mantissa-sign sign)
	       (type (or null t) passed-decimal passed-mantissa))
      (loop for c across string
	 do (cond ((char= c #\+)
		   (if passed-mantissa
		       (setf mantissa-sign 1)
		       (setf sign 1)))
		  ((char= c #\-)
		   (if passed-mantissa
		       (setf mantissa-sign -1)
		       (setf sign -1)))
		  ((char= c #\.)
		   (setf passed-decimal t))
		  ((char= c #\e)
		   (setf passed-mantissa t))
		  (passed-mantissa
		   (setf mantissa-part
			 (+ (* mantissa-part 10)
			    (digit-char-p c))))
		  (passed-decimal
		   (setf decimal-part
			 (+ (* decimal-part 10)
			    (digit-char-p c))
			 decimal-length
			 (* 10 decimal-length)))
		  (t
		   (setf integer-part
			 (+ (* integer-part 10)
			    (digit-char-p c))))))
      (coerce (* sign (+ integer-part (/ decimal-part decimal-length)) (expt 10 (* mantissa-sign mantissa-part))) 'double-float))))

(defun string-to-ratio (string len)
  (when (and string (> (or len (length string)) 0))
    (let ((numerator 0)
	  (denominator 1)
	  (passed-decimal nil)
	  (sign 1))
      (declare (type integer numerator denominator))
      (loop for c across string
	 do (progn
	      (cond ((eq c #\.)
		     (setf passed-decimal t)t)
		    ((eq c #\-)
		     (setf sign (* -1 sign)))
		    (t
		     (when  passed-decimal
		       (setf denominator (* denominator 10)))
		     (setf numerator
			   (+ (digit-char-p c) (* numerator 10)))))))
      (* sign (/ numerator denominator)))))

(defun string-to-date (string &optional len)
  (declare (optimize (speed 3) (safety 3))
           (type (or null simple-string) string)
           (type (or null fixnum) len))
  (when (and string (> (or len (length string)) 9))
    (let ((y (parse-integer string :start 0 :end 4))
	  (m (parse-integer string :start 5 :end 7))
	  (d (parse-integer string :start 8 :end 10)))
      (encode-universal-time 0 0 0 d m y))))

(defun string-to-seconds (string &optional len)
  "Fairly ugly function to turn MySQL TIME duration into an integer representation.
   It's complicated because of ... well, read this:  http://dev.mysql.com/doc/refman/5.0/en/time.html"
  (declare (optimize (speed 3) (safety 3))
           (type (or null simple-string) string)
           (type (or null fixnum) len))
  (when string
    (let* ((strlen (or len (length string)))
	   (offset (- strlen 8)))
      (when (and (>= offset 0) (< offset 3))
	(let* ((start (if (eql #\- (elt string 0)) 1 0))
	       (h (parse-integer string :start start :end (+ 2 offset)))
	       (m (parse-integer string :start (+ 3 offset) :end (+ 5 offset)))
	       (s (parse-integer string :start (+ 6 offset) :end (+ 8 offset)))
	       (time (+ (* h 3600) (* m 60) s)))
	  (declare (type (integer 0 839) h)
		   (type (integer 0 59) m s))
	  (if (eql start 1)
	      (* -1 time)
	      time))))))

(defun string-to-universal-time (string &optional len)
  (declare (optimize (speed 3) (safety 3))
           (type (or null simple-string) string)
           (type (or null fixnum) len))
  (when (and string (> (or len (length string)) 0))
    (+ (string-to-date (subseq string 0 10))
       (string-to-seconds (subseq string 11)))))

(defparameter *type-map* (make-hash-table))

(eval-when (:load-toplevel)
  (mapcar (lambda (map)
	    (setf (gethash (first map) *type-map*) (second map)))
	  '((:DECIMAL  string-to-ratio)
	    (:TINY  string-to-integer)
	    (:SHORT  string-to-integer)
	    (:LONG  string-to-integer)
	    (:FLOAT  string-to-float)
	    (:DOUBLE  string-to-float)
	    (:NULL  (lambda (string)
		      (declare (ignore string))
		      nil))
	    (:TIMESTAMP  string-to-universal-time)
	    (:LONGLONG string-to-integer)
	    (:INT24  string-to-integer)
	    (:DATE  string-to-date)
	    (:TIME  string-to-seconds)
	    (:DATETIME  string-to-universal-time)
	    (:YEAR  string-to-integer)
	    (:NEWDATE  string-to-universal-time)
	    (:NEWDECIMAL  string-to-ratio))))

;;; Error handling
;;;
(define-condition mysql-error (error)
                  ((message :initarg :message :reader mysql-error-message)
		   (errno :initarg :errno :reader mysql-error-errno))
  (:report (lambda (condition stream)
             (format stream "MySQL error: \"~A\" (errno = ~D)."
                     (mysql-error-message condition)
		     (mysql-error-errno condition)))))

(define-condition cl-mysql-error (error)
                  ((message :initarg :message :reader cl-mysql-error-message))
  (:report (lambda (condition stream)
             (format stream "cl-mysql error: \"~A\""
                     (cl-mysql-error-message condition)))))

(defun error-if-non-zero (database return-value)
  (if (not (eql 0 return-value))
      (error 'mysql-error
	     :message (mysql-error database)
	     :errno (mysql-errno database)))
  return-value)

(defun error-if-null (database return-value)
  (if (null return-value)
      (error 'mysql-error
	     :message (mysql-error database)
	     :errno (mysql-errno database)))
  return-value)

(defun error-if-set (database)
  (let ((errno (mysql-errno database)))
    (when (not (eql 0 errno))
      (error 'mysql-error 
	     :message (mysql-error database)
	     :error errno))))

;;; High level API
;;;
(defmacro with-connection ((var &optional database) &body body)
  `(let ((,var (or ,database (cl-mysql-pool:get-connection))))
     ,@body))

(defun use (name &key database)
  "Equivalent to running:
   CL-USER> (query \"USE <name>\")"
  (with-connection (conn database)
    (error-if-non-zero conn (mysql-select-db conn name))
    (values)))

(defun decode-version (int-version)
  "Return a list of version details <major> <release> <version>"
  (let* ((version (mod int-version 100))
	 (major-version (floor int-version 10000))
	 (release-level (mod (floor int-version 100) 10)))
    (list major-version release-level version)))

(defun client-version ()
  "Returns a three part list containing the decoded client version information"
  (decode-version (mysql-get-client-version)))

(defun server-version (&key database)
  "Returns a three part list containing the decoded server version information"
  (with-connection (conn database)
    (decode-version (mysql-get-server-version conn))))

(defun list-dbs (&key database)
  (with-connection (conn database)
    (let ((result (error-if-null conn (mysql-list-dbs conn (null-pointer)))))
      (process-result-set result *type-map* conn))))

(defun list-tables (&key database)
  (with-connection (conn database)
    (let ((result (error-if-null conn (mysql-list-tables conn (null-pointer)))))
      (process-result-set result *type-map* conn))))

(defun list-fields (table &key database)
  (with-connection (conn database)
    (let ((result (error-if-null conn (mysql-list-fields conn table (null-pointer)))))
      (process-result-set result *type-map* conn))))

(defun list-processes (&key database)
  (with-connection (conn database)
    (let ((result (error-if-null conn (mysql-list-processes conn))))
      (process-result-set result *type-map* conn))))

;;; String/Character set/Collation stuff
;;;
(defun escape-string (string &key database)
  "Given a string, encode it appropriately.   This function relies on the fact that
   the character set encoding was set to UTF-8 when the connection is made."
  (when string
    (with-connection (conn database)
      (with-foreign-string (from-string string)
	(let* ((from-length (cffi-utf8-length from-string))
	       (to-length (1+ (* from-length 2)))
	       (to-string (foreign-alloc :unsigned-char :count to-length))
	       (return-string nil))
	  (unwind-protect (progn
			    (mysql-real-escape-string conn to-string from-string from-length)
			    (setf return-string (foreign-string-to-lisp to-string)))
	    (foreign-free to-string))
	  (values return-string))))))

(defun cffi-utf8-length (cffi-string)
  "We need this function because mysql_real_escape_string requires the length
   of the from string in bytes (not characters)"
  (do ((i 0 (incf i)))
      ((eql 0 (mem-ref cffi-string :unsigned-char i)) i)))

(defun get-character-set-info (&key database)
  "Returns the character set information for the connection as a sequence:
      (collation name number state)"
  (with-connection (conn database)
    (with-foreign-object (charset 'character-set)
      (mysql-get-character-set-info conn charset)
      (list (foreign-slot-value charset 'character-set 'csname)
	    (foreign-slot-value charset 'character-set 'name)
	    (foreign-slot-value charset 'character-set 'number)
	    (foreign-slot-value charset 'character-set 'state)))))

(defun set-character-set (csname &key database)
  (with-connection (conn database)
    (error-if-non-zero database (mysql-set-character-set conn csname))))

;;; Result set functions
;;;
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

(defparameter *binary-types* #(:BIT :BINARY :VARBINARY :GEOMETRY))
(declaim (inline extract-field process-row))
(defun extract-field (row field-index field-length type-map field-detail)
  "Returns either a string or an unsigned byte array for known binary types. The
   designation of binary types per the C API seems a bit weird.   Basically,
   BIT, BINARY and VARBINARY are binary and so are BLOBs with the binary flag
   set.   It seems that other fields also have the binary flag set that are not
   binary and the BIT type, whilst binary doesn't have the flag set.   Bizarre-o."
  (destructuring-bind (field-name field-type field-flag) field-detail
    (declare (ignore field-name)
             (optimize (speed 3) (safety 3))
	     (type (integer 0 65536) field-index field-flag)
	     (type (integer 0 4294967296) field-length )
	     (type (simple-array symbol)  *binary-types*))
    (if (eql field-length 0)
	(return-from extract-field nil))
    (if (or (and (eq field-type :BLOB)
		 (logtest +field-binary+ field-flag))
	    (find field-type *binary-types*))
	(let ((arr (make-array field-length :element-type '(unsigned-byte 8)))
	      (ptr (mem-ref row :pointer field-index)))
	  (loop for i from 0 to (1- field-length)
	     do (setf (elt arr i) (mem-ref ptr :unsigned-char i)))
	  (values  arr))
	(let ((fn (gethash field-type type-map)))
	  (declare (type (or null symbol function) fn))
	  (values (if fn
		      (funcall fn (mem-ref row :string field-index) field-length)
		      (mem-ref row :string field-index)))))))

(defun process-row (mysql-res row num-fields field-names-and-types type-map)
  (declare (optimize (speed 3) (safety 3))
	   (type (integer 0 65536) num-fields))
  (let* ((mysql-lens (mysql-fetch-lengths mysql-res))
         (int-size (foreign-type-size :int)))
    (declare (type (integer 0 16) int-size))
    (loop for i of-type fixnum from 0 to (* num-fields int-size) by int-size
       for f of-type list in field-names-and-types
       collect (extract-field row i
			      (mem-ref mysql-lens :unsigned-long i) type-map f))))

(defun result-data (mysql-res type-map field-names-and-types)
  "Internal function that processes a result set and returns all the data.
   If field-names-and-types is NIL the raw (string) data is returned"
  (declare (optimize (speed 3))
	   (ftype (function (t t fixnum list (or t null)) list) process-row))
  (let ((num-fields (mysql-num-fields mysql-res)))
    (loop for row = (mysql-fetch-row mysql-res)
       until (null-pointer-p row)
       collect (process-row mysql-res row num-fields field-names-and-types type-map))))

(defun process-result-set (mysql-res type-map database)
  "Result set will be NULL if the command did not return any results.   In this case we return a cons
   the rows affected."
  (declare (optimize (speed 3)))
  (cond ((null-pointer-p mysql-res)
	 (cons (mysql-affected-rows (or database (cl-mysql-pool:get-connection))) nil))
	(t
	 (let ((fields (field-names-and-types mysql-res)))
					;(print fields)
	   (cons
	    (mapcar #'car fields)
	    (result-data mysql-res type-map fields))))))

(defun next-result-set (last-result &key database)
  "Retrieve the next result set."
  (with-connection (conn database)
    (when last-result
      (mysql-free-result last-result)
      (if (not (eql 0 (mysql-next-result conn)))
	  (return-from next-result-set nil)))
    (error-if-null conn (mysql-use-result conn))))

(defun next-row (result-set &key (type-map *type-map*) database)
  "Retrieve and decode (according to the type map) the next row in the query.   This
   function will return NIL when the last row has been retrieved."
  (with-connection (conn database)
    (let* ((fields-and-names (field-names-and-types result-set))
	   (row (mysql-fetch-row result-set)))
      (if (null-pointer-p row)
	(error-if-set conn)
	(process-row result-set row (length fields-and-names) fields-and-names type-map)))))

(defun query (query &key (type-map *type-map*) database (store t))
  "For a SELECT query or stored procedure that returns data, query will return 
   a list of result sets.   Each result set will have 1 or more sublists 
   where the first sublist contains the column names and the remaining lists 
   represent the rows of the result set.  If the query does not return a result
   set (for example if the query is an INSERT, UPDATE) the return value is the 
   number of rows affected.    Because cl-mysql supports multiple-statements 
   you can execute code like the following:

    CL-MYSQL-SYSTEM> (query \"CREATE TABLE a(a INT); 
                   INSERT INTO a (a) VALUES (1); DELETE FROM a; DROP TABLE a\")
    ((0) (1) (1) (0))</code></pre>

    The type-map, if set will alter the decoding into CL types.   If you set this to
    nil it will have the effect of disabling all CL type conversions and return
    either character or binary data only.   This might be useful for performance reasons, (cl-mysql 
    is much faster when it doesn't need to translate types) but it also might 
    be all you need.   Consider for instance if you're displaying a lot of 
    numerics on a web-page.    If you do not need to convert the data into
    floats/integers before displaying them on a page then raw could be useful 
    here too.  Finally, <strong>cl-mysql</strong> attempts to convert all 
    numeric types to their closest CL representation.   For very large numerics,
    or numerics that have very high precision this might not be what you want.
    In this case you could attempt to write your own conversion routine and 
    inject it into cl-mysql through the type-map."
  (with-connection (conn database)
    (error-if-non-zero conn (mysql-query conn query))
    (if store
      (loop for result-set = (mysql-store-result conn)
	 nconc (list (unwind-protect
			  (process-result-set result-set (or type-map
							     (make-hash-table)) conn)))
	 until (progn
		 (mysql-free-result result-set)
		 (not (eql 0 (mysql-next-result conn))))))))

(defun ping (&key database)
  "Check whether a connection is established or not.  If :opt-reconnect is 
   set and there is no connection then MySQL's C API attempts a reconnection."
  (with-connection (conn database)
    (error-if-non-zero conn (mysql-ping conn))
    (values t)))

(defun %set-string-option (option value &key database)
  (let ((retval 0))
    (with-connection (conn database)
      (with-foreign-pointer-as-string (str 255)
	(setf retval (mysql-options conn
				    (foreign-enum-value 'enum-options option)
				    (lisp-string-to-foreign value str 255)))))
    (values retval)))

(defun %set-int-option (option value &key database)
  (let ((retval 0))
    (with-connection (conn database)
      (with-foreign-object (int-value :int)
	(setf (mem-ref int-value :int) value)
	(setf retval (mysql-options conn
				    (foreign-enum-value 'enum-options option)
				    int-value))))
    (values retval)))

(defun option (option value &key database)
  "Use this to set a client specific connection option.
   CL-USER> (option :opt-reconnect 1)"
  (typecase value
    (string (%set-string-option option value :database database))
    (null   (%set-int-option option 0 :database database))
    (t      (%set-int-option option value :database database))))

(defun get-field (column-name field-names-and-types row)
  "Returns the correct element in the sequence from the row that matches the column-name"
  (elt row (position column-name field-names-and-types :test (lambda (x)
						      (string= (car x) column-name)))))

