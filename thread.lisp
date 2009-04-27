;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Author: Steve Knight <stknig@gmail.com>
;;;;
(in-package "CL-MYSQL-SYSTEM")

(defun make-lock (name)
  #+sbcl (sb-thread:make-mutex :name name))

(defun make-wait-resource ()
  #+sbcl (sb-thread:make-waitqueue))

(defmacro with-lock (lock &body body)
  #+sbcl `(sb-thread:with-recursive-lock (,lock) ,@body)
  #-sbcl body)

(defun pool-wait (pool)
  "We release the pool lock we're holding to wait on the queue lock.   Note that
   we MUST gain the pool lock back before we can continue because dependent code
   is expecting us.   This method is a little fiddly because we need to make sure
   that we have the pool lock before we call can-aquire, but also we should not
   release the lock unless we can't aquire.  We also throw in an unwind-protect
   to try and make sure that whatever happens we reenter the pool code with the
   lock held. "
  #+sbcl (progn
	   (sb-thread:release-mutex (pool-lock pool))
	   (sb-thread:with-mutex ((wait-queue-lock pool))
	     (loop until (progn
			   (sb-thread:get-mutex (pool-lock pool))
			   (cond
			     ((can-aquire pool) t)
			     (t
			      (sb-thread:release-mutex (pool-lock pool))
			      (sb-thread:condition-wait (wait-queue pool)
							(wait-queue-lock pool)))))))))

(defun pool-notify (pool)
  #+sbcl (sb-thread:with-mutex ((wait-queue-lock pool))
	    (sb-thread:condition-notify (wait-queue pool))))

