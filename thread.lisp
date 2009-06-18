;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Author: Steve Knight <stknig@gmail.com>
;;;;
(in-package "CL-MYSQL-SYSTEM")

(eval-when (:load-toplevel)
  #+allegro (mp:start-scheduler))

(defun make-lock (name)
  #+sb-thread (sb-thread:make-mutex :name name)
  #+allegro (mp:make-process-lock :name name))

(defun make-wait-resource ()
  #+sb-thread (sb-thread:make-waitqueue))

(defun wait-on-threads (threads)
  #+sb-thread (mapcar #'sb-thread:join-thread threads)
  #+allegro (mapcar (lambda (p)
                      (mp:process-wait "Joining ..." (lambda ()
                                                       (not (mp:process-alive-p p))))) threads)
 )

(defmacro with-lock (lock &body body)
  #+sb-thread `(sb-thread:with-recursive-lock (,lock) ,@body)
  #+allegro `(mp:with-process-lock (,lock) ,@body)
  #-(or sb-thread allegro) `(progn ,@body))

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
                                   (wait-queue-lock pool))))))))
  #+allegro (mp:process-wait "Waiting for pool" #'can-aquire pool))

(defun pool-notify (pool)
  #+sbcl (sb-thread:with-mutex ((wait-queue-lock pool))
	    (sb-thread:condition-notify (wait-queue pool))))

