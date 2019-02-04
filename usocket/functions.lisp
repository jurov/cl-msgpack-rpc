(in-package :cl-msgpack-rpc-usocket)

(defclass usock-rpc-stream (rpc-stream) 
   ;;Lock for output to output-stream
   ((lock :accessor lock :initform (make-lock)))
  )

(defmethod msgpack-rpc:mpk-encode :around ((inst usock-rpc-stream) msgtyp msgid obj)
  (with-lock-held ((lock inst))
                  (call-next-method inst msgtyp msgid obj)))

;;Threadsafe, blocking
(defmethod sync-rpcall 
  ((client usock-rpc-stream) 
   method 
   &rest params
   &aux (res nil)
   (err nil)
   (condition (make-condition-variable))
   (lock (make-lock))
   (callback (lambda (&rest stuff)
               (setf res stuff)
               (condition-notify condition)))
   (errback (lambda (&rest errr)
              (setf err errr)
              (condition-notify condition))))
  (acquire-lock lock)
  (let ((myid (apply #'rpcall client callback errback method params)))
    (declare (ignore myid))
    (thread-yield)
    (loop until (or res err) do (condition-wait condition lock))
    (if err
      (progn
        (format *rpc-error-out* "remote error: ~a~%" (first err))
        (force-output *rpc-error-out*)
        (cerror "ignore remote error ~a" (write-to-string err)))
      (first res))))

(defun make-usock-rpc (socket)
  (let ((inst (make-instance 'usock-rpc-stream
                             :input-stream (socket-stream socket)
                             :output-stream (socket-stream socket))))

    inst))

(defun rpclisten(socket
                  &optional (inst (make-usock-rpc socket)))
    (loop
      while (rpclisten-once inst))
  (when *rpc-debug-out*
  (format *rpc-debug-out* "rpclisten exited: ~{~a~^.~}:~s"
                                 (coerce (get-peer-address socket) 'list)
                                 (get-peer-port socket))))

(defun rpclisten-async(socket &key (connect-cb nil))
  (make-thread #'(lambda () 
                   (let ((inst (make-usock-rpc socket)))
                     (when connect-cb (funcall connect-cb inst))
                     (rpclisten socket inst)))
                   :name (format nil "msgpack-rpc ~{~a~^.~}:~s"
                                 (coerce (get-peer-address socket) 'list)
                                 (get-peer-port socket))))


(defun rpc-server (port &key (connect-cb nil))
;;Open listening socket and serve each new connection in its own thread
  (make-thread
    (let ((lsck (usocket:socket-listen #(127 0 0 1) port :element-type '(unsigned-byte 8))))
      (lambda()
        (loop do (rpclisten-async (usocket:socket-accept lsck) :connect-cb connect-cb))))
    :name (format nil "msgpack rpc-server ~a" port)))
     
(defun rpc-stream (port &key (connect-cb nil)
  ;; Open connection (async)
  &aux (socket (usocket:socket-connect #(127 0 0 1) port :element-type '(unsigned-byte 8))))
  (rpclisten-async socket :connect-cb connect-cb))

(defun rpc-stream-sync (port
  ;; Open connection (sync) and return rpc object
  &aux (socket (usocket:socket-connect #(127 0 0 1) port :element-type '(unsigned-byte 8))))
  (make-usock-rpc socket))


