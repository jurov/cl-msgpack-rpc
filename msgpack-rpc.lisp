(in-package :msgpack-rpc)
(declaim (optimize (debug 3) (safety 3)))

(defvar +msgid+ 1)

;;Open debugger locally instead of sending error 
(defvar +DEBUG+ nil)

(defvar *rpc-streams* ())
;;When client executes incoming RPC call, sets this
(defvar *rpc-current* nil)

(defvar *rpc-debug-out* sb-sys:*stderr*)
(defvar *rpc-error-out* sb-sys:*stderr*)

(defconstant =REQUEST= 0)
(defconstant =RESPONSE= 1)
(defconstant =NOTIFY= 2)

(defclass rpc-stream () 
  ;;Manages RPC streams, provides basic RPC interface with asynchronous callbacks.
  ((input-stream :accessor input-stream :initarg :input-stream)
   (output-stream :accessor output-stream :initarg :output-stream)
   ;;Lock for output to output-stream
;   (lock :accessor lock :initform (make-lock))
   ;;callback handles
   (handles :accessor handles :initform (make-hash-table))
   ;;Package with callable symbols. By default nothing can be called.
   (inpkg :accessor inpkg :initform nil)
   ))

(defmethod initialize-instance :after ((inst rpc-stream) &key &allow-other-keys)
  (push inst *rpc-streams*))

(defmethod do-call ((inst rpc-stream) funcname params
                                      ;;Returns error or nil + list of values ...
                                      &aux (*rpc-current* inst)
                                      (fnsym (and (inpkg inst) 
                                                  (find-symbol funcname (inpkg inst)))))
  (if +DEBUG+ 
    (handler-bind 
      ((error #'invoke-debugger))

      (list nil (apply fnsym params)))
    (handler-case 
      (list nil (apply fnsym params))
      (error (e)    (format *rpc-error-out* "do-call got error: ~a~%" e) 
             (force-output *rpc-error-out* )

             (list e)))))

(defmethod mpk-encode ((inst rpc-stream) msgtyp msgid obj
                                         &aux (params (make-array (length (rest obj)) :initial-contents (rest obj))))

  (if msgid
  (mpk:encode-stream (list msgtyp msgid (first obj) params) (output-stream inst))
  (mpk:encode-stream (list msgtyp (first obj) params) (output-stream inst)))

  (when *rpc-debug-out*
    (let ((*print-pretty* nil)) (format *rpc-debug-out* "encoded(~A ~A): ~S ~%" msgid msgtyp obj))
    (force-output *rpc-debug-out*))
  (force-output (output-stream inst)))

(defmethod mpk-decode ((inst rpc-stream))
  (let* ((mpk:*decoder-prefers-lists* t)
         (res (mpk:decode-stream (input-stream inst))))

    (when *rpc-debug-out*
      (let ((*print-pretty* nil)) (format *rpc-debug-out* "decoded: ~S~%" res))
      (force-output *rpc-debug-out*))
    res))

(defmethod rpclisten-once((inst rpc-stream) &aux (ddata nil))
  ;;Work horse. Call when input-stream is ready for readings
  (handler-case
    (let* ((data (mpk-decode inst)))
      (setq ddata data)
      (cond 

             ((eq (first data) =REQUEST=) (apply #'rpclisten-request inst (rest data)))
             ((eq (first data) =RESPONSE=) (apply #'rpclisten-response inst (rest data)))
             ((eq (first data) =NOTIFY=) (apply #'rpclisten-notify inst (rest data)))))

    ;; forcedly handling all conditions - drop connection
    (condition (c)
               (format *rpc-error-out* "Decoded RPC data: ~s~%" ddata) 
               (format *rpc-error-out* "rpclisten-once got error: ~a~%" c) 
               (force-output *rpc-error-out* )
               (drop-all inst)
               nil)))

(defmethod rpcresponse ((client rpc-stream) msgid error res
                                            ;;Send response to incoming RPC call
                                            &aux (err (if (typep error 'condition) (write-to-string error) error )))
  (handler-case
    (handler-bind
      ((condition (lambda (c)
                    (when *rpc-debug-out*
                      ;;print backtrace in context where it occurred
                      (uiop/image::print-condition-backtrace c :stream *rpc-debug-out*)))))


      (if (first err)
        (mpk-encode client (list =RESPONSE= msgid err nil))

        (mpk-encode client (list =RESPONSE= msgid nil res))))
    ;; handle serialization errors here - using handler-bind would not unlock rpc lock
    (condition (c)
               (when *rpc-debug-out*
                 (format *rpc-debug-out* "sending RPC error: ~a~%" c)
                 (force-output *rpc-debug-out*))
               (mpk-encode client =RESPONSE= msgid (list (write-to-string c) nil)))))

(defmethod rpclisten-request((client rpc-stream) msgid meth params)
           (apply #'rpcresponse client msgid
                  (do-call client meth params)))

(defmethod rpclisten-response((client rpc-stream) msgid err res) 
  (let ((callbacks (gethash msgid (handles client))))
    (if callbacks
      (progn
        (remhash msgid (handles client))
        (if err
          (funcall (second callbacks) err)
          (funcall (first callbacks) res)))
      (cerror "Unexpected reply:~S" res))))

(defmethod rpclisten-notify((client rpc-stream) method params) 
  (let  ((notifyres (do-call client method params)))
    (when *rpc-debug-out*
      (format *rpc-debug-out* "notify: ~s ~s ~%~s~%" method params 
              notifyres)
      (force-output *rpc-debug-out*))))

(defmethod rpcall ((client rpc-stream) callback errback method &rest params
                                       &aux (myid (incf +msgid+)))
  (setf (gethash myid (handles client)) (list callback errback))
  (when *rpc-debug-out*
    (format *rpc-debug-out* "sending call: ~s~%"  `(,method ,@params))
    (force-output *rpc-debug-out*))
  (mpk-encode  client =REQUEST= myid `(,method ,@params))
  myid)


;;Placeholder for reentrant/concurent implementations
(defgeneric sync-rpcall (rpc-stream string &rest params))

;;No locking or reentrancy or replies reordering whatsoever. Do not mix with async versions!
(defun naive-rpcall (client method &rest params
                            &aux (res nil)
                            (err nil)
                            (callback (lambda (stuff)
                                        (setf res stuff)))
                            (errback (lambda (errr)
                                       (setf err errr))))
  (let ((myid (apply #'rpcall client callback errback method params)))
    (declare (ignore myid))
    (rpclisten-once client)
    (if err
      (progn
        (format *rpc-error-out* "remote error: ~a~%" err)
        (force-output *rpc-error-out*)
        ;;some errors have required parameters, so just convert them into string - simple-error
        (cerror "ignore remote error: ~a" (write-to-string err) ))
      res)))

(defmethod notify ((client rpc-stream) method &rest params)
               (when *rpc-debug-out*
                 (format *rpc-debug-out* "sending notify: ~s~%" `(,method ,@params))
                 (force-output *rpc-debug-out*))
               (mpk-encode client =NOTIFY= nil `(,method ,@params)))

(defmethod drop-all((inst rpc-stream))
  ;;Closes streams and calls all outstanding errbacks with control-error. Returns their count.
  (setf *rpc-streams* (delete inst *rpc-streams*))
  (with-slots (input-stream output-stream handles) inst 
    (when output-stream (close output-stream ))
    (unless (eq output-stream input-stream )
      (when input-stream (close input-stream)))
    (when (hash-table-p handles )
      (prog1 (hash-table-count handles) 
        (loop for hdl being the hash-values of handles
              do (funcall (second hdl) '(cl:control-error)))
        (setf handles nil)))))
