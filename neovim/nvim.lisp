(in-package #:msgpack-rpc-neovim)

(defparameter *ext-type-list*
  (messagepack:define-extension-types
    '(:numeric
      0
      Buffer
      Window
      Tabpage
      )))

(defparameter *ext-lookup-table*
  (make-hash-table))


(defclass neovim-rpc-stream (cl-msgpack-rpc-usocket::usock-rpc-stream)
  ())

(defmethod msgpack-rpc:mpk-encode :around ((inst neovim-rpc-stream) msgtyp msgid obj)
  (let ((messagepack:*extended-types* *ext-type-list*)
        (messagepack:*lookup-table* *ext-lookup-table*))
    (call-next-method inst msgtyp msgid obj)))

(defmethod msgpack-rpc:mpk-decode :around ((inst neovim-rpc-stream))
  (let ((messagepack:*extended-types* *ext-type-list*)
        (messagepack:*lookup-table* *ext-lookup-table*))
    (call-next-method inst)))

(defun make-usock-nvim-rpc (socket &rest args)
  (let ((inst (apply #'make-instance 'neovim-rpc-stream
                             :input-stream (usocket:socket-stream socket)
                             :output-stream (usocket:socket-stream socket)
                             args)))
    inst))
