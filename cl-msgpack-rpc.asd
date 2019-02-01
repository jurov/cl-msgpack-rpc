(asdf:defsystem #:cl-msgpack-rpc
  :description "RPC library implementation of messagepack-rpc"
  :author "Juraj Variny"
  :license "MIT"
  :depends-on (#:cl-messagepack)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) 
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))

  :serial t
  :components ((:file "package")
	       (:file "msgpack-rpc")))
