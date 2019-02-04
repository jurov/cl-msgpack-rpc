(asdf:defsystem #:cl-msgpack-rpc-usocket
  :description "RPC library implementation of messagepack-rpc"
  :author "Juraj Variny"
  :license "MIT"
  :depends-on (#:usocket-server #:cl-msgpack-rpc #:bordeaux-threads)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) 
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))

  :serial t
  :components ((:file "usocket/package")
	       (:file "usocket/functions")))
