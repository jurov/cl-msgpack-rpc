(asdf:defsystem #:cl-msgpack-rpc-neovim
  :description "RPC library implementation of messagepack-rpc"
  :author "Juraj Variny"
  :license "MIT"
  :depends-on (#:cl-msgpack-rpc-usocket)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) 
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))

  :serial t
  :components ((:file "neovim/package")
	       (:file "neovim/nvim")
           (:file "neovim/api")))
