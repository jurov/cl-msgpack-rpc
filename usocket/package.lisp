(defpackage #:cl-msgpack-rpc-usocket
  (:use #:cl #:msgpack-rpc #:usocket #:bordeaux-threads)
  (:export #:rpc-server
           #:rpc-stream))

