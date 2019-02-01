(defpackage #:msgpack-rpc
  (:use #:cl #:messagepack)
  (:export #:*rpc-streams*
           #:*rpc-debug-out*
           #:*rpc-error-out*
           #:rpc-stream
           #:naive-rpcall
           #:notify
           ))

