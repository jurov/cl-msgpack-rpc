(defpackage #:msgpack-rpc
  (:use #:cl #:messagepack)
  (:export #:*rpc-streams*
           #:*rpc-debug-out*
           #:*rpc-error-out*
           #:rpc-stream
           #:do-call
           #:mpk-encode
           #:rpcresponse
           #:rpclisten-once
           #:rpclisten-request
           #:rpclisten-response
           #:rpclisten-notify
           #:rpcall
           #:sync-rpcall
           #:naive-rpcall
           #:notify
           #:drop-all
           ))

