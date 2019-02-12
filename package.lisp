(defpackage #:msgpack-rpc
  (:use #:cl #:messagepack)
  (:export #:*rpc-streams*
           #:*rpc-debug-out*
           #:*rpc-error-out*
           #:rpc-stream
           #:rpcname
           #:inpkg
           #:do-call
           #:mpk-encode
           #:mpk-decode
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

