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

  
