(eval-when (:compile-toplevel)
  (error "This ASDF file should be run interpreted."))

(defsystem swank-client
  :name "Swank Client"
  :description "Client side of the Swank protocol."
  :long-description "An implementation of the client side of Slime's Swank debugging protocol."
  :version "1.6"
  :author "Robert Brown <robert.brown@gmail.com>"
  :license "GPL version 2.  See the copyright messages in individual files."
  :depends-on (:bordeaux-threads :alexandria :slynk :usocket)
  ;;:in-order-to ((test-op (test-op swank-client/test)))
  :components
  ((:module src
    :components
    ((:file "package")
     (:file "util")
     (:file "swank-client" :depends-on ("util" "package"))))))

;; (defsystem swank-client/test
;;   :name "Swank Client test"
;;   :description "Test code for package SWANK-CLIENT."
;;   :version "1.6"
;;   :author "Robert Brown <robert.brown@gmail.com>"
;;   :license "GPL version 2.  See the copyright messages in individual files."
;;   :depends-on (hu.dwim.stefil swank-client)
;;   :components
;;   ((:file "swank-client-test")))

;; (defmethod perform ((operation test-op) (component (eql (find-system 'swank-client/test))))
;;   (symbol-call 'swank-client-test 'test-swank-client))
