(eval-when (:compile-toplevel)
  (error "This ASDF file should be run interpreted."))

(defsystem slynk-client
  :name "Slynk Client"
  :description "Client side of the Slynk protocol."
  :long-description "An implementation of the client side of Slime's Slynk debugging protocol."
  :version "1.6"
  :author "Robert Brown <robert.brown@gmail.com>"
  :license "GPL version 2.  See the copyright messages in individual files."
  :depends-on (:bordeaux-threads :alexandria :slynk :usocket)
  :components
  ((:module src
    :components
    ((:file "package")
     (:file "util")
     (:file "slynk-client" :depends-on ("util" "package"))))))

(defsystem slynk-client/test
  :name "Slynk Client test"
  :description "Test code for package SLYNK-CLIENT."
  :version "1.6"
  :author "Robert Brown <robert.brown@gmail.com>"
  :license "GPL version 2.  See the copyright messages in individual files."
  :depends-on (:fiveam :slynk-client)
  :components
  ((:module tests
    :components
    ((:file "slynk-client-test")))))

