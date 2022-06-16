(require :asdf)

(asdf:load-system :slynk-client)
(asdf:load-system :slynk-client/test)

;; Insert 5am project suite name
;; (5am:run! ...)

(in-package :slynk-client-test)

(5am:run! 'test-slynk-client)

(sb-ext::quit)
