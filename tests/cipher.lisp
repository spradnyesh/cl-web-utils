(in-package :web-utils-tests)

(def-suite :cipher :in :web-utils)
(in-suite :cipher)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; mocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-config (string)
  string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash
(test hash
  (is (equal (macroexpand-1 '(web-utils::hash "abc"))
             '(IRONCLAD:BYTE-ARRAY-TO-HEX-STRING
               (IRONCLAD:DIGEST-SEQUENCE "abc"
                (IRONCLAD:ASCII-STRING-TO-BYTE-ARRAY
                 WEB-UTILS::TEXT))))))

;; md5-hash
(test md5-hash
  (is (string-equal (md5-hash "abc")
                    "900150983cd24fb0d6963f7d28e17f72")))

;; sha256-hash
(test sha256-hash
  (is (string-equal (sha256-hash "abc")
                    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")))

;; get-cipher
; TODO

;; do-encrypt
(test do-encrypt
  (is (string-equal (do-encrypt "abc" "def")
                    "3SSIR")))

;; do-decrypt
(test do-decrypt
  (is (string-equal (do-decrypt "3SSIR" "def")
                    "abc")))

;; insecure-encrypt
(test insecure-encrypt
  (is (string-equal (insecure-encrypt "abc")
                    "3SSIR")))

;; insecure-decrypt
(test insecure-decrypt
  (is (string-equal (insecure-decrypt "3SSIR")
                    "abc")))
