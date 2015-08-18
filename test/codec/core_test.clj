
;;; ASN.1 encoding / known values

(ns codec.core-test
  (:require [clojure.test :refer :all]
            [codec.core :refer :all]))

(deftest dig8
  (testing "8-bit digits"
    (is (= (to-8bit-digits 111111111111111) [101 14 18 78 241 199]))))

(deftest big-1
  (testing "bignum 127"
    (is (= (bignum 127) [127]))))

(deftest big-2
  (testing "bignum"
    (is (= (bignum 1111111111111111111) [143 181 221 181 178 222 145 227 71]))))

(deftest id-1
  (testing "object identifier"
    (is (= (encode-object-identifier (list 1 2 3 4)) [6 3 42 3 4]))))

(deftest bitstring-1
  (testing "bitstring conversion" 
    (is (= (bitstring2bytes "1111111100000000111100000000111100000001")
           [255 0 240 15 1]))))

(deftest bitstring-2
  (testing "bitstring encoding"
    (is (= (encode-bitstring "1010111111111000000001111000010101")
           [3 6 6 175 248 7 133 64]))))

(deftest ia5string-1
  (testing "ia5string encoding"
    (is (= (encode-ia5string "Hello, world!")
           [22 13 72 101 108 108 111 44 32 119 111 114 108 100 33]))))

(deftest ostring
  (testing "octet string encoding"
    (is (= (encode-octet-string (list 0 1 2 127 128 255))
           [4 6 0 1 2 127 128 255]))))

(deftest tsequence
  (testing "sequence encoding"
    (is (= (encode-sequence encode-null encode-null encode-null)
           [48 6 5 0 5 0 5 0]))))

(deftest tset
  (testing "set encoding"
    (is (= (encode-set (encode-integer 3) (encode-integer 1) (encode-integer 2))
           [49 9 2 1 3 2 1 1 2 1 2]))))

(deftest tsetof
  (testing "set-of encoding"
    (is (= (encode-set-of (encode-integer 3) (encode-integer 1) (encode-integer 2))
           [49 9 2 1 1 2 1 2 2 1 3]))))

(deftest expl1
  (testing "explicit encoding"
    (is (= (encode-explicit 1 (encode-sequence (encode-object-identifier (list 1 2 3)) (encode-integer 42) encode-null))
           [161 11 48 9 6 2 42 3 2 1 42 5 0]))))

(deftest utctime
  (testing "utctime encoding"
    (is (= (encode-utc-time "200630093839Z")
           [23 13 50 48 48 54 51 48 48 57 51 56 51 57 90]))))

(deftest pritable
  (testing "printable string encodind"
    (is (= (encode-printable-string "Clojutre")
           [19 8 67 108 111 106 117 116 114 101]))))

(deftest asn-dsl-1
  (testing "asn dsl"
    (is (= [48 45 6 3 42 3 4 160 16 49 6 2 1 2 2 1 1 49 6 2 1 1 2 1 2 19 3 102 111 111 23 13 50 48 48 54 51 48 48 57 51 56 51 57 90 5 0]
           (asn1-encode
            [:sequence
              [:identifier 1 2 3 4]
              [:explicit 0
                [:set 2 1]
                [:set-of 2 1]]
              "foo"
              [:utctime "200630093839Z"]
              ()])))))

(deftest asn-enc-oct
  (testing "asn dsl encapsulated o-string"
    (is (= [4 10 4 8 2 6 10 27 1 212 177 199]
          (asn1-encode
            [:encapsulated-octet-string
              [:encapsulated-octet-string 11111111111111]])))))

(deftest asn-enc-bit
  (testing "asn dsl encapsulated b-string"
    (is (= [3 7 0 2 4 66 58 53 199]
          (asn1-encode
              [:encapsulated-bitstring 1111111111])))))


;;; ASN.1 AST -> bytes -> AST' equality comparisons

(deftest asn-rencode-1 (testing "asn-rencode 1" (is true (asn1-rencode 0))))
(deftest asn-rencode-2 (testing "asn-rencode 2" (is true (asn1-rencode 127))))
(deftest asn-rencode-3 (testing "asn-rencode 3" (is true (asn1-rencode 128))))
(deftest asn-rencode-4 (testing "asn-rencode 4" (is true (asn1-rencode 255))))
(deftest asn-rencode-5 (testing "asn-rencode 5" (is true (asn1-rencode 256))))
(deftest asn-rencode-6 (testing "asn-rencode 6" (is true (asn1-rencode 65535))))
(deftest asn-rencode-7 (testing "asn-rencode 7" (is true (asn1-rencode 65536))))
(deftest asn-rencode-8 (testing "asn-rencode 8" (is true (asn1-rencode 11111111111111))))

(deftest asn-rencode-9 (testing "asn-rencode 9" (is true (asn1-rencode [:octet-string (list)]))))
(deftest asn-rencode-10 (testing "asn-rencode 10" (is true (asn1-rencode [:octet-string (list 1)]))))
(deftest asn-rencode-11 (testing "asn-rencode 11" (is true (asn1-rencode [:octet-string (list 0 1 1 0 1 1 1 0 0 1 0 1 1 1 0 1 1 1 1 0 0 0)]))))

(deftest asn-rencode-12 (testing "asn-rencode 12" (is true (asn1-rencode [:sequence]))))
(deftest asn-rencode-13 (testing "asn-rencode 13" (is true (asn1-rencode [:sequence 1]))))
(deftest asn-rencode-14 (testing "asn-rencode 14" (is true (asn1-rencode [:sequence 1 2]))))
(deftest asn-rencode-15 (testing "asn-rencode 15" (is true (asn1-rencode [:sequence 1 2 3 4 5 6]))))
(deftest asn-rencode-15 (testing "asn-rencode 15" (is true (asn1-rencode [:sequence [:sequence 1 2] [:sequence 3 4 5] 6]))))

(deftest asn-rencode-16 (testing "asn-rencode printable string " (is true (asn1-rencode [:printable-string "Hello, world!"]))))
(deftest asn-rencode-17 (testing "asn-rencode ia5string "        (is true (asn1-rencode [:ia5string "foo@bar.com"]))))

(deftest asn-rencode-18 (testing "asn-rencode set "        (is true (asn1-rencode [:set 1 2 3 4]))))
(deftest asn-rencode-19 (testing "asn-rencode set "        (is true (asn1-rencode [:set [:set [:set 1 [:sequence 2 [:set 3 4]]]]]))))


;;; Base64

(deftest b64-1
  (testing "base64 blank"
    (is (= (base64-decode "")
           ""))))

(deftest b64-2
  (testing "base64 a"
    (is (= (base64-decode "YQ==")
           "a"))))

(deftest b64-3
  (testing "base64 ab"
    (is (= (base64-decode "YWI=")
           "ab"))))

(deftest b64-4
  (testing "base64 abc"
    (is (= (base64-decode "YWJj")
           "abc"))))

(deftest b64-5
  (testing "base64 abcd"
    (is (= (base64-decode "YWJjZA==")
           "abcd"))))

(deftest b64-6
  (testing "base64 HAL"
    (is (= (base64-decode "T3BlbiB0aGUgcG9kIGJheSBkb29ycyBwbGVhc2UgSEFMIQo=")
           "Open the pod bay doors please HAL!\n"))))


