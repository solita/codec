
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


