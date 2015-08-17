(ns codec.core)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;;;
;;; ASN.1 (DER) encoding
;;;

(defn to-7bit-digits [n]
  (if (< n 128)
    (list n)
    (cons (bit-and n 127)
      (to-7bit-digits 
        (bit-shift-right n 7)))))

(defn to-8bit-digits [n]
  (loop [n n out (list)]
    (let [this (bit-and n 255) n (bit-shift-right n 8)]
      (if (= n 0)
        (cons this out)
        (recur n (cons this out))))))

(defn bignum [in]
  (loop
    [left (bit-shift-right in 7)
     out (list (bit-and in 127))]
    (if (= left 0)
      out
      (recur (bit-shift-right left 7)
        (cons (bit-or (bit-and left 127) 128) out)))))

(defn length-bs [len]
  (if (< len 128)
    (list len)
    (let
      [ds (to-8bit-digits len)
       nd (count ds)]
      (if (< nd 128)
        (cons (bit-or 128 nd) ds)
        (throw (Exception. "too many length bytes"))))))

(defn byte2bits [tl b]
  (loop [bit 128 out tl]
    (if (= bit 0)
      out
      (recur (bit-shift-right bit 1)
        (cons
          (if (= (bit-and b bit) 0) \0 \1)
          out)))))

(defn fold [op st lst]
  (if (empty? lst)
    st
    (recur op (op st (first lst)) (rest lst))))

(defn bytes2bitstring [bs]
  (reverse
    (fold byte2bits () bs)))
 
(defn identifier [class consp tagnum]
  (if (> tagnum 30)
    (cons
      (bit-or (bit-shift-left class 6) (bit-or (bit-shift-left consp 5) 31))
      (bignum tagnum))
    (list
      (bit-or (bit-shift-left class 6) (bit-or (bit-shift-left consp 5) tagnum)))))

;;; todo: preshift

(def class-universal  0)
(def class-application 1)
(def class-context-specific 2)
(def class-private 3)

(def is-primitive 0)
(def is-constructed 1)

(def tag-integer 2)
(def tag-bit-string 3)
(def tag-octet-string 4)
(def tag-null 5)
(def tag-object-identifier 6)
(def tag-sequence 16) ;; also sequence-of
(def tag-set 17) ;; also set-of
(def tag-printable-string 19)
(def tag-t61string 20)
(def tag-ia5string 22)
(def tag-utc-time 23)

(def integer-identifier 
  (identifier class-universal is-primitive tag-integer))

(defn encode-integer [int]
  (concat integer-identifier
    (cond
      (= int 0)
        (list 1 0)
      (< int 0)
        (throw (Exception. "negative integer"))
      :true
        (let
          [bytes (to-8bit-digits int)
           bytes (if (= 0x80 (bit-and (first bytes) 0x80))
            (cons 0 bytes)
            bytes)]
          (concat
            (length-bs (count bytes))
            bytes)))))

(defn bitstring2bytes [str]
  (loop
    [bs (seq str)
     bit 128
     this 0
     out (list)]
    (if (empty? bs)
      (reverse
        (if (= bit 128) out (cons this out)))
      (let [this (if (= (first bs) \1) (bit-or bit this) this)]
        (if (= bit 1)
          (recur (rest bs) 128 0 (cons this out))
          (recur (rest bs) (bit-shift-right bit 1) this out))))))

(def encode-null 
  (list 5 0))

(defn encode-object-identifier [ids]
  ;; first two ids are merged and there are always at least two of them
  (let
    [ids (cons (+ (* 40 (first ids)) (nth ids 1)) (rest (rest ids)))
     contents
      (apply concat 
        (map bignum ids))]
    (concat
      (identifier class-universal is-primitive tag-object-identifier)
      (concat
        (length-bs (count contents))
        contents))))

(defn abs [n] (if (< n 0) (* n -1) n))

;; (ceil (/ x 8)), but avoid clojure/java math weirdness here
(defn needed-bytes [bits]
  (+ (bit-shift-right bits 3)
    (if (= 0 (bit-and bits 3)) 0 1)))

(defn encode-bitstring [bs]
  (let
    [l (count bs)
     nb (needed-bytes l)
     pad-bits (abs (- l (* nb 8)))
     bytes (bitstring2bytes bs)
     content (cons pad-bits bytes)
     len-bytes (length-bs (count content))]
    (concat (identifier class-universal is-primitive tag-bit-string)
      len-bytes content)))

(defn encode-ia5string [str]
  (let [l (count str)]
    (concat
      (identifier class-universal is-primitive tag-ia5string)
      (length-bs l)
      (seq (.getBytes str)))))

(defn encode-printable-string [str]
  (let [l (count str)]
    (concat
      (identifier class-universal is-primitive tag-printable-string)
      (length-bs l)
      (seq (.getBytes str)))))

(defn encode-octet-string [bs]
  (concat
    (identifier class-universal is-primitive tag-octet-string)
    (length-bs (count bs))
    (seq bs)))

(defn encode-sequence [& es]
  (concat
    (identifier class-universal is-constructed tag-sequence)
    (let [bs (apply concat es)]
      (concat
        (length-bs (count bs))
        bs))))

(defn encode-set [& encoded]
  (let [bs (apply concat encoded)]
    (concat
      (identifier class-universal is-constructed tag-set)
      (length-bs (count bs))
      bs)))

(defn lex< [a b]
  (cond
    (empty? a) (not (empty? b))
    (empty? b) false
    (< (first a) (first b)) true
    (= (first a) (first b)) (recur (rest a) (rest b))
    :true false))

;; as encode-set, but order is lexicographic
(defn encode-set-of [& encoded]
  (let [bs (apply concat (sort lex< encoded))]
    (concat
      (identifier class-universal is-constructed tag-set)
      (length-bs (count bs))
      bs)))

(defn encode-explicit [n & es]
  (cons (+ 0xa0 n)
    (let [bs (apply concat es)]
      (concat (length-bs (count bs)) bs))))

(defn encode-utc-time [timestr]
  (concat
    (identifier class-universal is-primitive tag-utc-time)
    (length-bs (count timestr))
    (seq (.getBytes timestr))))

(defn asn1-encode [node]
  (cond
    (vector? node)
      (let [op (first node)]
        (cond
          (= op :sequence)
            (apply encode-sequence
              (map asn1-encode (rest node)))
          (= op :set)
            (apply encode-set (map asn1-encode (rest node)))
          (= op :set-of)
            (apply encode-set-of (map asn1-encode (rest node)))
          (= op :explicit)
            (apply encode-explicit (cons (nth node 1) (map asn1-encode (rest (rest node)))))
          (= op :ia5string)
            (if (= (count node) 2)
              (encode-ia5string (nth node 1))
              (throw (Exception. ":ia5string wants one string element")))
          (= op :printable-string)
            (if (= (count node) 2)
              (encode-printable-string (nth node 1))
              (throw (Exception. ":printable-string wants one string element")))
          (= op :identifier)
            (encode-object-identifier (rest node))
          (= op :encapsulated-octet-string)
            ;; these are just octet strings which happen to have valid content
            (if (= (count node) 2)
              (encode-octet-string 
                (asn1-encode (nth node 1)))
              (throw (Exception. ":encapsulated-octet-string requires one argument (did you want a sequence?)")))
          (= op :encapsulated-bitstring)
            ;; these are just bitstrings which happen to have valid content
            (if (= (count node) 2)
              (encode-bitstring 
                (bytes2bitstring
                  (asn1-encode (nth node 1))))
              (throw (Exception. ":encapsulated-bitstring requires one argument (did you want a sequence?)")))
          (= op :utctime)
            (if (= (count node) 2)
              (encode-utc-time (nth node 1))
              (throw (Exception. ":utctime wants one string element")))
          :true
            (throw (Exception. "Unknown ASN.1 operator"))))
    (integer? node)
      (encode-integer node)
    (string? node)
      (encode-printable-string node)
    (= node :null)
      encode-null
    (= node ())
      encode-null
    :true
      (throw (Exception. "Unknown ASN.1 encoder node type: " node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;;;
;;; ASN.1 (DER) decoding
;;;

(defn parse-bignum [lst]
   (loop [out 0 this (first lst) lst (rest lst)]
      (cond
         (nil? this)
            (vector false "end of input" lst)
         (= 128 (bit-and this 128))
            ;; 7 more bits in this byte
            (recur 
               (bit-or (bit-shift-left out 7) (bit-and this 127))
               (first lst) (rest lst))
         :true
            (vector true (bit-or (bit-shift-left out 7) this) lst))))

;; parsers are lst → ok/bool value/reason rest-of-input
(defn parse-identifier [tag tail]
   (let
      [class (bit-shift-right tag 6)
       consp (bit-and (bit-shift-right tag 5) 1)
       tagnum (bit-and tag 31)]
      (if (= tagnum 31)
         (let [[ok tagnum tailp] (parse-bignum tail)]
            (if ok 
               (vector true class consp tagnum tailp)
               (vector false (list "bad bignum: " tagnum) tail)))
         (vector true class consp tagnum tail))))

(defn read-bytes [bs count]
   (loop [bs bs count count out 0]
      (if (= count 0)
         (vector true out bs)
         (let [hd (first bs)]
            (if hd
               (recur (rest bs) (- count 1) (bit-or (bit-shift-left out 8) hd))
               (vector false "out of data" bs))))))
         
(defn parse-length [bs]
   (let [n (first bs)]
      (cond
         (not n)
            (vector false "out of data" bs)
         (< n 128)
            (vector true n (rest bs))
         :true
            (let [count (- n 128)]
               (read-bytes bs count)))))

(defn parse-integer [bs]
   (let
      [[ok nb bs] (parse-length bs)]
      (if ok
         (read-bytes bs nb)
         (vector false (str "failed to get integer size: " nb) bs))))

(defn decode [bs]
    (let [tag (first bs)]
      (if tag
         (let [[ok class consp tagnum bs] (parse-identifier tag (rest bs))]
            (if (= consp 0)
               (if ok
                  (cond
                     (= tagnum tag-integer)
                        ;; permissive: assumed universal
                        (parse-integer bs)
                     :true
                        (vector false (str "Unknown identifier tag: " tagnum) bs))
                  (vector false (str "Failed to read identifier: " class) bs))
               (vector false "constructed incoding not allowed in DER" bs)))
         (vector false "no input" bs))))

(defn asn1-decode [bs]
   (let [[ok value bs] (decode bs)]
      (if ok
         (do
            (if (not (empty? bs))
               (println "Warning: " (count bs) " bytes of trailing garbage ignored after ASN.1 decoding"))
            value)
         (do
            (println "ERROR: ASN.1 decoding failed: " value)
            nil))))

;; for testing only
(defn asn1-rencode [ast]
   (let
      [bs (asn1-encode ast)
       astp (asn1-decode bs)]
      (if (= ast astp)
         true
         (do
            (println "IN:  " ast)
            (println "OUT: " astp)
            (println "ENCODED: " bs)
            false))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;;;
;;; ASN.1 AST utils
;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;;;
;;; Quoted-printable
;;;

(defn hexval [a]
  "ascii value -> integer (denoting 4 bits) | nil"
  (cond
    (< 47 a 58)   (- a 48) ;; 0-9
    (< 96 a 103)  (- a 87) ;; a-z
    (< 64 a 71)   (- a 55) ;; A-Z
    :else nil))

(defn quoted-printable-decode
  "decode quoted printable encoding in character sequence, nil if invalid data"
  [data]
  (loop [data data out ()]
    (let [[c & data] data]
      (cond
        (= c \=)
          (let [[a b & data] data]
            (cond
              (nil? b)
                nil
              (and (= a \return) (= b \newline))
                (recur data out)
              :else
                (let [na (hexval (int a)) nb (hexval (int b))]
                  (if (and na nb)
                    (recur data
                      (cons (char (bit-or (bit-shift-left na 4) nb)) out))
                    (do
                      ;(println "Invalid quoted printable: '=" a " " b "' = " (int a) ", " (int b) " -> " (list na nb))
                      ;(println (take 100 data))
                      nil)))))
        (nil? c)
          (reverse out)
        :else
          (recur data (cons c out))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;;;
;;; Base64 decoder
;;;

;; (int \A) = 65, (char 65) = \A

;; 0-25 = A-Z
;; 26-51 = a-z
;; 52-61 = 0-9
;; 62 = +
;; 63 = /
;; __ = =

(defn base64-value [c]
  (let [c (int c)]
    (cond
      (and (<= 48 c) (< c 58))
        (+ c 4) ;; 0-9, 52 + (c - 48) = c + 4
      (and (<= 65 c) (< c 91))
        (- c 65) ;; A-Z
      (and (<= 97 c) (< c 123))
        (- c 71) ;; a-z, 26 + (c - 97) = c - 71
      (= c 43)
        62 ; +
      (= c 47)
        63 ; /
      (= c 61)
        :end
      (or (= c 10) (= c 13))
        :skip ;; = \r \n
      :else
        (do
          (println "Invalid byte in base64 decoding: " c)
          :bad))))

(defn base64-finish [val state out]
  (cond
    (= val 0)
      (apply str (map char (reverse out)))
    :else
      nil))

;            0     1    2     3    decoder states
; bits    |----||----||----||----| from base64 values
;         |      ||      ||      | 
; output  '------''------''------' to output
;            0        1      2     encoder states

(defn base64-decode [data]
  (loop
    [data (map base64-value data)
     state 0
     val 0
     out ()]
    (let [[v & data] data]
      (cond
        (= v :skip)
          (recur data state val out)
        (= v :bad)
          nil
        (= v :end)
          (base64-finish val state out)
        (nil? v)
          (base64-finish val state out)
        (= state 0)
          (recur data 1 (bit-shift-left v 2) out)
        (= state 1)
          (let [lo2 (bit-shift-right v 4)
                hi4 (bit-and v 15)]
            (recur data 2
              (bit-shift-left hi4 4)
              (cons (bit-or val lo2) out)))
        (= state 2)
          (let [lo4 (bit-shift-right v 2)
                hi2 (bit-and v 3)]
            (recur data 3
              (bit-shift-left hi2 6)
              (cons (bit-or val lo4) out)))
        :else
          (recur data 0 0
            (cons (bit-or val v) out))))))

(defn base64-digit [b]
  (if (== b (bit-and b 63))
    (char
      (cond
        (< b 26) (+ b 65)
        (< b 52) (- b 70)
        (< b 62) (+ b 4)
        (= b 62) 43
        :else 47))
    (throw
      (Exception. "invalid input to base64 encode"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
