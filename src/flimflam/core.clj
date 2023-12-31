(ns flimflam.core
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

;;;;  parser

(def ^:private parser
  (insta/parser
   "address = local-part '@' domain
    FWS = #'[ \t]'+
    CFWS = ((FWS? comment)+ FWS?) | FWS
    comment = '(' (FWS? ccontent)* FWS? ')'
    ccontent = ctext | quoted-pair | comment
    ctext = #'[\\u0001-\\u0008\\u000b\\u000c\\u000e-\\u001f\\u0021-\\u0027\\u002a-\\u005b\\u005d-\\u007f]+'
    local-part = quoted-string | obs-local-part
    quoted-string = CFWS? '\"' (FWS? qcontent)* FWS? '\"' CFWS?
    qcontent = qtext | quoted-pair
    qtext = #'[\\u0001-\\u0008\\u000b\\u000c\\u000e-\\u001f\\u0021\\u0023-\\u005b\\u005d-\\u007f]+'
    obs-local-part = word ('.' word)*
    word = atom | quoted-string
    atom = CFWS? atext CFWS?
    atext = #'[\\u0021\\u0023-\\u0027\\u002a\\u002b\\u002d\\u002f-\\u0039\\u003d\\u003f\\u0041-\\u005a\\u005e-\\u007e]+'
    quoted-pair = #'\\u005c([\\u0000-\\u007f])'
    domain = CFWS? (hostname | fqdn | ip) CFWS?
    hostname = label
    fqdn = (label '.')+ label?
    label = #'(?i)[a-z]([a-z0-9-]{0,61}[a-z0-9])?'
    ip = '[' FWS? (v4 | 'IPv6:' v6) FWS? ']'
    v4 = octet '.' octet '.' octet '.' octet
    v680 = hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet
    v670 = '::' hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet
    v671 = hextet '::' hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet
    v672 = hextet ':' hextet '::' hextet ':' hextet ':' hextet ':' hextet ':' hextet
    v673 = hextet ':' hextet ':' hextet '::' hextet ':' hextet ':' hextet ':' hextet
    v674 = hextet ':' hextet ':' hextet ':' hextet '::' hextet ':' hextet ':' hextet
    v675 = hextet ':' hextet ':' hextet ':' hextet ':' hextet '::' hextet ':' hextet
    v676 = hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet '::' hextet
    v677 = hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet '::'
    v660 = '::' hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet
    v661 = hextet '::' hextet ':' hextet ':' hextet ':' hextet ':' hextet
    v662 = hextet ':' hextet '::' hextet ':' hextet ':' hextet ':' hextet
    v663 = hextet ':' hextet ':' hextet '::' hextet ':' hextet ':' hextet
    v664 = hextet ':' hextet ':' hextet ':' hextet '::' hextet ':' hextet
    v665 = hextet ':' hextet ':' hextet ':' hextet ':' hextet '::' hextet
    v666 = hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet '::'
    v650 = '::' hextet ':' hextet ':' hextet ':' hextet ':' hextet
    v651 = hextet '::' hextet ':' hextet ':' hextet ':' hextet
    v652 = hextet ':' hextet '::' hextet ':' hextet ':' hextet
    v653 = hextet ':' hextet ':' hextet '::' hextet ':' hextet
    v654 = hextet ':' hextet ':' hextet ':' hextet '::' hextet
    v655 = hextet ':' hextet ':' hextet ':' hextet ':' hextet '::'
    v640 = '::' hextet ':' hextet ':' hextet ':' hextet
    v641 = hextet '::' hextet ':' hextet ':' hextet
    v642 = hextet ':' hextet '::' hextet ':' hextet
    v643 = hextet ':' hextet ':' hextet '::' hextet
    v644 = hextet ':' hextet ':' hextet ':' hextet '::'
    v630 = '::' hextet ':' hextet ':' hextet
    v631 = hextet '::' hextet ':' hextet
    v632 = hextet ':' hextet '::' hextet
    v633 = hextet ':' hextet ':' hextet '::'
    v620 = '::' hextet ':' hextet
    v621 = hextet '::' hextet
    v622 = hextet ':' hextet '::'
    v610 = '::' hextet
    v611 = hextet '::'
    v600 = '::'
    v6604 = hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6504 = '::' hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6514 = hextet '::' hextet ':' hextet ':' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6524 = hextet ':' hextet '::' hextet ':' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6534 = hextet ':' hextet ':' hextet '::' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6544 = hextet ':' hextet ':' hextet ':' hextet '::' hextet ':' octet '.' octet '.' octet '.' octet
    v6554 = hextet ':' hextet ':' hextet ':' hextet ':' hextet '::' octet '.' octet '.' octet '.' octet
    v6404 = '::' hextet ':' hextet ':' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6414 = hextet '::' hextet ':' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6424 = hextet ':' hextet '::' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6434 = hextet ':' hextet ':' hextet '::' hextet ':' octet '.' octet '.' octet '.' octet
    v6444 = hextet ':' hextet ':' hextet ':' hextet '::' octet '.' octet '.' octet '.' octet
    v6304 = '::' hextet ':' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6314 = hextet '::' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6324 = hextet ':' hextet '::' hextet ':' octet '.' octet '.' octet '.' octet
    v6334 = hextet ':' hextet ':' hextet '::' octet '.' octet '.' octet '.' octet
    v6204 = '::' hextet ':' hextet ':' octet '.' octet '.' octet '.' octet
    v6214 = hextet '::' hextet ':' octet '.' octet '.' octet '.' octet
    v6224 = hextet ':' hextet '::' octet '.' octet '.' octet '.' octet
    v6104 = '::' hextet ':' octet '.' octet '.' octet '.' octet
    v6114 = hextet '::' octet '.' octet '.' octet '.' octet
    v6004 = '::' octet '.' octet '.' octet '.' octet
    v6 = (v680
          | v670 | v671 | v672 | v673 | v674 | v675 | v676 | v677
          | v660 | v661 | v662 | v663 | v664 | v665 | v666
          | v650 | v651 | v652 | v653 | v654 | v655
          | v640 | v641 | v642 | v643 | v644
          | v630 | v631 | v632 | v633
          | v620 | v621 | v622
          | v610 | v611
          | v600
          | v6604
          | v6504 | v6514 | v6524 | v6534 | v6544 | v6554
          | v6404 | v6414 | v6424 | v6434 | v6444
          | v6304 | v6314 | v6324 | v6334
          | v6204 | v6214 | v6224
          | v6104 | v6114
          | v6004)
    hextet = #'[a-fA-F0-9]{1,4}'
    octet = #'(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)'"))

(defn- unfold
  [s]
  (str/replace s #"\r\n\s+" " "))

(defn- trim-addr-spec-parts
  [email]
  (when-let [idx (str/last-index-of email "@")]
    (str (str/trim (subs email 0 idx))
         (str/trim (subs email idx)))))

(defn- parse
  [email]
  (some-> email
          unfold
          trim-addr-spec-parts
          parser))

;;;; validation

(defn invalid?
  "Returns true `email` is an invalid email address."
  [email]
  (-> email
      parse
      insta/failure?))

(defn valid?
  "Returns true if `email` is a valid email address."
  [email]
  (not (invalid? email)))

;;;; normalization

;;; normalize local-part

(defn- unquote-string
  [& r]
  (let [s (apply str r)
        l (str/index-of s "\"")
        r (str/last-index-of s "\"")]
    (str (subs s 0 l)
         (subs s (inc l) r)
         (subs s (inc r)))))

(defn- local-part->str
  [& r]
  (->> r
       (insta/transform {:obs-local-part str
                         :FWS str
                         :CFWS str
                         :comment (constantly "")
                         :word str
                         :atom str
                         :atext str
                         :quoted-string unquote-string
                         :qcontent str
                         :qtext str
                         :quoted-pair #(subs % 1)})
       str/join))

;;; normalize domain

(defn- fqdn->str
  [& r]
  (str/replace (apply str r) #"\.$" ""))

(defn- flat-ipv6-bytes-with-double-colon
  [coll]
  (filter #(or (int? %) (= "::" %))
          (flatten coll)))

(defn- substitute-double-colon
  [bytes]
  (let [[l [_ & r]] (split-with #(not= "::" %) bytes)]
    (concat l (repeat (- 16 (count l) (count r)) 0) r)))

(defn- bytes->hextets
  [bytes]
  (map (fn [[msb lsb]]
         (format "%02x%02x" msb lsb))
       (partition 2 bytes)))

(defn- substitute-consecutive-zeros
  [hextets]
  (when-not (empty? hextets)
    (let [[zeros r] (split-with #(= "0000" %) hextets)]
      (cond
        (empty? zeros) (cons (first r) (substitute-consecutive-zeros (next r)))
        (and (> (count zeros) 1)
             (not-any? #(> (count %) (count zeros))
                       (filter #(apply = "0000" %)
                               (partition-by #(= "0000" %) r)))) (cons "::" r)
        :else (concat zeros (substitute-consecutive-zeros r))))))

(defn- interpose-hextets
  [[hextet & r]]
  (when hextet
    (if (or (nil? r)
            (= hextet "::")
            (= (first r) "::"))
      (cons hextet (interpose-hextets r))
      (concat [hextet ":"] (interpose-hextets r)))))

(defn- ipv6->str
  [[_ & r]]
  (-> r
      flat-ipv6-bytes-with-double-colon
      substitute-double-colon
      bytes->hextets
      substitute-consecutive-zeros
      interpose-hextets
      str/join))

(defn- hextet->bytes
  [s]
  (let [word (Integer/parseInt s 16)]
    [(bit-shift-right word 8)
     (bit-and word 255)]))

(defn- domain->str
  [& r]
  (->> r
       (insta/transform {:CFWS (constantly "")
                         :FWS (constantly "")
                         :hostname str
                         :fqdn fqdn->str
                         :label str/lower-case
                         :ip str
                         :v4 str
                         :v6 ipv6->str
                         :octet parse-long
                         :hextet hextet->bytes})
       str/join))

;;; normalize addr-spec

(defn- quote?
  [s]
  (or (empty? s)
      (re-find #"\p{C}|\s|\"|@" s)))

(defn- escape-char
  [c]
  (let [n (int c)]
    (if (or (< n 8)
            (= n 11)
            (< 13 n 32)
            (= n 127))
      (format "\\u%04x" n)
      (char-escape-string c))))

(defn- escape
  [s]
  (if (quote? s)
    (str \" (str/escape s escape-char) \")
    s))

(defn- address->str
  [local-part _ domain]
  (str (escape local-part) \@ domain))

(defn normalize
  "Converts `email` to a uniform format."
  [email]
  (let [result (parse email)]
    (when-not (insta/failure? result)
      (insta/transform {:address address->str
                        :local-part local-part->str
                        :domain domain->str}
                       result))))
