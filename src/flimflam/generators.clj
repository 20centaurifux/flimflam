(ns flimflam.generators
  (:require [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [miner.strgen :as sg]))

(defn- strjoin
  [separator & gx]
  (gen/fmap (fn [x]
              (if (coll? x)
                (str/join separator (flatten x))
                (str x)))
            (apply gen/tuple gx)))

(defn- strconcat
  [& gx]
  (apply strjoin (cons "" gx)))

(def dns-label (sg/string-generator "[a-zA-Z]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"))

(def fqdn (strjoin "." (gen/vector dns-label 2 4)))

(def ipv4 (strjoin "." (gen/vector (gen/choose 0 255) 4)))

(def hextet (gen/fmap #(format "%04x" %)
                      (gen/choose 0 65535)))

(defn ipv6-xxxxs
  [& {:keys [hextets] :or {hextets 8}}]
  (strjoin ":" (gen/vector hextet hextets)))

(defn ipv6-0000-xxxxs
  [& {:keys [max-hextets] :or {max-hextets 7}}]
  (strjoin ":"
           (gen/return ":")
           (gen/vector hextet 1 max-hextets)))

(defn ipv6-xxxxs-0000
  [& {:keys [max-hextets] :or {max-hextets 7}}]
  (strjoin ":"
           (gen/vector hextet 1 max-hextets)
           (gen/return ":")))

(defn ipv6-xxxxs-0000-xxxxs
  [& {:keys [max-hextets] :or {max-hextets 7}}]
  (gen/fmap (fn [v]
              (let [idx (rand-int (dec (count v)))
                    [l r] (split-at (inc idx) v)]
                (str (str/join ":" l) "::" (str/join ":" r))))
            (gen/vector hextet 2 max-hextets)))

(def ipv6-xxxxs-octet
  (strconcat (gen/one-of [(gen/tuple (ipv6-xxxxs :hextets 6)
                                     (gen/return ":")
                                     ipv4)
                          (gen/tuple (ipv6-0000-xxxxs :max-hextets 5)
                                     (gen/return ":")
                                     ipv4)
                          (gen/tuple (ipv6-xxxxs-0000 :max-hextets 5)
                                     ipv4)
                          (gen/tuple (gen/return "::")
                                     ipv4)])))

(def ipv6 (gen/one-of [(ipv6-xxxxs)
                       (ipv6-xxxxs-0000)
                       (ipv6-0000-xxxxs)
                       (ipv6-xxxxs-0000-xxxxs)
                       ipv6-xxxxs-octet
                       (gen/return "::")]))

(def FWS (gen/bind (gen/elements ["([\t ]*\r\n)?[\t ]+"
                                  "[\t ]+(\r\n[\t ]+)*"])
                   sg/string-generator))

(def FWS-optional (gen/one-of [FWS (gen/return "")]))

(defn- choose-char
  ([lower upper]
   (gen/fmap char
             (gen/choose (int lower) (int upper))))
  ([c]
   (gen/return c)))

(def ctext (strconcat (gen/vector (gen/one-of [(choose-char \! \')
                                               (choose-char \* \[)
                                               (choose-char \] \~)
                                               (choose-char 0x01 0x08)
                                               (choose-char 0x0b 0x0c)
                                               (choose-char 0x0e 0x1f)
                                               (choose-char 0x7f)])
                                  1
                                  20)))

(def quoted-pair (gen/fmap #(format "\\%c" (char %))
                           (gen/choose 0 127)))

(defn- comment'
  [& gx]
  (gen/tuple (gen/return \()
             (gen/vector (gen/tuple FWS-optional (gen/one-of gx)) 1 5)
             FWS-optional
             (gen/return \))))

(def comment (strconcat (gen/one-of [(comment' ctext quoted-pair)
                                     (comment' (comment' ctext quoted-pair))])))

(def CFWS (strconcat (gen/one-of [FWS
                                  (gen/tuple (gen/vector (gen/tuple FWS-optional comment) 1 5)
                                             FWS-optional)])))

(def atext (strconcat (gen/vector (gen/one-of [(choose-char \!)
                                               (choose-char \# \')
                                               (choose-char \* \+)
                                               (choose-char \-)
                                               (choose-char \/ \9)
                                               (choose-char \=)
                                               (choose-char \?)
                                               (choose-char \A \Z)
                                               (choose-char \a \z)])
                                  1
                                  20)))

(def dot-atom-text (strconcat atext
                              (gen/vector (gen/tuple (gen/return ".") atext) 0 5)))

(def qtext (strconcat (gen/vector (gen/one-of [(choose-char 0x0001 0x0008)
                                               (choose-char 0x000b 0x000c)
                                               (choose-char 0x000e 0x001f)
                                               (choose-char 0x007f)
                                               (choose-char \!)
                                               (choose-char \# \[)
                                               (choose-char \] \~)])
                                  1
                                  20)))

(def quoted-string (strconcat (gen/return "\"")
                              (gen/vector (gen/tuple FWS-optional
                                                     (gen/one-of [qtext quoted-pair]))
                                          0
                                          5)
                              FWS-optional
                              (gen/return "\"")))

(def word (strconcat (gen/one-of [(gen/tuple quoted-string)
                                  (gen/tuple atext)
                                  (gen/tuple CFWS atext)
                                  (gen/tuple atext CFWS)
                                  (gen/tuple CFWS atext CFWS)])))

(def obs-local-part (strconcat word
                               (gen/vector (gen/tuple (gen/return ".") word) 1 5)))
