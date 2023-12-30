(ns flimflam.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.results :refer [pass?]]
            [flimflam.core :as ff]
            [flimflam.generators :as ffg]
            [instaparse.core :as insta]))

(def ^:private num-tests 200)

;;;; local-part validation

;;; dot-atom

(defspec $dot-atom-text-at-example-org-valid? num-tests
  (prop/for-all [dot-atom-text ffg/dot-atom-text]
    (ff/valid? (format "%s@example.org" dot-atom-text))))

(defspec $CFWS-$dot-atom-text-at-example-org-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 dot-atom-text ffg/dot-atom-text]
    (ff/valid? (format "%s%s@example.org" cfws dot-atom-text))))

(defspec $dot-atom-text-$CFWS-at-example-org-valid? num-tests
  (prop/for-all [dot-atom-text ffg/dot-atom-text
                 cfws ffg/CFWS]
    (ff/valid? (format "%s%s@example.org" dot-atom-text cfws))))

(defspec $CFWS-$dot-atom-text-$CFWS-at-example-org-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 dot-atom-text ffg/dot-atom-text
                 cfws' ffg/CFWS]
    (ff/valid? (format "%s%s%s@example.org" cfws dot-atom-text cfws'))))

;;; quoted string

(defspec $quoted-string-at-example-org-valid? num-tests
  (prop/for-all [quoted-string ffg/quoted-string]
    (ff/valid? (format "%s@example.org" quoted-string))))

(defspec $quoted-string-at-example-org-valid? num-tests
  (prop/for-all [quoted-string ffg/quoted-string]
    (ff/valid? (format "%s@example.org" quoted-string))))

(defspec $CFWS-$quoted-string-at-example-org-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 quoted-string ffg/quoted-string]
    (ff/valid? (format "%s%s@example.org" cfws quoted-string))))

(defspec $CFWS-$quoted-string-at-example-org-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 quoted-string ffg/quoted-string]
    (ff/valid? (format "%s%s@example.org" cfws quoted-string))))

(defspec $quoted-string-$CFWS-at-example-org-valid? num-tests
  (prop/for-all [quoted-string ffg/quoted-string
                 cfws ffg/CFWS]
    (ff/valid? (format "%s%s@example.org" quoted-string cfws))))

(defspec $quoted-string-$CFWS-at-example-org-valid? num-tests
  (prop/for-all [quoted-string ffg/quoted-string
                 cfws ffg/CFWS]
    (ff/valid? (format "%s%s@example.org" quoted-string cfws))))

(defspec $CFWS-$quoted-string-$CFWS-at-example-org-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 quoted-string ffg/quoted-string
                 cfws' ffg/CFWS]
    (ff/valid? (format "%s%s%s@example.org" cfws quoted-string cfws'))))

(defspec $CFWS-$quoted-string-$CFWS-at-example-org-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 quoted-string ffg/quoted-string
                 cfws' ffg/CFWS]
    (ff/valid? (format "%s%s%s@example.org" cfws quoted-string cfws'))))

;;; obs-local-part (generator includes CFWS)

(defspec $obs-local-part-at-example-org-valid? num-tests
  (prop/for-all [obs-local-part ffg/obs-local-part]
    (ff/valid? (format "%s@example.org" obs-local-part))))

;;;; domain validation

;;; hostname

(defspec noreply-at-$dns-label-valid? num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/valid? (format "noreply@%s" dns-label))))

(defspec noreply-at-hyphen-$dns-label-invalid? num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/invalid? (format "noreply@-%s" dns-label))))

(defspec noreply-at-digit-$dns-label-invalid? num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/invalid? (format "noreply@1%s" dns-label))))

(defspec noreply-at-$dns-label-hyphen-invalid? num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/invalid? (format "noreply@%s-" dns-label))))

(defspec noreply-at-$CFWS-$dns-label-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 dns-label ffg/dns-label]
    (ff/valid? (format "noreply@%s%s" cfws dns-label))))

(defspec noreply-at-$dns-label-$CFWS-valid? num-tests
  (prop/for-all [dns-label ffg/dns-label
                 cfws ffg/CFWS]
    (ff/valid? (format "noreply@%s%s" dns-label cfws))))

(defspec noreply-at-$CFWS-$dns-label-$CFWS-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 dns-label ffg/dns-label
                 cfws' ffg/CFWS]
    (ff/valid? (format "noreply@%s%s%s" cfws dns-label cfws'))))

(defspec noreply-at-bracket-$dns-label-bracket-invalid? num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/invalid? (format "noreply@[%s]" dns-label))))

;;; fqdn

(defspec noreply-at-$fqdn-valid? num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (ff/valid? (format "noreply@%s" fqdn))))

(defspec noreply-at-hyphen-$fqdn-invalid? num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (ff/invalid? (format "noreply@-%s" fqdn))))

(defspec noreply-at-$fqdn-hyphen-invalid? num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (ff/invalid? (format "noreply@%s-" fqdn))))

(defspec noreply-at-$fqdn-period-valid? num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (ff/valid? (format "noreply@%s." fqdn))))

(defspec noreply-at-$CFWS-$fqdn-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 fqdn ffg/fqdn]
    (ff/valid? (format "noreply@%s%s" cfws fqdn))))

(defspec noreply-at-$fqdn-$CFWS-valid? num-tests
  (prop/for-all [fqdn ffg/fqdn
                 cfws ffg/CFWS]
    (ff/valid? (format "noreply@%s%s" fqdn cfws))))

(defspec noreply-at-$CFWS-$fqdn-$CFWS-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 fqdn ffg/fqdn
                 cfws' ffg/CFWS]
    (ff/valid? (format "noreply@%s%s%s" cfws fqdn cfws'))))

(defspec noreply-at-bracket-$fqdn-bracket-invalid? num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (ff/invalid? (format "noreply@[%s]" fqdn))))

;;; IPv4

(defspec noreply-at-$ip4-invalid? num-tests
  (prop/for-all [ip ffg/ipv4]
    (ff/invalid? (format "noreply@%s" ip))))

(defspec noreply-at-bracket-$ip4-invalid? num-tests
  (prop/for-all [ip ffg/ipv4]
    (ff/invalid? (format "noreply@[%s" ip))))

(defspec noreply-at-$ip4-bracket-invalid? num-tests
  (prop/for-all [ip ffg/ipv4]
    (ff/invalid? (format "noreply@%s]" ip))))

(defspec noreply-at-bracket-$ip4-bracket-valid? num-tests
  (prop/for-all [ip ffg/ipv4]
    (ff/valid? (format "noreply@[%s]" ip))))

(defspec noreply-at-$CFWS-bracket-$ip4-bracket-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ffg/ipv4]
    (ff/valid? (format "noreply@%s[%s]" cfws ip))))

(defspec noreply-at-bracket-$ip4-bracket-$CFWS-valid? num-tests
  (prop/for-all [ip ffg/ipv4
                 cfws ffg/CFWS]
    (ff/valid? (format "noreply@[%s]%s" ip cfws))))

(defspec noreply-at-$CFWS-bracket-$ip4-bracket-$CFWS-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ffg/ipv4
                 cfws' ffg/CFWS]
    (ff/valid? (format "noreply@%s[%s]%s" cfws ip cfws'))))

(defspec noreply-at-bracket-$FWS-$ip4-bracket-valid? num-tests
  (prop/for-all [fws ffg/FWS
                 ip ffg/ipv4]
    (ff/valid? (format "noreply@[%s%s]" fws ip))))

(defspec noreply-at-racket-$ip4-$FWS-bracket-valid? num-tests
  (prop/for-all [ip ffg/ipv4
                 fws ffg/FWS]
    (ff/valid? (format "noreply@[%s%s]" ip fws))))

(defspec noreply-at-bracket-$FWS-$ip4-$FWS-bracket-valid? num-tests
  (prop/for-all [fws ffg/FWS
                 ip ffg/ipv4
                 fws' ffg/FWS]
    (ff/valid? (format "noreply@[%s%s%s]" fws ip fws'))))

;;; IPv6

(defspec noreply-at-$ip6-invalid? num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/invalid? (format "noreply@%s" ip))))

(defspec noreply-at-IPv6-$ip6-invalid? num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/invalid? (format "noreply@IPv6:%s" ip))))

(defspec noreply-at-bracket-IPv6-$ip6-invalid? num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/invalid? (format "noreply@[IPv6:%s" ip))))

(defspec noreply-at-IPv6-$ip6-bracket-invalid? num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/invalid? (format "noreply@IPv6:%s]" ip))))

(defspec noreply-at-bracket-$ip6-bracket-invalid? num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/invalid? (format "noreply@[%s]" ip))))

(defspec noreply-at-bracket-IPv6-$ip6-bracket-valid? num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/valid? (format "noreply@[IPv6:%s]" ip))))

(defspec noreply-at-$CFWS-bracket-IPv6-$ip6-bracket-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ffg/ipv6]
    (ff/valid? (format "noreply@%s[IPv6:%s]" cfws ip))))

(defspec noreply-at-bracket-IPv6-$ip6-bracket-$CFWS-valid? num-tests
  (prop/for-all [ip ffg/ipv6
                 cfws ffg/CFWS]
    (ff/valid? (format "noreply@[IPv6:%s]%s" ip cfws))))

(defspec noreply-at-$CFWS-bracket-IPv6-$ip6-bracket-$CFWS-valid? num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ffg/ipv6
                 cfws' ffg/CFWS]
    (ff/valid? (format "noreply@%s[IPv6:%s]%s" cfws ip cfws'))))

(defspec noreply-at-bracket-$FWS-IPv6-$ip6-bracket-valid? num-tests
  (prop/for-all [fws ffg/FWS
                 ip ffg/ipv6]
    (ff/valid? (format "noreply@[%sIPv6:%s]" fws ip))))

(defspec noreply-at-bracket-IPv6-$ip6-$FWS-bracket-valid? num-tests
  (prop/for-all [ip ffg/ipv6
                 fws ffg/FWS]
    (ff/valid? (format "noreply@[IPv6:%s%s]" ip fws))))

(defspec noreply-at-bracket-$FWS-IPv6-$ip6-$FWS-bracket-valid? num-tests
  (prop/for-all [fws ffg/FWS
                 ip ffg/ipv6
                 fws' ffg/FWS]
    (ff/valid? (format "noreply@[%sIPv6:%s%s]" fws ip fws'))))

;;;; local-part normalization

;;; dot-atom-text

(defspec normalize-$dot-atom-text-at-example-org num-tests
  (prop/for-all [dot-atom-text ffg/dot-atom-text]
    (= (format "%s@example.org" dot-atom-text)
       (ff/normalize (format "%s@example.org" dot-atom-text)))))

(defspec normalize-CRLF-WSP-$dot-atom-text-at-example-org num-tests
  (prop/for-all [dot-atom-text ffg/dot-atom-text]
    (= (format "%s@example.org" dot-atom-text)
       (ff/normalize (format "\r\n \t %s@example.org" dot-atom-text)))))

(defspec normalize-WSP-CRLF-WSP-$dot-atom-text-at-example-org num-tests
  (prop/for-all [dot-atom-text ffg/dot-atom-text]
    (= (format "%s@example.org" dot-atom-text)
       (ff/normalize (format " \t \r\n \t %s@example.org" dot-atom-text)))))

(defspec normalize-$comment-$dot-atom-text-at-example-org num-tests
  (prop/for-all [cmnt ffg/comment
                 dot-atom-text ffg/dot-atom-text]
    (= (format "%s@example.org" dot-atom-text)
       (ff/normalize (format "%s%s@example.org" cmnt dot-atom-text)))))

(defspec normalize-$dot-atom-text-CRLF-WSP-at-example-org num-tests
  (prop/for-all [dot-atom-text ffg/dot-atom-text]
    (= (format "%s@example.org" dot-atom-text)
       (ff/normalize (format "%s\r\n \t @example.org" dot-atom-text)))))

(defspec normalize-$dot-atom-text-WSP-CRLF-WSP-at-example-org num-tests
  (prop/for-all [dot-atom-text ffg/dot-atom-text]
    (= (format "%s@example.org" dot-atom-text)
       (ff/normalize (format "%s \t \r\n \t @example.org" dot-atom-text)))))

(defspec normalize-$dot-atom-text-$comment-at-example-org num-tests
  (prop/for-all [dot-atom-text ffg/dot-atom-text
                 cmnt ffg/comment]
    (= (format "%s@example.org" dot-atom-text)
       (ff/normalize (format "%s%s@example.org" dot-atom-text cmnt)))))

;;; quoted-string

(defspec normalize-quote-$alpha-numeric-string-quote-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s@example.org" qtext)
       (ff/normalize (format "\"%s\"@example.org" qtext)))))

(defspec normalize-quote-WSP-$alpha-numeric-string-quote-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\" \\t %s\"@example.org" qtext)
       (ff/normalize (format "\" \t %s\"@example.org" qtext)))))

(defspec normalize-quote-$alpha-numeric-string-WSP-quote-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s \\t \"@example.org" qtext)
       (ff/normalize (format "\"%s \t \"@example.org" qtext)))))

(def ^:private escapable-quoted-string-ctrl-char (gen/elements [\backspace
                                                                \formfeed]))
(defspec normalize-quote-$escapable-ctrl-char-quote-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char]
    (= (format "\"%s\"@example.org" (char-escape-string c))
       (ff/normalize (format "\"%s\"@example.org" c)))))

(defspec normalize-quote-WSP-$escapable-ctrl-char-quote-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char]
    (= (format "\" \\t %s\"@example.org" (char-escape-string c))
       (ff/normalize (format "\" \t %s\"@example.org" c)))))

(defspec normalize-quote-$escapable-ctrl-char-WSP-quote-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char]
    (= (format "\"%s \\t \"@example.org" (char-escape-string c))
       (ff/normalize (format "\"%s \t \"@example.org" c)))))

(def ^:private hex-escapable-quoted-string-ctrl-char
  (gen/fmap char (gen/one-of [(gen/choose 1 7)
                              (gen/return 11)
                              (gen/choose 14 31)
                              (gen/return 127)])))

(defspec normalize-quote-$hex-escapable-ctrl-char-quote-at-example-org num-tests
  (prop/for-all [c hex-escapable-quoted-string-ctrl-char]
    (= (format "\"\\u%04x\"@example.org" (int c))
       (ff/normalize (format "\"%s\"@example.org" c)))))

(defspec normalize-quote-WSP-$hex-escapable-ctrl-char-quote-at-example-org num-tests
  (prop/for-all [c hex-escapable-quoted-string-ctrl-char]
    (= (format "\" \\t \\u%04x\"@example.org" (int c))
       (ff/normalize (format "\" \t %s\"@example.org" c)))))

(defspec normalize-quote-$hex-escapable-ctrl-char-WSP-quote-at-example-org num-tests
  (prop/for-all [c hex-escapable-quoted-string-ctrl-char]
    (= (format "\"\\u%04x \\t \"@example.org" (int c))
       (ff/normalize (format "\"%s \t \"@example.org" c)))))

(def ^:private non-escapable-quoted-pair-visible-char
  (gen/fmap char (gen/one-of [(gen/return 33)
                              (gen/choose 35 63)
                              (gen/choose 65 126)])))

(defspec normalize-quote-$quoted-non-escapable-visible-char-quote-at-example-org num-tests
  (prop/for-all [c non-escapable-quoted-pair-visible-char]
    (= (format "%s@example.org" c)
       (ff/normalize (format "\"\\%s\"@example.org" c)))))

(defspec normalize-quote-WSP-$quoted-non-escapable-visible-char-quote-at-example-org num-tests
  (prop/for-all [c gen/char-alpha-numeric]
    (= (format "\" \\t %s\"@example.org" c)
       (ff/normalize (format "\" \t \\%s\"@example.org" c)))))

(defspec normalize-quote-$quoted-non-escapable-visible-char-WSP-quote-at-example-org num-tests
  (prop/for-all [c gen/char-alpha-numeric]
    (= (format "\"%s \\t \"@example.org" c)
       (ff/normalize (format "\"\\%s \t \"@example.org" c)))))

(deftest normalize-quoted-$quoted-special-char-at-example-org
  (testing "normalize-quote-$quoted-special-char-quote-at-example-org"
    (is (= "\"@\"@example.org"
           (ff/normalize "\"\\@\"@example.org")))
    (is (= "\"\\\"\"@example.org"
           (ff/normalize "\"\\\"\"@example.org"))))
  (testing "normalize-quote-WSP-$quoted-special-char-quote-at-example-org"
    (is (= "\" \\t @\"@example.org"
           (ff/normalize "\" \t \\@\"@example.org")))
    (is (= "\" \\t \\\"\"@example.org"
           (ff/normalize "\" \t \\\"\"@example.org"))))
  (testing "normalize-quote-$quoted-special-char-WSP-quote-at-example-org"
    (is (= "\"@ \\t \"@example.org"
           (ff/normalize "\"\\@ \t \"@example.org")))
    (is (= "\"\\\" \\t \"@example.org"
           (ff/normalize "\"\\\" \t \"@example.org")))))

(def ^:private escapable-quoted-pair-ctrl-char (gen/elements [\backspace
                                                              \tab
                                                              \newline
                                                              \formfeed
                                                              \return]))

(defspec normalize-quote-$quoted-escapable-ctrl-char-quote-at-example-org num-tests
  (prop/for-all [c escapable-quoted-pair-ctrl-char]
    (= (format "\"%s\"@example.org" (char-escape-string c))
       (ff/normalize (format "\"\\%s\"@example.org" c)))))

(defspec normalize-quote-WSP-$quoted-escapable-ctrl-char-quote-at-example-org num-tests
  (prop/for-all [c escapable-quoted-pair-ctrl-char]
    (= (format "\" \\t %s\"@example.org" (char-escape-string c))
       (ff/normalize (format "\" \t \\%s\"@example.org" c)))))

(defspec normalize-quote-$quoted-escapable-ctrl-char-WSP-quote-at-example-org num-tests
  (prop/for-all [c escapable-quoted-pair-ctrl-char]
    (= (format "\"%s \\t \"@example.org" (char-escape-string c))
       (ff/normalize (format "\"\\%s \t \"@example.org" c)))))

(def ^:private hex-escapable-quoted-pair-ctrl-char
  (gen/fmap char (gen/one-of [(gen/choose 0 7)
                              (gen/return 11)
                              (gen/choose 14 31)
                              (gen/return 127)])))

(defspec normalize-quote-$quoted-hex-escapable-ctrl-char-quote-at-example-org num-tests
  (prop/for-all [c hex-escapable-quoted-pair-ctrl-char]
    (= (format "\"\\u%04x\"@example.org" (int c))
       (ff/normalize (format "\"\\%s\"@example.org" c)))))

(defspec normalize-quote-WSP-$quoted-hex-escapable-ctrl-char-quote-at-example-org num-tests
  (prop/for-all [c hex-escapable-quoted-pair-ctrl-char]
    (= (format "\" \\t \\u%04x\"@example.org" (int c))
       (ff/normalize (format "\" \t \\%s\"@example.org" c)))))

(defspec normalize-quote-$quoted-hex-escapable-ctrl-char-WSP-quote-at-example-org num-tests
  (prop/for-all [c hex-escapable-quoted-pair-ctrl-char]
    (= (format "\"\\u%04x \\t \"@example.org" (int c))
       (ff/normalize (format "\"\\%s \t \"@example.org" c)))))

(defspec normalize-CRLF-WSP-$quoted-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s@example.org" qtext)
       (ff/normalize (format "\r\n \t \"%s\"@example.org" qtext)))))

(defspec normalize-WSP-CRLF-WSP-$quoted-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s@example.org" qtext)
       (ff/normalize (format " \t \r\n \t \"%s\"@example.org" qtext)))))

(defspec normalize-$comment-$quoted-string-at-example-org num-tests
  (prop/for-all [cmnt ffg/comment
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s@example.org" qtext)
       (ff/normalize (format "%s\"%s\"@example.org" cmnt qtext)))))

(defspec normalize-$quoted-string-CRLF-WSP-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s@example.org" qtext)
       (ff/normalize (format "\"%s\"\r\n \t @example.org" qtext)))))

(defspec normalize-$quoted-string-WSP-CRLF-WSP-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s@example.org" qtext)
       (ff/normalize (format "\"%s\" \t \r\n \t @example.org" qtext)))))

(defspec normalize-$quoted-string-$comment-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 cmnt ffg/comment]
    (= (format "%s@example.org" qtext)
       (ff/normalize (format "\"%s\"%s@example.org" qtext cmnt)))))

;;; obs-local-part (atext.atext)

(defspec normalize-$atext-dot-$atext-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 atext' ffg/atext]
    (= (format "%s.%s@example.org" atext atext')
       (ff/normalize (format "%s.%s@example.org" atext atext')))))

(defspec normalize-CRLF-WSP-$atext-dot-$atext-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 atext' ffg/atext]
    (= (format "%s.%s@example.org" atext atext')
       (ff/normalize (format "\r\n \t %s.%s@example.org" atext atext')))))

(defspec normalize-WSP-CRLF-WSP-$atext-dot-$atext-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 atext' ffg/atext]
    (= (format "%s.%s@example.org" atext atext')
       (ff/normalize (format " \t \r\n \t %s.%s@example.org" atext atext')))))

(defspec normalize-$comment-$atext-dot-$atext-at-example-org num-tests
  (prop/for-all [cmnt ffg/comment
                 atext ffg/atext
                 atext' ffg/atext]
    (= (format "%s.%s@example.org" atext atext')
       (ff/normalize (format "%s%s.%s@example.org" cmnt atext atext')))))

(defspec normalize-$atext-CRLF-WSP-dot-$atext-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 atext' ffg/atext]
    (= (format "\"%s .%s\"@example.org" atext atext')
       (ff/normalize (format "%s\r\n .%s@example.org" atext atext')))))

(defspec normalize-$atext-WSP-CRLF-WSP-dot-$atext-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 atext' ffg/atext]
    (= (format "\"%s \\t  .%s\"@example.org" atext atext')
       (ff/normalize (format "%s \t \r\n \t .%s@example.org" atext atext')))))

(defspec normalize-$atext-$comment-dot-$atext-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 cmnt ffg/comment
                 atext' ffg/atext]
    (= (format "%s.%s@example.org" atext atext')
       (ff/normalize (format "%s%s.%s@example.org" atext cmnt atext')))))

(defspec normalize-$atext-dot-CRLF-WSP-$atext-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 atext' ffg/atext]
    (= (format "\"%s. %s\"@example.org" atext atext')
       (ff/normalize (format "%s.\r\n \t %s@example.org" atext atext')))))

(defspec normalize-$atext-dot-WSP-CRLF-WSP-$atext-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 atext' ffg/atext]
    (= (format "\"%s. \\t  %s\"@example.org" atext atext')
       (ff/normalize (format "%s. \t \r\n \t %s@example.org" atext atext')))))

(defspec normalize-$atext-dot-$comment-$atext-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 cmnt ffg/comment
                 atext' ffg/atext]
    (= (format "%s.%s@example.org" atext atext')
       (ff/normalize (format "%s.%s%s@example.org" atext cmnt atext')))))

(defspec normalize-$atext-dot-$atext-CRLF-WSP-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 atext' ffg/atext]
    (= (format "%s.%s@example.org" atext atext')
       (ff/normalize (format "%s.%s\r\n \t @example.org" atext atext')))))

(defspec normalize-$atext-dot-$atext-WSP-CRLF-WSP-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 atext' ffg/atext]
    (= (format "%s.%s@example.org" atext atext')
       (ff/normalize (format "%s.%s \t \r\n \t @example.org" atext atext')))))

(defspec normalize-$atext-dot-$atext-$comment-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 atext' ffg/atext
                 cmnt ffg/comment]
    (= (format "%s.%s@example.org" atext atext')
       (ff/normalize (format "%s.%s%s@example.org" atext atext' cmnt)))))

;;; obs-local-part (quoted-alpha-numeric-text.quoted-alpha-numeric-text)

(defspec normalize-$quoted-alpha-numeric-string-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" qtext qtext')
       (ff/normalize (format "\"%s\".\"%s\"@example.org" qtext qtext')))))

(defspec normalize-CRLF-WSP-$quoted-alpha-numeric-string-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" qtext qtext')
       (ff/normalize (format "\r\n \t \"%s\".\"%s\"@example.org" qtext qtext')))))

(defspec normalize-WSP-CRLF-WSP-$quoted-alpha-numeric-string-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" qtext qtext')
       (ff/normalize (format " \t \r\n \t \"%s\".\"%s\"@example.org" qtext qtext')))))

(defspec normalize-$comment-$quoted-alpha-numeric-string-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [cmnt ffg/comment
                 qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" qtext qtext')
       (ff/normalize (format "%s\"%s\".\"%s\"@example.org" cmnt qtext qtext')))))

(defspec normalize-$quoted-alpha-numeric-string-CRLF-WSP-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s .%s\"@example.org" qtext qtext')
       (ff/normalize (format "\"%s\"\r\n \t .\"%s\"@example.org" qtext qtext')))))

(defspec normalize-$quoted-alpha-numeric-string-WSP-CRLF-WSP-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s \\t  .%s\"@example.org" qtext qtext')
       (ff/normalize (format "\"%s\" \t \r\n \t .\"%s\"@example.org" qtext qtext')))))

(defspec normalize-$quoted-alpha-numeric-string-$comment-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 cmnt ffg/comment
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" qtext qtext')
       (ff/normalize (format "\"%s\"%s.\"%s\"@example.org" qtext cmnt qtext')))))

(defspec normalize-$quoted-alpha-numeric-string-dot-CRLF-WSP-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s. %s\"@example.org" qtext qtext')
       (ff/normalize (format "\"%s\".\r\n \t \"%s\"@example.org" qtext qtext')))))

(defspec normalize-$quoted-alpha-numeric-string-dot-WSP-CRLF-WSP-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s. \\t  %s\"@example.org" qtext qtext')
       (ff/normalize (format "\"%s\". \t \r\n \t \"%s\"@example.org" qtext qtext')))))

(defspec normalize-$quoted-alpha-numeric-string-dot-$comment-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 cmnt ffg/comment
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" qtext qtext')
       (ff/normalize (format "\"%s\".%s\"%s\"@example.org" qtext cmnt qtext')))))

(defspec normalize-$quoted-alpha-numeric-string-dot-$quoted-alpha-numeric-string-CRLF-WSP-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" qtext qtext')
       (ff/normalize (format "\"%s\".\"%s\"\r\n \t @example.org" qtext qtext')))))

(defspec normalize-$quoted-alpha-numeric-string-dot-$quoted-alpha-numeric-string-WSP-CRLF-WSP-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" qtext qtext')
       (ff/normalize (format "\"%s\".\"%s\" \t \r\n \t @example.org" qtext qtext')))))

(defspec normalize-$quoted-alpha-numeric-string-dot-$quoted-alpha-numeric-string-$comment-at-example-org num-tests
  (prop/for-all [qtext (gen/not-empty gen/string-alpha-numeric)
                 qtext' (gen/not-empty gen/string-alpha-numeric)
                 cmnt ffg/comment]
    (= (format "%s.%s@example.org" qtext qtext')
       (ff/normalize (format "\"%s\".\"%s\"%s@example.org" qtext qtext' cmnt)))))

;;; obs-local-part (quoted-escapable-text.quoted-alpha-numeric-text)

(defspec normalize-$quoted-escapable-string-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s.%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\"%s\".\"%s\"@example.org" c qtext)))))

(defspec normalize-CRLF-WSP-$quoted-escapable-string-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s.%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\r\n \t \"%s\".\"%s\"@example.org" c qtext)))))

(defspec normalize-WSP-CRLF-WSP-$quoted-escapable-string-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s.%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format " \t \r\n \t \"%s\".\"%s\"@example.org" c qtext)))))

(defspec normalize-$comment-$quoted-escapable-string-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [cmnt ffg/comment
                 c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s.%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "%s\"%s\".\"%s\"@example.org" cmnt c qtext)))))

(defspec normalize-$quoted-escapable-string-CRLF-WSP-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s .%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\"%s\"\r\n \t .\"%s\"@example.org" c qtext)))))

(defspec normalize-$quoted-escapable-string-WSP-CRLF-WSP-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s \\t  .%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\"%s\" \t \r\n \t .\"%s\"@example.org" c qtext)))))

(defspec normalize-$quoted-escapable-string-$comment-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 cmnt ffg/comment
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s.%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\"%s\"%s.\"%s\"@example.org" c cmnt qtext)))))

(defspec normalize-$quoted-escapable-string-dot-CRLF-WSP-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s. %s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\"%s\".\r\n \t \"%s\"@example.org" c qtext)))))

(defspec normalize-$quoted-escapable-string-dot-WSP-CRLF-WSP-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s. \\t  %s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\"%s\". \t \r\n \t \"%s\"@example.org" c qtext)))))

(defspec normalize-$quoted-escapable-string-dot-$comment-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 cmnt ffg/comment
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s.%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\"%s\".%s\"%s\"@example.org" c cmnt qtext)))))

(defspec normalize-$quoted-escapable-string-dot-$quoted-alpha-numeric-string-CRLF-WSP-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s.%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\"%s\".\"%s\"\r\n \t @example.org" c qtext)))))

(defspec normalize-$quoted-escapable-string-dot-$quoted-alpha-numeric-string-WSP-CRLF-WSP-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s.%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\"%s\".\"%s\" \t \r\n \t @example.org" c qtext)))))

(defspec normalize-$quoted-escapable-string-dot-$quoted-alpha-numeric-string-$comment-at-example-org num-tests
  (prop/for-all [c escapable-quoted-string-ctrl-char
                 qtext (gen/not-empty gen/string-alpha-numeric)
                 cmnt ffg/comment]
    (= (format "\"%s.%s\"@example.org" (char-escape-string c) qtext)
       (ff/normalize (format "\"%s\".\"%s\"%s@example.org" c qtext cmnt)))))

; ;;; obs-local-part (atext.quoted-alpha-numeric-string)

(defspec normalize-$atext-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" atext qtext)
       (ff/normalize (format "%s.\"%s\"@example.org" atext qtext)))))

(defspec normalize-CRLF-WSP-$atext-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" atext qtext)
       (ff/normalize (format "\r\n \t %s.\"%s\"@example.org" atext qtext)))))

(defspec normalize-WSP-CRLF-WSP-$atext-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" atext qtext)
       (ff/normalize (format " \t \r\n \t %s.\"%s\"@example.org" atext qtext)))))

(defspec normalize-$comment-$atext-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [cmnt ffg/comment
                 atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" atext qtext)
       (ff/normalize (format "%s%s.\"%s\"@example.org" cmnt atext qtext)))))

(defspec normalize-$atext-CRLF-WSP-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s .%s\"@example.org" atext qtext)
       (ff/normalize (format "%s\r\n \t .\"%s\"@example.org" atext qtext)))))

(defspec normalize-$atext-WSP-CRLF-WSP-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s \\t  .%s\"@example.org" atext qtext)
       (ff/normalize (format "%s \t \r\n \t .\"%s\"@example.org" atext qtext)))))

(defspec normalize-$atext-$comment-dot-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 cmnt ffg/comment
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" atext qtext)
       (ff/normalize (format "%s%s.\"%s\"@example.org" atext cmnt qtext)))))

(defspec normalize-$atext-dot-CRLF-WSP-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s. %s\"@example.org" atext qtext)
       (ff/normalize (format "%s.\r\n \t \"%s\"@example.org" atext qtext)))))

(defspec normalize-atextg-dot-WSP-CRLF-WSP-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "\"%s. \\t  %s\"@example.org" atext qtext)
       (ff/normalize (format "%s. \t \r\n \t \"%s\"@example.org" atext qtext)))))

(defspec normalize-atextg-dot-$comment-$quoted-alpha-numeric-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 cmnt ffg/comment
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" atext qtext)
       (ff/normalize (format "%s.%s\"%s\"@example.org" atext cmnt qtext)))))

(defspec normalize-$atext-dot-$quoted-alpha-numeric-string-CRLF-WSP-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" atext qtext)
       (ff/normalize (format "%s.\"%s\"\r\n \t @example.org" atext qtext)))))

(defspec normalize-$atext-dot-$quoted-alpha-numeric-string-WSP-CRLF-WSP-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)]
    (= (format "%s.%s@example.org" atext qtext)
       (ff/normalize (format "%s.\"%s\" \t \r\n \t @example.org" atext qtext)))))

(defspec normalize-$atext-dot-$quoted-alpha-numeric-string-$comment-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 qtext (gen/not-empty gen/string-alpha-numeric)
                 cmnt ffg/comment]
    (= (format "%s.%s@example.org" atext qtext)
       (ff/normalize (format "%s.\"%s\"%s@example.org" atext qtext cmnt)))))

;;; obs-local-part (atext.quoted-escapable-string)

(defspec normalize-$atext-dot-$quoted-escapable-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s.%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s.\"%s\"@example.org" atext c)))))

(defspec normalize-CRLF-WSP-$atext-dot-$quoted-escapable-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s.%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "\r\n \t %s.\"%s\"@example.org" atext c)))))

(defspec normalize-WSP-CRLF-WSP-$atext-dot-$quoted-escapable-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s.%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format " \t \r\n \t %s.\"%s\"@example.org" atext c)))))

(defspec normalize-$comment-$atext-dot-$quoted-escapable-string-at-example-org num-tests
  (prop/for-all [cmnt ffg/comment
                 atext ffg/atext
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s.%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s%s.\"%s\"@example.org" cmnt atext c)))))

(defspec normalize-$atext-CRLF-WSP-dot-$quoted-escapable-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s .%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s\r\n \t .\"%s\"@example.org" atext c)))))

(defspec normalize-$atext-WSP-CRLF-WSP-dot-$quoted-escapable-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s \\t  .%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s \t \r\n \t .\"%s\"@example.org" atext c)))))

(defspec normalize-$atext-$comment-dot-$quoted-escapable-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 cmnt ffg/comment
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s.%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s%s.\"%s\"@example.org" atext cmnt c)))))

(defspec normalize-$atext-dot-CRLF-WSP-$quoted-escapable-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s. %s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s.\r\n \t \"%s\"@example.org" atext c)))))

(defspec normalize-atextg-dot-WSP-CRLF-WSP-$quoted-escapable-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s. \\t  %s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s. \t \r\n \t \"%s\"@example.org" atext c)))))

(defspec normalize-atextg-dot-$comment-$quoted-escapable-string-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 cmnt ffg/comment
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s.%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s.%s\"%s\"@example.org" atext cmnt c)))))

(defspec normalize-$atext-dot-$quoted-escapable-string-CRLF-WSP-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s.%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s.\"%s\"\r\n \t @example.org" atext c)))))

(defspec normalize-$atext-dot-$quoted-escapable-string-WSP-CRLF-WSP-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 c escapable-quoted-string-ctrl-char]
    (= (format "\"%s.%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s.\"%s\" \t \r\n \t @example.org" atext c)))))

(defspec normalize-$atext-dot-$quoted-escapable-string-$comment-at-example-org num-tests
  (prop/for-all [atext ffg/atext
                 c escapable-quoted-string-ctrl-char
                 cmnt ffg/comment]
    (= (format "\"%s.%s\"@example.org" atext (char-escape-string c))
       (ff/normalize (format "%s.\"%s\"%s@example.org" atext c cmnt)))))

;;;; domain normalization

;;; hostname

(defspec normalize-noreply-at-$dns-label num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (= (str/lower-case (format "noreply@%s" dns-label))
       (ff/normalize (format "noreply@%s" dns-label)))))

(defspec normalize-noreply-at-$CFWS-$dns-label num-tests
  (prop/for-all [cfws ffg/CFWS
                 dns-label ffg/dns-label]
    (= (str/lower-case (format "noreply@%s" dns-label))
       (ff/normalize (format "noreply@%s%s" cfws dns-label)))))

(defspec normalize-noreply-at-$dns-label-$CFWS num-tests
  (prop/for-all [dns-label ffg/dns-label
                 cfws ffg/CFWS]
    (= (str/lower-case (format "noreply@%s" dns-label))
       (ff/normalize (format "noreply@%s%s" dns-label cfws)))))

(defspec normalize-noreply-at-$CFWS-$dns-label-$CFWS num-tests
  (prop/for-all [cfws ffg/CFWS
                 dns-label ffg/dns-label
                 cfws' ffg/CFWS]
    (= (str/lower-case (format "noreply@%s" dns-label))
       (ff/normalize (format "noreply@%s%s%s" cfws dns-label cfws')))))

;;; fqdn

(defspec normalize-noreply-at-$fqdn num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (= (str/lower-case (format "noreply@%s" fqdn))
       (ff/normalize (format "noreply@%s" fqdn)))))

(defspec normalize-noreply-at-$fqdn-period num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (= (str/lower-case (format "noreply@%s" fqdn))
       (ff/normalize (format "noreply@%s." fqdn)))))

(defspec normalize-noreply-at-$CFWS-$fqdn num-tests
  (prop/for-all [cfws ffg/CFWS
                 fqdn ffg/fqdn]
    (= (str/lower-case (format "noreply@%s" fqdn))
       (ff/normalize (format "noreply@%s%s" cfws fqdn)))))

(defspec normalize-noreply-at-$fqdn-$CFWS num-tests
  (prop/for-all [fqdn ffg/fqdn
                 cfws ffg/CFWS]
    (= (str/lower-case (format "noreply@%s" fqdn))
       (ff/normalize (format "noreply@%s%s" fqdn cfws)))))

(defspec normalize-noreply-at-$CFWS-$fqdn-$CFWS num-tests
  (prop/for-all [cfws ffg/CFWS
                 fqdn ffg/fqdn
                 cfws' ffg/CFWS]
    (= (str/lower-case (format "noreply@%s" fqdn))
       (ff/normalize (format "noreply@%s%s%s" cfws fqdn cfws')))))

;;; IPv4

(defspec normalize-noreply-at-bracket-$ip4-bracket num-tests
  (prop/for-all [ip ffg/ipv4]
    (= (format "noreply@[%s]" ip)
       (ff/normalize (format "noreply@[%s]" ip)))))

(deftest normalize-noreply-at-bracket-$ip4-bracket
  (testing "octets without leading zeros"
    (let [prop (prop/for-all [ip ffg/ipv4]
                 (= (str/lower-case (format "noreply@[%s]" ip))
                    (ff/normalize (format "noreply@[%s]" ip))))]
      (is (pass? (quick-check num-tests prop)))))
  (testing "octets with leading zeros"
    (is (= "noreply@[127.0.0.1]"
           (ff/normalize "noreply@[127.0.00.001]")))))

(defspec normalize-noreply-at-$CFWS-bracket-$ip4-bracket num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ffg/ipv4]
    (= (format "noreply@[%s]" ip)
       (ff/normalize (format "noreply@%s[%s]" cfws ip)))))

(defspec normalize-noreply-at-bracket-$ip4-bracket-$CFWS num-tests
  (prop/for-all [ip ffg/ipv4
                 cfws ffg/CFWS]
    (= (format "noreply@[%s]" ip)
       (ff/normalize (format "noreply@[%s]%s" ip cfws)))))

(defspec normalize-noreply-at-$CFWS-bracket-$ip4-bracket-$CFWS num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ffg/ipv4
                 cfws' ffg/CFWS]
    (= (format "noreply@[%s]" ip)
       (ff/normalize (format "noreply@%s[%s]%s" cfws ip cfws')))))

(defspec normalize-noreply-at-bracket-$FWS-$ip4-bracket num-tests
  (prop/for-all [fws ffg/FWS
                 ip ffg/ipv4]
    (= (format "noreply@[%s]" ip)
       (ff/normalize (format "noreply@[%s%s]" fws ip)))))

(defspec normalize-noreply-at-bracket-$ip4-$FWS-bracket num-tests
  (prop/for-all [ip ffg/ipv4
                 fws ffg/FWS]
    (= (format "noreply@[%s]" ip)
       (ff/normalize (format "noreply@[%s%s]" ip fws)))))

(defspec normalize-noreply-at-bracket-$FWS-$ip4-$FWS-bracket num-tests
  (prop/for-all [fws ffg/FWS
                 ip ffg/ipv4
                 fws' ffg/FWS]
    (= (format "noreply@[%s]" ip)
       (ff/normalize (format "noreply@[%s%s%s]" fws ip fws')))))

;;; IPv6

(def ^:private ipv6-without-zeros
  (gen/such-that (complement #(re-find #"0000" %)) (ffg/ipv6-xxxxs)))

(deftest normalize-noreply-at-bracket-IPv6-$ip6-bracket
  (testing "no consecutive zeros"
    (let [prop (prop/for-all [ip ipv6-without-zeros]
                 (= (format "noreply@[IPv6:%s]" ip)
                    (ff/normalize (format "noreply@[IPv6:%s]" ip))))]
      (is (pass? (quick-check num-tests prop)))))
  (testing "replace consecutive zeros"
    (is (= "noreply@[IPv6:2000::ff00:aabb:ff00:0000]"
           (ff/normalize "noreply@[IPv6:2000:0000:0000:0000:ff00:aabb:ff00:0000]"))))
  (testing "optimize consecutive zero replacement"
    (is (= "noreply@[IPv6:0000:0000:2000:feab::abcd]"
           (ff/normalize "noreply@[IPv6:::2000:feab:0000:0000:0000:abcd]"))))
  (testing "convert octets to hextets"
    (is (= "noreply@[IPv6:::7f00:0001]"
           (ff/normalize "noreply@[IPv6:::127.0.0.1]")))))

(defspec normalize-noreply-at-$CFWS-bracket-IPv6-$ip6-bracket num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ipv6-without-zeros]
    (= (format "noreply@[IPv6:%s]" ip)
       (ff/normalize (format "noreply@%s[IPv6:%s]" cfws ip)))))

(defspec normalize-noreply-at-bracket-IPv6-$ip6-bracket-$CFWS num-tests
  (prop/for-all [ip ipv6-without-zeros
                 cfws ffg/CFWS]
    (= (format "noreply@[IPv6:%s]" ip)
       (ff/normalize (format "noreply@[IPv6:%s]%s" ip cfws)))))

(defspec normalize-noreply-at-$CFWS-bracket-IPv6-$ip6-bracket-$CFWS num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ipv6-without-zeros
                 cfws' ffg/CFWS]
    (= (format "noreply@[IPv6:%s]" ip)
       (ff/normalize (format "noreply@%s[IPv6:%s]%s" cfws ip cfws')))))

(defspec normalize-noreply-at-bracket-$FWS-IPv6-$ip6-bracket num-tests
  (prop/for-all [fws ffg/FWS
                 ip ipv6-without-zeros]
    (= (format "noreply@[IPv6:%s]" ip)
       (ff/normalize (format "noreply@[%sIPv6:%s]" fws ip)))))

(defspec normalize-noreply-at-bracket-IPv6-$ip6-$FWS-bracket num-tests
  (prop/for-all [ip ipv6-without-zeros
                 fws ffg/FWS]
    (= (format "noreply@[IPv6:%s]" ip)
       (ff/normalize (format "noreply@[IPv6:%s%s]" ip fws)))))

(defspec normalize-noreply-at-bracket-$FWS-IPv6-$ip6-$FWS-bracket num-tests
  (prop/for-all [fws ffg/FWS
                 ip ipv6-without-zeros
                 fws' ffg/FWS]
    (= (format "noreply@[IPv6:%s]" ip)
       (ff/normalize (format "noreply@[%sIPv6:%s%s]" fws ip fws')))))
