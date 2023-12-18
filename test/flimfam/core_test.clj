(ns flimfam.core-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [flimfam.core :as ff]
            [flimfam.generators :as ffg]))

(def ^:private num-tests 100)

(defspec noreply-at-$dns-label num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/valid? (format "noreply@%s" dns-label))))

(defspec noreply-at-hyphen-$dns-label num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/invalid? (format "noreply@-%s" dns-label))))

(defspec noreply-at-digit-$dns-label num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/invalid? (format "noreply@1%s" dns-label))))

(defspec noreply-at-$dns-label-hyphen num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/invalid? (format "noreply@%s-" dns-label))))

(defspec noreply-at-$dns-label-period num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/invalid? (format "noreply@%s." dns-label))))

(defspec noreply-at-$CFWS-$dns-label num-tests
  (prop/for-all [cfws ffg/CFWS dns-label ffg/dns-label]
    (ff/valid? (format "noreply@%s%s" cfws dns-label))))

(defspec noreply-at-$dns-label-$CFWS num-tests
  (prop/for-all [dns-label ffg/dns-label
                 cfws ffg/CFWS]
    (ff/valid? (format "noreply@%s%s" dns-label cfws))))

(defspec noreply-at-$CFWS-$dns-label-$CFWS num-tests
  (prop/for-all [cfws ffg/CFWS
                 dns-label ffg/dns-label
                 cfws' ffg/CFWS]
    (ff/valid? (format "noreply@%s%s%s" cfws dns-label cfws'))))

(defspec noreply-at-$fqdn num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (ff/valid? (format "noreply@%s" fqdn))))

(defspec noreply-at-hyphen-$fqdn num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (ff/invalid? (format "noreply@-%s" fqdn))))

(defspec noreply-at-$fqdn-hyphen num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (ff/invalid? (format "noreply@%s-" fqdn))))

(defspec noreply-at-$fqdn-period num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (ff/valid? (format "noreply@%s." fqdn))))

(defspec noreply-at-$CFWS-$fqdn num-tests
  (prop/for-all [cfws ffg/CFWS
                 fqdn ffg/fqdn]
    (ff/valid? (format "noreply@%s%s" cfws fqdn))))

(defspec noreply-at-$fqdn-$CFWS num-tests
  (prop/for-all [fqdn ffg/fqdn
                 cfws ffg/CFWS]
    (ff/valid? (format "noreply@%s%s" fqdn cfws))))

(defspec noreply-at-$CFWS-$fqdn-$CFWS num-tests
  (prop/for-all [cfws ffg/CFWS
                 fqdn ffg/fqdn
                 cfws' ffg/CFWS]
    (ff/valid? (format "noreply@%s%s%s" cfws fqdn cfws'))))

(defspec noreply-at-bracket-$dns-label-bracket num-tests
  (prop/for-all [dns-label ffg/dns-label]
    (ff/invalid? (format "noreply@[%s]" dns-label))))

(defspec noreply-at-bracket-$fqdn-bracket num-tests
  (prop/for-all [fqdn ffg/fqdn]
    (ff/invalid? (format "noreply@[%s]" fqdn))))

(defspec noreply-at-$ip4 num-tests
  (prop/for-all [ip ffg/ipv4]
    (ff/invalid? (format "noreply@%s" ip))))

(defspec noreply-at-bracket-$ip4 num-tests
  (prop/for-all [ip ffg/ipv4]
    (ff/invalid? (format "noreply@[%s" ip))))

(defspec noreply-at-$ip4-bracket num-tests
  (prop/for-all [ip ffg/ipv4]
    (ff/invalid? (format "noreply@%s]" ip))))

(defspec noreply-at-bracket-$ip4-bracket num-tests
  (prop/for-all [ip ffg/ipv4]
    (ff/valid? (format "noreply@[%s]" ip))))

(defspec noreply-at-$CFWS-bracket-$ip4-bracket num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ffg/ipv4]
    (ff/valid? (format "noreply@%s[%s]" cfws ip))))

(defspec noreply-at-bracket-$ip4-bracket-$CFWS num-tests
  (prop/for-all [ip ffg/ipv4
                 cfws ffg/CFWS]
    (ff/valid? (format "noreply@[%s]%s" ip cfws))))

(defspec noreply-at-$CFWS-bracket-$ip4-bracket-$CFWS num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ffg/ipv4
                 cfws' ffg/CFWS]
    (ff/valid? (format "noreply@%s[%s]%s" cfws ip cfws'))))

(defspec noreply-at-bracket-$FWS-$ip4-bracket num-tests
  (prop/for-all [fws ffg/FWS
                 ip ffg/ipv4]
    (ff/valid? (format "noreply@[%s%s]" fws ip))))

(defspec noreply-at-racket-$ip4-$FWS-bracket num-tests
  (prop/for-all [ip ffg/ipv4
                 fws ffg/FWS]
    (ff/valid? (format "noreply@[%s%s]" ip fws))))

(defspec noreply-at-bracket-$FWS-$ip4-$FWS-bracket num-tests
  (prop/for-all [fws ffg/FWS
                 ip ffg/ipv4
                 fws' ffg/FWS]
    (ff/valid? (format "noreply@[%s%s%s]" fws ip fws'))))

(defspec noreply-at-$ip6 num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/invalid? (format "noreply@%s" ip))))

(defspec noreply-at-IPv6-$ip6 num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/invalid? (format "noreply@IPv6:%s" ip))))

(defspec noreply-at-bracket-IPv6-$ip6 num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/invalid? (format "noreply@[IPv6:%s" ip))))

(defspec noreply-at-IPv6-$ip6-bracket num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/invalid? (format "noreply@IPv6:%s]" ip))))

(defspec noreply-at-bracket-$ip6-bracket num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/invalid? (format "noreply@[%s]" ip))))

(defspec noreply-at-bracket-IPv6-$ip6-bracket num-tests
  (prop/for-all [ip ffg/ipv6]
    (ff/valid? (format "noreply@[IPv6:%s]" ip))))

(defspec noreply-at-$CFWS-bracket-IPv6-$ip6-bracket num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ffg/ipv6]
    (ff/valid? (format "noreply@%s[IPv6:%s]" cfws ip))))

(defspec noreply-at-bracket-IPv6-$ip6-bracket-$CFWS num-tests
  (prop/for-all [ip ffg/ipv6
                 cfws ffg/CFWS]
    (ff/valid? (format "noreply@[IPv6:%s]%s" ip cfws))))

(defspec noreply-at-$CFWS-bracket-IPv6-$ip6-bracket-$CFWS num-tests
  (prop/for-all [cfws ffg/CFWS
                 ip ffg/ipv6
                 cfws' ffg/CFWS]
    (ff/valid? (format "noreply@%s[IPv6:%s]%s" cfws ip cfws'))))

(defspec noreply-at-bracket-$FWS-IPv6-$ip6-bracket num-tests
  (prop/for-all [fws ffg/FWS
                 ip ffg/ipv6]
    (ff/valid? (format "noreply@[%sIPv6:%s]" fws ip))))

(defspec noreply-at-bracket-IPv6-$ip6-$FWS-bracket num-tests
  (prop/for-all [ip ffg/ipv6
                 fws ffg/FWS]
    (ff/valid? (format "noreply@[IPv6:%s%s]" ip fws))))

(defspec noreply-at-bracket-$FWS-IPv6-$ip6-$FWS-bracket num-tests
  (prop/for-all [fws ffg/FWS
                 ip ffg/ipv6
                 fws' ffg/FWS]
    (ff/valid? (format "noreply@[%sIPv6:%s%s]" fws ip fws'))))

(defspec $dot-atom-text-at-example-org num-tests
  (prop/for-all [dot-atom-text ffg/dot-atom-text]
    (ff/valid? (format "%s@example.org" dot-atom-text))))

(defspec $CFWS-$dot-atom-text-at-example-org num-tests
  (prop/for-all [cfws ffg/CFWS
                 dot-atom-text ffg/dot-atom-text]
    (ff/valid? (format "%s%s@example.org" cfws dot-atom-text))))

(defspec $dot-atom-text-$CFWS-at-example-org num-tests
  (prop/for-all [dot-atom-text ffg/dot-atom-text
                 cfws ffg/CFWS]
    (ff/valid? (format "%s%s@example.org" dot-atom-text cfws))))

(defspec $CFWS-$dot-atom-text-$CFWS-at-example-org num-tests
  (prop/for-all [cfws ffg/CFWS
                 dot-atom-text ffg/dot-atom-text
                 cfws' ffg/CFWS]
    (ff/valid? (format "%s%s%s@example.org" cfws dot-atom-text cfws'))))

(defspec $quoted-string-at-example-org num-tests
  (prop/for-all [quoted-string ffg/quoted-string]
    (ff/valid? (format "%s@example.org" quoted-string))))

(defspec $CFWS-$quoted-string-at-example-org num-tests
  (prop/for-all [cfws ffg/CFWS
                 quoted-string ffg/quoted-string]
    (ff/valid? (format "%s%s@example.org" cfws quoted-string))))

(defspec $quoted-string-$CFWS-at-example-org num-tests
  (prop/for-all [quoted-string ffg/quoted-string
                 cfws ffg/CFWS]
    (ff/valid? (format "%s%s@example.org" quoted-string cfws))))

(defspec $CFWS-$quoted-string-$CFWS-at-example-org num-tests
  (prop/for-all [cfws ffg/CFWS
                 quoted-string ffg/quoted-string
                 cfws' ffg/CFWS]
    (ff/valid? (format "%s%s%s@example.org" cfws quoted-string cfws'))))

(defspec $obs-local-part-at-example-org num-tests
  (prop/for-all [obs-local-part ffg/obs-local-part]
    (ff/valid? (format "%s@example.org" obs-local-part))))
