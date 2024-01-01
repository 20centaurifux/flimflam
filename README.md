# flimflam

## Description

**flimflam** is an email address validation library written in Clojure. It tests if a given email address conforms to the *addr-spec* specification as described in [RFC 5322](https://www.rfc-editor.org/rfc/rfc5322#section-3.4.1).

## local-part

The locally interpreted string is either a *quoted-string* or a *dot-atom*. If quoted, it may contain whitespace and quoted (control) characters. **flimflam** doesn't support obsolete syntax (*obs-local-part*).

The following strings are valid email addresses:

* "John.Doe@example.org"
* "(nested (comment))John.Doe@example.org"
* "\\"John\r\n Doe\\"@example.org"
* "\\"John@Doe\\"@example.org"
* "\\"\\"@example.org"

## domain

The domain must be a hostname, a list of dot-separated DNS labels (see [RFC 1035](https://www.rfc-editor.org/rfc/rfc1035)) or an IP address. IPv6 addresses with zone index (e.g. fe80::3dd0:7f8e:57b7:34d5%19) are not supported. **flimflam** doesn't test the length of FQDNs.

The following strings are valid email addresses:

* "john.doe@(comment)\r\n localhost"
* "john.doe@example.org."
* "john.doe@[127.0.0.1]"
* "john.doe@[IPv6:2001:0db8::192.168.14.25]"

## Validation

*flimflam.core/valid?* and *flimflam.core/invalid?* test if an email address conforms to the *addr-spec* specification.

```
user=> (require '[flimflam.core :as ff])

user=> (ff/valid? "john.doe@[IPv6:::]")
true

user=> (ff/invalid? "@localhost")
true
```

## Normalization

*flimflam.core/normalize* converts email addresses to a uniform format. It

* removes unneccessary whitespace characters and comments.
* removes enclosing quotation marks from *local-part* when possible.
* normalizes IP addresses, hostnames and FQDNs.

Folding white space that occurs inside *quoted-strings* is interpreted as a single space character.

```
user=> (ff/normalize "john.doe@[IPv6:0000:0000:0000:0000:0000:0000:7f00:0001]")
"john.doe@[IPv6:::7f00:0001]"

user=> (ff/normalize "john.doe@[IPv6:::127.0.0.1]")
"john.doe@[IPv6:::7f00:0001]"

user=> (ff/normalize "john.doe@ExAmPlE.ORG.")
"john.doe@example.org"

user=> (ff/normalize "john.doe@(hello world)example.org")
"john.doe@example.org"

user=> (ff/normalize "john.doe   @example.org")
"john.doe@example.org"

user=> (ff/normalize "\"john\\.doe\"@example.org")
"john.doe@example.org"

user=> (ff/normalize "\"john\t\r\n   doe\"@example.org")
"\"john doe\"@example.org"
```
