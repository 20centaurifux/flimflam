# flimflam

## Description

**flimflam** is an email address validation library written in Clojure. It tests if a given address conforms to the *addr-spec* specification as described in [RFC 5322](https://www.rfc-editor.org/rfc/rfc5322#section-3.4.1).

## local-part

Elements of the *local-part* of an email addresses can be unquoted or enclosed in quotation marks. If quoted, they may contain whitespace and control characters. There are several places where comments and folding white space may occur.

The following strings are valid email addresses:

* "John.Doe@example.org"
* "(comment)John(nested (comment)).Doe@example.org"
* "John.\r\n Doe@example.org"
* "\\"John@Doe\\"@example.org"
* "\\"\tJohn\bDoe\backslash\\"@example.org"
* "\\"\\"@example.org"

## domain

The *domain* part must be a hostname, a list of dot-separated DNS labels (see [RFC 1035](https://www.rfc-editor.org/rfc/rfc1035)) or an IP address. IPv6 addresses with zone index are not supported. **flimflam** doesn't test the length of FQDNs.

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

user=> (ff/invalid? "   @localhost")
true
```

## Normalization

*flimflam.core/normalize* converts email addresses to a uniform format. It

* removes unneccessary whitespace characters and comments.
* removes enclosing quotation marks from *local-part* when possible.
* adds enclosing quotation marks to *local-part* when neccessary.
* converts control characters to escape sequences.
* converts IP addresses, hostnames and FQDNs.

```
user=> (ff/normalize "john.doe@[IPv6:0000:0000:0000:0000:0000:0000:7f00:0001]")
"john.doe@[IPv6:::7f00:0001]"

user=> (ff/normalize "john.doe@[IPv6:::127.0.0.1]")
"john.doe@[IPv6:::7f00:0001]"

user=> (ff/normalize "john.doe@(hello world)example.org")
"john.doe@example.org"

user=> (ff/normalize "john.doe@ExAmPlE.")
"john.doe@example"

user=> (ff/normalize "john.\"doe\"@example.org")
"john.doe@example.org"

user=> (ff/normalize "\"\"@example.org")
"\"\"@example.org"

user=> (ff/normalize "john.\"\\\"Doe\\\"\"@example.org")
"\"john.\\\"Doe\\\"\"@example.org"
```
