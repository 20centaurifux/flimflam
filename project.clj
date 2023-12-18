(defproject de.dixieflatline/flimfam "0.1.0-SNAPSHOT"
  :description "Email address validation."
  :url "https://github.com/20centaurifux/flimfam"
  :license {:name "AGPLv3"
            :url "https://www.gnu.org/licenses/agpl-3.0"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.4.12"]
                 [org.clojure/test.check "1.1.1"]
                 [com.velisco/strgen "0.2.5"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :plugins [[lein-marginalia "0.9.1"]
            [dev.weavejester/lein-cljfmt "0.12.0"]]
  :cljfmt {:load-config-file? true})
