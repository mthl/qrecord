(defproject fr.reuz/qrecord "0.2.0-SNAPSHOT"
  :description "defrecord with qualified field names"
  :url "http://github.com/mthl/qrecord"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.2"]]
  :repl-options {:init-ns qrecord.core}
  :deploy-repositories
  [["releases" {:url "https://repo.clojars.org" :creds :gpg}]
   ["snapshots" {:url "https://repo.clojars.org" :creds :gpg}]])
