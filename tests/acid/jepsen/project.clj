(defproject jepsen.terminusdb "0.1.0"
  :description "Jepsen ACID verification for TerminusDB: elle-checked append
                transactions under kill/pause nemeses"
  :dependencies [[org.clojure/clojure "1.11.4"]
                 [jepsen "0.3.8"]
                 [clj-http "3.13.0"]
                 [cheshire "5.13.0"]]
  :main jepsen.terminusdb
  :repl-options {:init-ns jepsen.terminusdb})
