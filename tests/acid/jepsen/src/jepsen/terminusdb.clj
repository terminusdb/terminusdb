(ns jepsen.terminusdb
  "Jepsen ACID verification for TerminusDB's atomic commit structure.

  Runs jepsen.tests.cycle.append (Elle): transactions of reads and appends
  across a small key set, each append performed through the document API
  under a TerminusDB-Data-Version guard — the versioned label CAS commit
  path. A nemesis kills (-9) and pauses (SIGSTOP) the server between ops.
  Elle proves the observed history strictly serializable, or reports the
  anomaly. Results land in ./store/."
  (:require [clojure.string :as str]
            [clojure.tools.logging :refer [info warn]]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [jepsen [cli :as cli]
             [client :as client]
             [control :as c]
             [db :as db]
             [generator :as gen]
             [nemesis :as nemesis]
             [tests :as tests]
             [util :as util :refer [meh]]]
            [jepsen.checker :as checker]
            [jepsen.checker.timeline :as timeline]
            [jepsen.control.util :as cu]
            [jepsen.os.debian :as debian]
            [jepsen.tests.cycle.append :as append]))

(def binary "/app/terminusdb/terminusdb")
(def store-dir "/opt/tdb/store")
(def log-file "/opt/tdb/terminusdb.log")
(def pid-file "/opt/tdb/terminusdb.pid")
(def db-name "ja")
(def port 6363)

(def serve-env
  {"TERMINUSDB_SERVER_DB_PATH" store-dir
   "TERMINUSDB_SERVER_PORT" (str port)
   "TERMINUSDB_SERVER_NAME" "127.0.0.1"
   "TERMINUSDB_ADMIN_PASS" "root"
   "TERMINUSDB_JWT_ENABLED" "false"
   "TERMINUSDB_ENABLE_OPTIMIZER" "0"
   "TERMINUSDB_INDEXER_BACKEND" "none"
   "TERMINUSDB_WORKER_POOL_SIZE" "8"
   "TERMINUSDB_COMMIT_QUEUE_TIMEOUT" "5"
   "TERMINUSDB_LOG_LEVEL" "ERROR"})

(defn env-args []
  (mapv (fn [[k v]] (str k "=" v)) serve-env))

(defn start-db!
  "Start `terminusdb serve` on the node via the current SSH session."
  [_test _node]
  (c/exec :mkdir :-p store-dir)
  (cu/start-daemon! {:logfile log-file
                     :pidfile pid-file
                     :chdir "/opt/tdb"
                     :env serve-env}
                    binary "serve"))

(defn stop-db!
  "Hard-kill the server — the nemesis kill op and teardown share this."
  [_test _node]
  (meh (cu/stop-daemon! pid-file))
  (meh (c/exec :pkill :-9 :-f "terminusdb serve")))

(defn init-store!
  "Reinitialise the store. Wipes everything — only ever called at setup."
  [_test _node]
  (stop-db! _test _node)
  (c/exec :rm :-rf store-dir)
  (c/exec :mkdir :-p store-dir)
  (apply c/exec :env (concat (env-args) [binary "store" "init" "--key" "root"])))

(defn api-url [node path]
  (str "http://" node ":" port path))

(defn wait-ok
  "Wait for /api/ok on the node. Runs on the control node over HTTP."
  [node timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if (try (= 200 (:status (http/get (api-url node "/api/ok")
                                        {:socket-timeout 1000
                                         :conn-timeout 1000
                                         :throw-exceptions false})))
               (catch Exception _ false))
        true
        (do (when (< deadline (System/currentTimeMillis))
              (throw (ex-info "terminusdb did not become ready" {:node node})))
            (Thread/sleep 500)
            (recur))))))

(defn tdb-db []
  (reify db/DB
    (setup! [_ test node]
      (init-store! test node)
      (start-db! test node)
      (wait-ok node 30000)
      (c/exec :python3 "/opt/tdb/setup-db.py"))
    (teardown! [_ test node]
      (stop-db! test node))
    db/LogFiles
    (log-files [_ _test _node]
      [log-file])))

;;; Document API client

(defn http-opts [extra]
  (merge {:basic-auth ["admin" "root"]
          :as :json
          :socket-timeout 10000
          :conn-timeout 3000
          :throw-exceptions false}
         extra))

(defn data-version [resp]
  (->> (:headers resp)
       (filter (fn [[k _]] (= "terminusdb-data-version" (str/lower-case (name k)))))
       first
       second))

(defn get-doc
  "GET a document by id; returns {:status, :doc, :version}."
  [node id]
  (let [r (http/get (api-url node (str "/api/document/admin/" db-name))
                    (http-opts {:query-params {"graph_type" "instance"
                                               "as_list" "true"
                                               "id" id}}))
        body (:body r)]
    {:status (:status r)
     :doc (when (vector? body) (first body))
     :version (data-version r)}))

(defn put-doc!
  "Replace/insert the document under a data-version guard. Returns resp."
  [node doc version]
  (http/put (api-url node (str "/api/document/admin/" db-name))
            (http-opts {:query-params {"id" (:doc-id doc)
                                       "graph_type" "instance"
                                       ;; create=true makes the replace an upsert:
                                       ;; first append inserts, later ones CAS-replace.
                                       "create" "true"
                                       "author" "jepsen"
                                       "message" "jepsen append"}
                        :content-type :json
                        :headers {"TerminusDB-Data-Version" (or version "")}
                        :body (json/generate-string
                                {"@id" (:doc-id doc)
                                 "@type" "JepsenList"
                                 "vals" (:vals doc)})})))

(defn version-mismatch?
  "400 version-conflict — the CAS lost a race; safe to retry. The api
  layer reports it as api:BadDataVersion or api:DataVersionMismatch; the
  body may arrive keyword- or string-keyed."
  [resp]
  (and (= 400 (:status resp))
       (let [b (:body resp)]
         (if (string? b)
           ;; error responses don't always come back JSON-parsed
           (or (str/includes? b "DataVersionMismatch")
               (str/includes? b "BadDataVersion"))
           (let [err (get b "api:error" (get b :api:error))
                 t   (get err "@type" (get err (keyword "@type")))]
             (contains? #{"api:DataVersionMismatch" "api:BadDataVersion"}
                        (some-> t name)))))))

(defn doc-id [k] (str "JepsenList/" k))

(defn doc-vals
  "The doc's vals array; the response body may be keyword- or
  string-keyed depending on how clj-http parsed it."
  [doc]
  (or (get doc "vals") (get doc :vals)))

(defn read-vals
  "Current ordered list for key k, as longs. A missing document (404)
  reads as []; anything that isn't a clean read must not be recorded
  as one — it fails the op instead."
  [node k]
  (let [{:keys [status doc]} (get-doc node (doc-id k))]
    (cond
      (and (= 200 status) doc)
      (mapv #(Long/parseLong %) (or (doc-vals doc) []))

      (or (= 404 status) (= 200 status)) []

      :else (throw (ex-info "read failed" {:key k :status status})))))

(defn append!
  "read-modify-write under the data-version CAS, bounded retries. Each
  completed append is exactly one committed version increment."
  [node k v]
  (loop [tries 30]
    (when (zero? tries)
      (throw (ex-info "data-version CAS retries exhausted" {:key k})))
    (let [{:keys [doc version status]} (get-doc node (doc-id k))
          _ (when-not (or (= 200 status) (= 404 status))
            (throw (ex-info "pre-append read failed" {:key k :status status})))
          vals (if doc (mapv str (or (doc-vals doc) [])) [])
          resp (put-doc! node {:doc-id (doc-id k)
                               :vals (conj vals (str v))}
                         version)]
      (cond
        (<= 200 (:status resp) 299) :done
        (version-mismatch? resp) (recur (dec tries))
        :else (throw (ex-info "put-doc failed"
                              {:key k :status (:status resp)
                               :api-error (get-in resp [:body "api:error" "@type"])
                               :resp-body (let [b (:body resp)]
                                            (subs (str b) 0 (min 300 (count (str b)))))}))))))

(defn mop!
  "Apply one append-txn micro-op; returns the completed mop."
  [node [f k v :as mop]]
  (case f
    :r      [:r k (read-vals node k)]
    :append (do (append! node k v) [:append k v])
    mop))

(defrecord AppendClient [node]
  client/Client
  (open! [this _test node] (assoc this :node node))
  (setup! [_this _test])
  (invoke! [this _test op]
    (let [node (:node this)]
      (try
        (assoc op :type :ok
               :value (mapv (partial mop! node) (:value op)))
        (catch clojure.lang.ExceptionInfo e
          ;; A 5xx may be post-commit: e.g. data_version_cas_lost is thrown
          ;; after builders committed. Mark those indeterminate, not :fail.
          (if (<= 500 (or (:status (ex-data e)) 0) 599)
            (assoc op :type :info :error (str "indeterminate " (pr-str (ex-data e))))
            (assoc op :type :fail
                 :error (str (.getMessage e) " " (pr-str (dissoc (ex-data e) :body))))))
        (catch java.net.SocketTimeoutException e
          (assoc op :type :info :error :timeout))
        (catch java.net.ConnectException e
          (assoc op :type :info :error :conn-refused))
        (catch org.apache.http.conn.HttpHostConnectException e
          (assoc op :type :info :error :conn-refused)))))
  (teardown! [_this _test])
  (close! [_this _test]))

;;; Nemesis: kill -9 and SIGSTOP/SIGCONT the server between ops

(defrecord ChaosNemesis []
  nemesis/Nemesis
  (setup! [this _test] this)
  (invoke! [_this test op]
    (let [node (first (:nodes test))]
      (case (:f op)
        :kill
        (do (info "nemesis: kill -9")
            (c/on node (stop-db! test node))
            (assoc op :value :killed))

        :restart
        (do (info "nemesis: restart")
            (c/on node (start-db! test node))
            (try (wait-ok node 60000)
                 (assoc op :value :restarted)
                 (catch Exception e
                   ;; The store failing to recover is itself a finding.
                   (assoc op :value :never-ready :error (.getMessage e)))))

        :pause
        (do (info "nemesis: SIGSTOP")
            (c/on node (meh (c/exec :pkill :-STOP :-f "terminusdb serve")))
            (assoc op :value :paused))

        :resume
        (do (info "nemesis: SIGCONT")
            (c/on node (meh (c/exec :pkill :-CONT :-f "terminusdb serve")))
            (try (wait-ok node 60000)
                 (assoc op :value :resumed)
                 (catch Exception e
                   (assoc op :value :never-ready :error (.getMessage e)))))

        (assoc op :value :unknown))))
  (teardown! [_this _test]))

(def nemesis-gen
  "Nemesis op sequence: alternate hard kills with pauses."
  (cycle [(gen/sleep 10)
          {:type :info, :f :kill}
          {:type :info, :f :restart}
          (gen/sleep 10)
          {:type :info, :f :pause}
          (gen/sleep 5)
          {:type :info, :f :resume}]))

(defn acid-test
  [opts]
  (let [workload (append/test {:key-count 4
                               :max-txn-length 4
                               :max-writes-per-key
                               (:max-writes-per-key opts)})]
    (merge tests/noop-test
           workload
           {:name "terminusdb-acid"
            :os debian/os
            :db (tdb-db)
            :client (->AppendClient nil)
            :nemesis (->ChaosNemesis)
            :ssh {:username "root"
                  :private-key-path "/root/.ssh/id_rsa"
                  :strict-host-key-checking false}
            :checker (checker/compose
                      {:elle-append (append/checker {:consistency-models
                                                     [:strict-serializable]})
                       :timeline (timeline/html)
                       :stats (checker/stats)
                       :perf (checker/perf)
                       :exceptions (checker/unhandled-exceptions)})
            :pure-generators true
            :generator (->> (:generator workload)
                            (gen/nemesis nemesis-gen)
                            (gen/time-limit (:time-limit opts)))}
           (dissoc opts :max-writes-per-key))))

(def cli-opts
  [[nil "--max-writes-per-key NUM" "Upper bound on appends per key"
    :default 128
    :parse-fn #(Long/parseLong %)]])

(defn -main [& args]
  (cli/run! (cli/single-test-cmd {:test-fn acid-test
                                  :opt-spec cli-opts})
            args))
