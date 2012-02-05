;; ## REST Fact endpoints
;;
;; There is only one route available for querying facts, which is
;; '/facts/<node>'. This route responds with a JSON version of the following
;; map:
;;
;;     {:name "node"
;;      :facts {"fact" "value"
;;              "fact" "value"
;;              ...}}
;;
;; If no facts are known for the node, the response is a 404 with an error message.

(ns com.puppetlabs.cmdb.http.facts
  (:require [cheshire.core :as json]
            [com.puppetlabs.utils :as utils]
            [com.puppetlabs.washboard :as wb]
            [com.puppetlabs.cmdb.query.facts :as f]))

(defn facts->json
  "Produce a response body for a request to lookup facts for `node`."
  [req {:keys [node facts]} resp]
  (assoc resp :body (json/generate-string {:name node :facts facts})))

(defn resource-exists?
  [req {:keys [node db] :as heap} resp]
  (let [facts (f/facts-for-node db node)
        heap  (assoc heap :facts facts)]
    (if (seq facts)
      {:result true :heap heap}
      (let [error (str "Could not find facts for " node)
            msg   (json/generate-string {:error error})
            resp  (assoc resp :body msg)]
        {:result false :response resp}))))

(defn malformed-request?
  [{:keys [params globals] :as req} heap resp]
  (let [heap (-> heap
                 (assoc :node (params "node"))
                 (assoc :db (:scf-db globals)))]
    (if (:node heap)
      {:result false, :heap heap}
      {:result true, :resp (assoc resp :body "missing node")})))

(def state-machine
  {:allowed-methods        (constantly {:result #{:get}})
   :resource-exists?       resource-exists?
   :malformed-request?     malformed-request?
   :content-types-provided (constantly {:result {"application/json" facts->json}})})

(def facts-app (wb/washboard-handler state-machine))
