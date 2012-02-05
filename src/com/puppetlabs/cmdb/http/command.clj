;; ## REST Command endpoints
;;
;; Commands can be submitted via HTTP, provided the following criteria
;; are met:
;;
;; * A `POST` is used
;;
;; * The `POST` contains a single parameter, `payload`
;;
;; * The `payload` paramater contains a string conforming to the
;;   structure of a command as outlined in
;;   `com.puppetlabs.cmdb.command`
;;
;; The response:
;;
;; * Has a content type of `application/json`
;;
;; * Contains a JSON object of `true` if the command was successfully
;;   submitted to the MQ
;;
(ns com.puppetlabs.cmdb.http.command
  (:require [clojure.contrib.logging :as log]
            [com.puppetlabs.mq :as mq]
            [com.puppetlabs.washboard :as wb]
            [cheshire.core :as json]
            [clamq.protocol.producer :as mq-producer]
            [clamq.protocol.connection :as mq-conn]))

(defn http->mq
  "Takes the given command and submits it to the specified endpoint on
  the indicated MQ.

  If successful, this function returns a JSON `true`."
  [req {:keys [payload mq-spec mq-endpoint]} resp]
  {:pre  [(string? payload)
          (string? mq-spec)
          (string? mq-endpoint)]
   :post [(map? %)]}
  (with-open [conn (mq/connect! mq-spec)]
    (let [producer (mq-conn/producer conn)]
      (mq-producer/publish producer mq-endpoint payload)))
  (-> resp
      (assoc :body (json/generate-string true))))

(defn malformed-request?
  [{:keys [params globals] :as req} heap resp]
  (let [heap (-> heap
                 (assoc :payload (params "payload"))
                 (assoc :mq-spec (get-in globals [:command-mq :connection-string]))
                 (assoc :mq-endpoint (get-in globals [:command-mq :endpoint])))]
    (if (:payload heap)
      {:result false, :heap heap}
      {:result true, :heap heap, :resp (assoc resp :body "missing payload")})))

(def state-machine
  {:allowed-methods        (constantly {:result #{:post}})
   :malformed-request?      malformed-request?
   :content-types-provided (constantly {:result {"application/json" http->mq}})})

(def command-app (wb/washboard-handler state-machine))
