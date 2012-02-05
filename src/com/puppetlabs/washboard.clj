;; ## Washboard
;;
;; A simpler version of [Clothesline](https://github.com/banjiewen/Clothesline).

(ns com.puppetlabs.washboard
  (:require [ring.util.response :as rr]
            [com.puppetlabs.utils :as utils]
            [clojure.string :as string]))

;; ## Handler functions
;;
;; ### allowed-methods?
;;
;; Returns a function that, when passed the method of the current
;; request, returns a boolen indicating whether that method is allowed
;; or not.
;;
;; ### resource-exists?
;;
;; Returns a boolean indicating whether or not the request resource
;; exists.
;;
;; ### malformed-request?
;;
;; Returns a boolean indicating whether or not the request is
;; malformed.
;;
;; ### content-types-provided
;;
;; A map of `{<content-type> <handler func>}`. Based on content
;; negotion, we'll select a particular contentent type and handler
;; function from the map such that it satisfies the request's
;; constraints. The response's content type is set to the suitable
;; type, and the body is set to the handler function. When the state
;; machine reaches completion and a response is needed, the handler
;; function is invoked.

;; ## Default handlers
;;
(def default-funcs
  {
   :allowed-methods        (constantly {:result #{:get :head}})
   :resource-exists?       (constantly {:result true})
   :malformed-request?     (constantly {:result false})
   :content-types-provided (constantly {:result {}})
   })

;; ## State machine
;;
;; Our HTTP handling state machine is the same as (the state machine
;; from Basho's
;; Webmachine)[https://github.com/basho/webmachine/raw/master/www/images/http-headers-status-v3.png].
;;
;; All of the transition states below share the same names as those in
;; Webmachine. For terminal states, we use state names corresponding
;; to the desired HTTP response code. So `:404` would be a terminal
;; state, whereas `:b13` would not.

;; We start at the _service available?_ state.
(def starting-state :b13)

;; Transitions for all states
(def state-machine
  {
   :b13 {true :b12 false :503}
   :b12 {true :b11 false :501}
   :b11 {true :414 false :b10}
   :b10 {true :b9 false :405}
   :b9  {true :400 false :b8}

   :b8  {true :b7 false :401}
   :b7  {true :403 false :b6}
   :b6  {true :501 false :b5}
   :b5  {true :415 false :b4}
   :b4  {true :413 false :b3}
   :b3  {true :200 false :c3}

   :c3  {true :c4 false :d4}
   :c4  {true :d4 false :406}

   :d4  {true :d5 false :e5}
   :d5  {true :e5 false :406}

   :e5  {true :e6 false :f6}
   :e6  {true :f6 false :406}

   :f6  {true :f7 false :g7}
   :f7  {true :g7 false :406}

   :g7  {true :g8 false :h7}
   :g8  {true :g9 false :h10}
   :g9  {true :h10 false :g11}
   :g11 {true :h10 false :412}

   :h7  {true :412 false :i7}
   :h10 {true :h11 false :i12}
   :h11 {true :h12 false :i12}
   :h12 {true :412 false :i12}

   :i4  {true :301 false :p3}
   :i7  {true :i4 false :k7}
   :i12 {true :i13 false :l13}
   :i13 {true :j18 false :k13}

   :j18 {true :304 false :412}

   :k5  {true :301 false :l5}
   :k7  {true :k5 false :l7}
   :k13 {true :j18 false :l13}

   :l5  {true :307 false :m5}
   :l7  {true :m7 false :404}
   :l13 {true :l14 false :m16}
   :l14 {true :l15 false :m16}
   :l15 {true :m16 false :l17}
   :l17 {true :m16 false :304}

   :m5  {true :n5 false :410}
   :m7  {true :n11 false :404}
   :m16 {true :m20 false :n16}
   :m20 {true :o20 false :202}

   :n5  {true :n11 false :410}
   :n11 {true :303 false :p11}
   :n16 {true :n11 false :o16}

   :o14 {true :409 false :p11}
   :o16 {true :o14 false :o18}
   :o18 {true :300 false :200}
   :o20 {true :o18 false :204}

   :p3  {true :409 false :p11}
   :p11 {true :201 false :o20}
   })

;; ## Transitions

(defn user-fn
  [kw request heap response]
  (let [func (get-in @heap [:fn-map kw])
        return-map (func request @heap @response)]

    (doseq [key (keys return-map)]
      (when-not (#{:result :heap :response} key)
        (throw (IllegalArgumentException.
                (format "Invalid key in return map: %s" key)))))

    (dosync
     (when-let [new-heap (:heap return-map)]
       (ref-set heap new-heap))
     (when-let [new-response (:response return-map)]
       (ref-set response new-response)))
    (:result return-map)))

(defmulti transition (fn [state _ _ _] state))

;; ### Known method?
(defmethod transition :b12
  [_ request heap response]
  (let [methods #{:get :head :post :put :delete :trace :connect :options}]
    (methods (:request-method request))))

;; ### Method allowed?
(defmethod transition :b10
  [_ request heap response]
  (let [req-method (:request-method request)
        allowed    (user-fn :allowed-methods request heap response)]
    (allowed req-method)))

;; ### Malformed?
(defmethod transition :b9
  [_ request heap response]
  (user-fn :malformed-request? request heap response))

;; ### "Accept" header exists?
(defmethod transition :c3
  [_ request heap response]
  (if (get-in request [:headers "accept"]) true false))

;; ### Acceptable media type available?
;;
;; TODO: Support prioritized content negotioation. As it stands, if
;; multiple handlers are acceptable we just pick a random one.
(defmethod transition :c4
  [_ request heap response]
  (let [accept-header (get-in request [:headers "accept"])
        provided      (user-fn :content-types-provided request heap response)
        suitable      (for [[type handler] provided
                            :when (utils/acceptable-content-type type accept-header)]
                        [type handler])]
    (if (seq suitable)
      ;; TODO: do something smarter when multiple acceptable choices
      ;; exist
      (let [[type handler] (first suitable)]
        (dosync
         (alter response rr/header "Content-Type" type)
         (alter response assoc :body handler))
        true)
      false)))

;; ### Resource exists?
(defmethod transition :g7
  [_ request heap response]
  (user-fn :resource-exists? request heap response))

;; ### "If-Match: *" header exists?
(defmethod transition :h7
  [_ request heap response]
  (= "*" (get-in request [:headers "if-match"])))

;; ### PUT?
(defmethod transition :i7
  [_ request heap response]
  (= :put (:request-method request)))

;; ### POST?
(defmethod transition :l7
  [_ request heap response]
  (= :post (:request-method request)))

;; ### POST?
(defmethod transition :m5
  [_ request heap response]
  (= :post (:request-method request)))

;; ### DELETE?
(defmethod transition :m16
  [_ request heap response]
  (= :delete (:request-method request)))

;; ### PUT?
(defmethod transition :o16
  [_ request heap response]
  (= :put (:request-method request)))

;; ### Response includes an entity?
(defmethod transition :o20
  [_ request heap response]
  (if (:body @response) true false))

;; ### POST?
(defmethod transition :n16
  [_ request heap response]
  (= :post (:request-method request)))

;; ### New resource?
(defmethod transition :p11
  [_ request heap response]
  (if (get-in request [:headers "Location"]) true false))

;; ### Unimplemented states
;;
;; These are currently implemented as "pass through", in that they
;; default to an implementation that leads to a non-terminal state.
(defmethod transition :b3 [_ request heap response] false)
(defmethod transition :b4 [_ request heap response] false)
(defmethod transition :b5 [_ request heap response] false)
(defmethod transition :b6 [_ request heap response] false)
(defmethod transition :b7 [_ request heap response] false)
(defmethod transition :b8 [_ request heap response] true)
(defmethod transition :b11 [_ request heap response] false)
(defmethod transition :b13 [_ request heap response] true)
(defmethod transition :d4 [_ request heap response] false)
(defmethod transition :e5 [_ request heap response] false)
(defmethod transition :f6 [_ request heap response] false)
(defmethod transition :g8 [_ request heap response] false)
(defmethod transition :h10 [_ request heap response] false)
(defmethod transition :i12 [_ request heap response] false)
(defmethod transition :k5 [_ request heap response] false)
(defmethod transition :k7 [_ request heap response] false)
(defmethod transition :l5 [_ request heap response] false)
(defmethod transition :l13 [_ request heap response] false)
(defmethod transition :m7 [_ request heap response] true)
(defmethod transition :m20 [_ request heap response] true)
(defmethod transition :n5 [_ request heap response] true)
(defmethod transition :n11 [_ request heap response] false)
(defmethod transition :o14 [_ request heap response] false)
(defmethod transition :o18 [_ request heap response] false)
(defmethod transition :p3 [_ request heap response] false)

;; ## Utility funcs

(defn numeric-keyword?
  [kw]
  (let [s (name kw)]
    (try
      (Integer/parseInt s)
      true
      (catch NumberFormatException e
        false))))

(defn finish-request
  [state request heap response]
  ;; Set response status code
  (dosync
   (alter response rr/status (Integer/parseInt (name state))))

  (let [body (:body @response)]
    (if (fn? body)
      (body request @heap @response)
      @response)))

;; ## External interface

(defn washboard-handler
  [fn-map]
  ;; Verify that all supplied handlers are allowed
  (doseq [name (keys fn-map)]
    (when-not (default-funcs name)
      (throw (IllegalArgumentException. (format "Unknown handler: %s" name)))))

  ;; Construct a Ring app
  (fn [request]
    (let [heap (ref {:fn-map (merge default-funcs fn-map)})
          resp (ref (rr/status (rr/response nil) 400))]

      (loop [state starting-state]
        (if (numeric-keyword? state)
          (finish-request state request heap resp)
          (let [result (transition state request heap resp)
                possible-next-states (get state-machine state {})
                next-state-key (if result true false)
                next-state (get possible-next-states next-state-key :500)]
            (recur next-state)))))))
