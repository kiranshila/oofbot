(ns oofbot.core
  (:require [discljord.connections :as c]
            [discljord.messaging :as m]
            [discljord.events :as e]
            [clojure.core.async :as a]
            [clojure.string :refer [includes? lower-case trim]]
            [clojure.pprint :refer [print-table]]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:gen-class))

;; Discord Bot API Token
(def token (trim (slurp "token.txt")))

;; Writes data to an edn file
(defn write-edn [data filename]
  (with-open [f (io/writer filename)]
    (binding [*out* f]
      (pr data))))

;; State Object
(def state (atom nil))

;; Atom for oofbot data
(def oof-data
  (if (.exists (io/as-file "oof-data.edn"))
    (atom (with-open [r (java.io.PushbackReader. (io/reader "oof-data.edn"))]
            (edn/read r)))
    (atom nil)))

;; How many users in the "top" list
(def top-count 5)

;; Check to see if a given str is a valid oof
(defn is-oof? [input]
  (some? (re-seq #"(?)\bo+of\b" input)))

;; Get a vector of 5 messages before 
(defn get-messages-before [before-id channel-id]
  @(m/get-channel-messages! (:messaging @state) channel-id
                            :before before-id
                            :limit top-count))

;; Get a username from a user-id
(defn get-username [user-id]
  (:username @(m/get-user! (:messaging @state) user-id)))

;; Get a message from a message-id and channel-id
(defn get-message [message-id channel-id]
  @(m/get-channel-message! (:messaging @state) channel-id message-id))

;; Given an oof message, find the first non-oof message above it
(defn find-oof-parent [oof-message-id channel-id]
  (loop [last-message-id oof-message-id
         message-chunk (get-messages-before last-message-id channel-id)]
    (if-let [[message & messages] (seq message-chunk)]
      (if (->> message
               :content
               is-oof?
               not)
        message
        (recur (:id message) messages))
      (recur last-message-id (get-messages-before last-message-id channel-id)))))

;; Send a str as a message on the channel
(defn send-message [message channel-id]
  (m/create-message! (:messaging @state) channel-id :content message))

;; Update the oof-data state from a message event
(defn do-oof-message [user-id message-id channel-id guild-id]
  (swap! oof-data update-in [guild-id user-id :given-oof] (fnil inc 0))
  (let [{{bot :bot user-id :id username :username} :author
         content :content
         message-id :id}
        (find-oof-parent message-id channel-id)]
    (swap! oof-data update-in [guild-id user-id :gotten-oofed] (fnil inc 0))))

;; Sort the oof data by key
(defn sort-oof-data [key guild-id]
  (take top-count (sort-by (comp key second) (fnil > 0 0) (into [] (get @oof-data guild-id)))))

;; Convert an oof data row into a "username" "count" map
(defn oof-row-to-table [oof-data-row key]
  {"Username" (get-username (first oof-data-row))
   "Count" (get (second oof-data-row) key)})

;; Convert oof-data into an ascii table sorted by key
(defn oof-data-to-table [key guild-id]
  (str "```"
       (let [top (sort-oof-data key guild-id)]
         (with-out-str (print-table (map #(oof-row-to-table % key)
                                         top))))
       "```"))

;; Get the string summary of oof-data
(defn get-oofs [guild-id]
  (str
   "Top Oofed Users"
   (oof-data-to-table :gotten-oofed guild-id)
   "Top Oofers"
   (oof-data-to-table :given-oof guild-id)))

(defmulti handle-event
  (fn [event-type event-data]
    event-type))

(defmethod handle-event :default
  [event-type event-data])

(defn set-status []
  (c/status-update! (:connection @state)
                    :activity (c/create-activity :name "!getoofs")))

(defmethod handle-event :message-create
  [event-type {{bot :bot user-id :id username :username} :author
               channel-id :channel-id
               content :content
               message-id :id
               guild-id :guild-id}]
  (if (and (is-oof? content)
           (not bot))
    (do-oof-message user-id message-id channel-id guild-id)
    ;; Commands
    (if (and (= (first content) \!)
             (not bot))
      (if (includes? (lower-case content) "getoofs")
        (send-message (get-oofs guild-id) channel-id)))))

(defmethod handle-event :message-reaction-add
  [event-type {:keys [user-id channel-id message-id emoji guild-id]}]
  (if (= "OOF" (:name emoji))
    (let [{{oofee-id :id} :author} (get-message message-id channel-id)]
      (swap! oof-data update-in [guild-id user-id :given-oof] (fnil inc 0))
      (swap! oof-data update-in [guild-id oofee-id :gotten-oofed] (fnil inc 0)))))

(defmethod handle-event :message-reaction-remove
  [event-type {:keys [user-id channel-id message-id emoji guild-id]}]
  (if (= "OOF" (:name emoji))
    (let [{{oofee-id :id} :author} (get-message message-id channel-id)]
      (swap! oof-data update-in [guild-id user-id :given-oof] (fnil dec 0))
      (swap! oof-data update-in [guild-id oofee-id :gotten-oofed] (fnil dec 0)))))

(defn save-data []
  (a/go-loop []
    (a/<! (a/timeout (* 5 60 1000)))
    (set-status)
    (print "Saving data\n")
    (write-edn @oof-data "oof-data.edn")
    (recur)))

(defn -main
  [& args]
  (let [event-ch (a/chan 100)
        connection-ch (c/connect-bot! token event-ch :intents #{:guild-messages
                                                                :guild-message-reactions})
        messaging-ch (m/start-connection! token)
        init-state {:connection connection-ch
                    :event event-ch
                    :messaging messaging-ch}]
    (reset! state init-state)
    (save-data)
    (e/message-pump! event-ch #'handle-event)
    (m/stop-connection! messaging-ch)
    (c/disconnect-bot! connection-ch)))

(let [old-ns *ns*]
  (require 'discljord.connections.impl)
  (in-ns 'discljord.connections.impl)
  (do
   (ns-unmap *ns* 'handle-shard-communication!)
   (defmulti handle-shard-communication!
     "Processes a communication `event` on the given `shard` for side effects.
      Returns a map with the new :shard and bot-evel :effects to process."
     (fn [shard url event]
       (first event)))

   (defmethod handle-shard-communication! :default
     [shard url event]
     (taoensso.timbre/warn "Unknown communication event recieved on a shard" event)
     {:shard shard
      :effects []})

   (defmethod handle-shard-communication! :guild-request-members
     [{:keys [heartbeat-ch event-ch] :as shard}
      url [_ & {:keys [guild-id query limit] :or {query "" limit 0}}]]
     (when guild-id
       (let [msg (clojure.data.json/write-str {:op 8
                                  :d {"guild_id" guild-id
                                      "query" query
                                      "limit" limit}})]
         (if-not (> (count msg) 4096)
           (do
             (taoensso.timbre/trace "Sending message to retrieve guild members from guild"
                        guild-id "over shard" (:id shard)
                        "with query" query)
             (gniazdo.core/send-msg (:ws (:websocket shard))
                          msg))
           (taoensso.timbre/error "Message for guild-request-members was too large on shard" (:id shard)
                      "Check to make sure that your query is of a reasonable size."))))
     {:shard shard
      :effects []})

   (defmethod handle-shard-communication! :status-update
     [{:keys [heartbeat-ch event-ch] :as shard}
      url [_ & {:keys [idle-since activity status afk]
                :or {afk false
                     status "online"}}]]
     (let [msg (clojure.data.json/write-str {:op 3
                                :d {"since" idle-since
                                    "game" activity
                                    "status" status
                                    "afk" afk}})]
       (if-not (> (count msg) 4096)
         (do
           (taoensso.timbre/trace "Sending status update over shard" (:id shard))
           (gniazdo.core/send-msg (:ws (:websocket shard))
                        msg))
         (taoensso.timbre/error "Message for status-update was too large."
                    "Use create-activity to create a valid activity"
                    "and select a reasonably-sized status message.")))
     {:shard shard
      :effects []})

   (defmethod handle-shard-communication! :voice-state-update
     [shard heartbeat-ch url event-ch [_ & {:keys [guild-id channel-id mute deaf]
                                            :or {mute false
                                                 deaf false}}]]
     (let [msg (clojure.data.json/write-str {:op 4
                                :d {"guild_id" guild-id
                                    "channel_id" channel-id
                                    "self_mute" mute
                                    "self_deaf" deaf}})]
       (if-not (> (count msg) 4096)
         (do
           (taoensso.timbre/trace "Sending voice-state-update over shard" (:id shard))
           (gniazdo.core/send-msg (:ws (:websocket shard))
                        msg))
         (taoensso.timbre/error "Message for voice-state-update was too large."
                    "This should not occur if you are using valid types for the keys.")))
     {:shard shard
      :effects []})

   (defmulti handle-connection-event!
     ""
     (fn [shard url [event-type & event-data]]
       event-type))

   (defmethod handle-connection-event! :disconnect
     [{:keys [heartbeat-ch communication-ch websocket id]} _ _]
     (when heartbeat-ch
       (clojure.core.async/close! heartbeat-ch))
     (clojure.core.async/close! communication-ch)
     (when websocket
       (gniazdo.core/close (:ws websocket))
       (.stop (:client websocket)))
     (taoensso.timbre/info "Disconnecting shard"
               id
               "and closing connection")
     {:shard nil
      :effects []})

   (defmethod handle-connection-event! :connect
     [{:keys [heartbeat-ch event-ch] :as shard} url _]
     (taoensso.timbre/info "Connecting shard" (:id shard))
     (when heartbeat-ch
       (clojure.core.async/close! heartbeat-ch))
     (let [event-ch (clojure.core.async/chan 100)
           websocket (try (connect-websocket! buffer-size url event-ch)
                          (catch Exception err
                            (taoensso.timbre/warn "Failed to connect a websocket" err)
                            nil))]
       (when-not websocket
         (clojure.core.async/put! event-ch [:disconnect nil "Failed to connect"]))
       {:shard (assoc (dissoc shard
                              :heartbeat-ch
                              :requested-disconnect)
                      :websocket websocket
                      :event-ch event-ch)
        :effects []}))

   (defn step-shard!
     "Starts a process to step a `shard`, handling side-effects.
  Returns a channel which will have a map with the new `:shard` and a vector of
  `:effects` for the entire bot to respond to placed on it after the next item
  the socket may respond to occurs."
     [shard url token]
     (taoensso.timbre/trace "Stepping shard" (:id shard) shard)
     (let [{:keys [event-ch websocket heartbeat-ch communication-ch connections-ch] :or {heartbeat-ch (clojure.core.async/chan)}} shard
           connections-fn (fn [event]
                            (taoensso.timbre/debug "Received connection event on shard" (:id shard))
                            (handle-connection-event! shard url event))
           communication-fn (fn [[event-type event-data :as value]]
                              (taoensso.timbre/debug "Recieved communication value" value "on shard" (:id shard))
                              (handle-shard-communication! shard value))
           heartbeat-fn (fn []
                          (if (:ack shard)
                            (do (taoensso.timbre/trace "Sending heartbeat payload on shard" (:id shard))
                                (gniazdo.core/send-msg (:ws websocket)
                                             (clojure.data.json/write-str {:op 1
                                                              :d (:seq shard)}))
                                {:shard (dissoc shard :ack)
                                 :effects []})
                            (do
                              (when websocket
                                (gniazdo.core/close (:ws websocket))
                                (.stop (:client websocket)))
                              (taoensso.timbre/info "Reconnecting due to zombie heartbeat on shard" (:id shard))
                              (clojure.core.async/close! heartbeat-ch)
                              (clojure.core.async/put! connections-ch [:connect])
                              {:shard (assoc (dissoc shard :heartbeat-ch)
                                             :requested-disconnect true)
                               :effects []})))
           event-fn (fn [event]
                      (let [{:keys [shard effects]} (handle-websocket-event shard event)
                            shard-map (reduce
                                       (fn [{:keys [shard effects]} new-effect]
                                         (let [old-effects effects
                                               {:keys [shard effects]}
                                               (handle-shard-fx! heartbeat-ch url token shard new-effect)
                                               new-effects (vec (concat old-effects effects))]
                                           {:shard shard
                                            :effects new-effects}))
                                       {:shard shard
                                        :effects []}
                                       effects)]
                        shard-map))]
       (clojure.core.async/go
         (if (:websocket shard)
           (clojure.core.async/alt!
             connections-ch ([event] (connections-fn event))
             communication-ch ([args] (communication-fn args))
             heartbeat-ch (heartbeat-fn)
             event-ch ([event] (event-fn event))
             :priority true)
           (clojure.core.async/alt!
             connections-ch ([event] (connections-fn event))
             event-ch ([event] (event-fn event))
             :priority true)))))

   (defn make-shard
     "Creates a new shard with the given `id` and `shard-count`."
     [intents id shard-count]
     {:id id
      :count shard-count
      :intents intents
      :event-ch (clojure.core.async/chan 100)
      :communication-ch (clojure.core.async/chan 100)
      :connections-ch (clojure.core.async/chan 1)})

   (defmethod handle-shard-fx! :reconnect
     [heartbeat-ch url token shard event]
     (when (:websocket shard)
       (gniazdo.core/close (:ws (:websocket shard)))
       (.stop (:client (:websocket shard))))
     (when (:invalid-session shard)
       (taoensso.timbre/warn "Got invalid session payload, reconnecting shard" (:id shard)))
     (when (:heartbeat-ch shard)
       (clojure.core.async/close! (:heartbeat-ch shard)))
     (let [retries (inc (or (:retries shard) 0))
           retry-wait (min (* 5100 (* retries retries)) (* 15 60 1000))]
       (taoensso.timbre/debug "Will try to connect in" (int (/ retry-wait 1000)) "seconds")
       (after-timeout! (fn []
                         (taoensso.timbre/debug "Sending reconnect signal to shard" (:id shard))
                         (clojure.core.async/put! (:connections-ch shard) [:connect]))
                       retry-wait)
       (let [shard (if (:invalid-session shard)
                     (dissoc shard :session-id)
                     shard)]
         {:shard (assoc (dissoc shard
                                :heartbeat-ch
                                :websocket)
                        :retries retries
                        :requested-disconnect true)
          :effects []})))
   (defn connect-shards!
     "Connects a set of shards with the given `shard-ids`.
  Returns nil."
     [output-ch communication-ch url token intents shard-count shard-ids]
     (let [shards (mapv #(make-shard intents % shard-count) shard-ids)]
       (clojure.core.async/go-loop [shards shards
                   shard-chs (mapv #(step-shard! % url token) shards)]
         (if (some identity shard-chs)
           (do (taoensso.timbre/trace "Waiting for a shard to complete a step")
               (let [[v p] (clojure.core.async/alts! (conj (remove nil? shard-chs)
                                          communication-ch))]
                 (if (= communication-ch p)
                   (let [[shards shard-chs & [effects]] (handle-communication! shards shard-chs v)]
                     (if (seq effects)
                       (let [[shards shard-chs] (reduce (fn [[shards shard-chs] effect]
                                                          (handle-bot-fx! output-ch
                                                                          url token
                                                                          shards shard-chs
                                                                          nil effect))
                                                        [shards shard-chs]
                                                        effects)]
                         (recur shards shard-chs))
                       (recur shards shard-chs)))
                   (let [idx (index-of p shard-chs)
                         effects (:effects v)
                         shards (assoc shards idx (:shard v))
                         shard-chs (assoc shard-chs idx (when (:shard v)
                                                          (step-shard! (:shard v) url token)))
                         [shards shard-chs] (reduce (fn [[shards shard-chs] effect]
                                                      (handle-bot-fx! output-ch
                                                                      url token
                                                                      shards shard-chs
                                                                      idx effect))
                                                    [shards shard-chs]
                                                    effects)]
                     (recur shards shard-chs)))))
           (do (taoensso.timbre/trace "Exiting the shard loop")
               (clojure.core.async/put! output-ch [:disconnect]))))
       (doseq [[idx shard] (map-indexed vector shards)]
         (clojure.core.async/put! (:connections-ch shard) [:connect]))
       (after-timeout! #(clojure.core.async/put! output-ch [:connected-all-shards]) (+ (* (dec (count shard-ids)) 5100)
                                                                      100))
       nil))

   (defmethod handle-bot-fx! :re-shard
     [output-ch url token shards shard-chs shard-idx [_ event-type event]]
     (taoensso.timbre/info "Stopping all current shards to prepare for re-shard.")
     (clojure.core.async/put! output-ch [:re-shard])
     (run! #(clojure.core.async/put! (:connections-ch %) [:disconnect]) shards)
     (run! #(clojure.core.async/<!! (step-shard! % url token))
           (remove nil?
                   (map (comp :shard clojure.core.async/<!!) shard-chs)))
     (if *handle-re-shard*
       (let [{:keys [url shard-count session-start-limit]} (get-websocket-gateway discljord.http/gateway-url token)]
         (when (> shard-count (:remaining session-start-limit))
           (taoensso.timbre/fatal "Asked to re-shard client, but too few session starts remain.")
           (throw (ex-info "Unable to re-shard client, too few session starts remaining."
                           {:token token
                            :shards-requested shard-count
                            :remaining-starts (:remaining session-start-limit)
                            :reset-after (:reset-after session-start-limit)})))
         (let [shards (mapv #(make-shard (:intents (nth shards shard-idx)) % shard-count)
                            (range shard-count))
               shard-chs (mapv #(step-shard! % url token) shards)]
           (doseq [[idx shard] (map-indexed vector shards)]
             (after-timeout! #(clojure.core.async/put! (:connections-ch shard) [:connect]) (* idx 5100)))
           (after-timeout! #(clojure.core.async/put! output-ch [:connected-all-shards]) (+ (* (dec shard-count) 5100)
                                                                          100))
           [shards shard-chs]))
       [nil nil]))

   (defmethod handle-bot-fx! :disconnect
     [output-ch url token shards shard-chs shard-idx _]
     (taoensso.timbre/info
      (if shard-idx
        (str "Full disconnect triggered from shard" shard-idx)
        "Full disconnect triggered from input"))
     (clojure.core.async/put! output-ch [:disconnect])
     (run! #(clojure.core.async/put! (:connections-ch %) [:disconnect]) shards)
     (run! #(clojure.core.async/<!! (step-shard! % url token))
           (remove nil?
                   (map (comp :shard clojure.core.async/<!!) shard-chs)))
     [nil nil])

   (defmethod handle-communication! :disconnect
     [shards shard-chs _]
     (run! #(clojure.core.async/put! (:connections-ch %) [:disconnect]) shards)
     [shards shard-chs [[:disconnect]]]))
  (in-ns old-ns))
