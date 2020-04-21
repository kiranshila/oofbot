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
    (def buffer-size
      "The suggested size of a buffer; default: 4MiB."
      4194304)

    (defmulti handle-websocket-event
      "Updates a `shard` based on shard events.
  Takes a `shard` and a shard event vector and returns a map of the new state of
  the shard and zero or more events to process."
      (fn [shard [event-type & args]]
        event-type))

    (def new-session-stop-code?
      "Set of stop codes after which a resume isn't possible."
      #{4003 4004 4007 4009})

    (defn should-resume?
      "Returns if a shard should try to resume."
      [shard]
      (when (:stop-code shard)
        (and (not (new-session-stop-code? (:stop-code shard)))
             (:seq shard)
             (:session-id shard)
             (not (:unresumable shard)))))

    (defmethod handle-websocket-event :connect
      [shard [_]]
      {:shard shard
       :effects [(if (should-resume? shard)
                   [:resume]
                   [:identify])]})

    (def ^:dynamic *stop-on-fatal-code*
      "Bind to to true to disconnect the entire bot after a fatal stop code."
      false)

    (def fatal-code?
      "Set of stop codes which after recieving, discljord will disconnect all shards."
      ;; NOTE(Joshua): the 4013 code is for invalid intents. In future that should
      ;;               be called out specifically since it's a user error.
      #{4001 4002 4003 4004 4005 4008 4010 4013})

    (def re-shard-stop-code?
      "Stop codes which Discord will send when the bot needs to be re-sharded."
      #{4011})

    (defmethod handle-websocket-event :disconnect
      [shard [_ stop-code msg]]
      (if (and shard
               (not (:requested-disconnect shard)))
        {:shard (assoc (dissoc shard
                               :websocket)
                       :stop-code stop-code
                       :disconnect-msg msg)
         :effects [(cond
                     (re-shard-stop-code? stop-code) [:re-shard]
                     (and *stop-on-fatal-code*
                          (fatal-code? stop-code))   [:disconnect-all]
                     :otherwise                      [:reconnect])]}
        {:shard (when (:requested-disconnect shard)
                  (dissoc shard
                          :websocket))
         :effects []}))

    (defmethod handle-websocket-event :error
      [shard [_ err]]
      {:shard shard
       :effects [[:error err]]})

    (defmethod handle-websocket-event :send-debug-effect
      [shard [_ & effects]]
      {:shard shard
       :effects (vec effects)})

    (def ^:private payload-id->payload-key
      "Map from payload type ids to the payload keyword."
      {0 :event-dispatch
       1 :heartbeat
       7 :reconnect
       9 :invalid-session
       10 :hello
       11 :heartbeat-ack})

    (defmulti handle-payload
      "Update a `shard` based on a message.
  Takes a `shard` and `msg` and returns a map with a :shard and an :effects
  vector."
      (fn [shard msg]
        (payload-id->payload-key (:op msg))))

    (defmethod handle-websocket-event :message
      [shard [_ msg]]
      (handle-payload shard (clean-json-input (json/read-str msg))))

    (defmulti handle-discord-event
      "Handles discord events for a specific shard, specifying effects."
      (fn [shard event-type event]
        event-type))

    (defmethod handle-discord-event :default
      [shard event-type event]
      {:shard shard
       :effects [[:send-discord-event event-type event]]})

    (defmethod handle-discord-event :ready
      [shard event-type event]
      {:shard (assoc (dissoc shard
                             :retries
                             :stop-code
                             :disconnect-msg
                             :invalid-session
                             :unresumable)
                     :session-id (:session-id event))
       :effects [[:send-discord-event event-type event]]})

    (defmethod handle-payload :event-dispatch
      [shard {:keys [d t s] :as msg}]
      (handle-discord-event (assoc shard :seq s) (json-keyword t) d))

    (defmethod handle-payload :heartbeat
      [shard msg]
      {:shard shard
       :effects [[:send-heartbeat]]})

    (defmethod handle-payload :reconnect
      [shard {d :d}]
      {:shard shard
       :effects [[:reconnect]]})

    (defmethod handle-payload :invalid-session
      [shard {d :d}]
      {:shard (assoc (dissoc shard
                             :session-id
                             :seq)
                     :invalid-session true
                     :unresumable (not d))
       :effects [[:reconnect]]})

    (defmethod handle-payload :hello
      [shard {{:keys [heartbeat-interval]} :d}]
      {:shard shard
       :effects [[:start-heartbeat heartbeat-interval]]})

    (defmethod handle-payload :heartbeat-ack
      [shard msg]
      {:shard (assoc shard :ack true)
       :effects []})

    (defn connect-websocket!
      "Connect a websocket to the `url` that puts all events onto the `event-ch`.
  Events are represented as vectors with a keyword for the event type and then
  event data as the rest of the vector based on the type of event.

  | Type          | Data |
  |---------------+------|
  | `:connect`    | None.
  | `:disconnect` | Stop code, string message.
  | `:error`      | Error value.
  | `:message`    | String message."
      [buffer-size url event-ch]
      (log/debug "Starting websocket of size" buffer-size "at url" url)
      (let [client (WebSocketClient. (doto (SslContextFactory.)
                                       (.setEndpointIdentificationAlgorithm "HTTPS")))]
        (doto (.getPolicy client)
          (.setMaxTextMessageSize buffer-size)
          (.setMaxBinaryMessageSize buffer-size))
        (doto client
          (.setMaxTextMessageBufferSize buffer-size)
          (.setMaxBinaryMessageBufferSize buffer-size)
          (.start))
        {:ws (ws/connect
              url
              :client client
              :on-connect (fn [_]
                            (log/trace "Websocket connected")
                            (a/put! event-ch [:connect]))
              :on-close (fn [stop-code msg]
                          (log/debug "Websocket closed with code:" stop-code "and message:" msg)
                          (a/put! event-ch [:disconnect stop-code msg]))
              :on-error (fn [err]
                          (log/warn "Websocket errored" err)
                          (a/put! event-ch [:error err]))
              :on-receive (fn [msg]
                            (log/trace "Websocket recieved message:" msg)
                            (a/put! event-ch [:message msg])))
         :client client}))

    (defmulti handle-shard-fx!
      "Processes an `event` on a given `shard` for side effects.
  Returns a map with the new :shard and bot-level :effects to process."
      (fn [heartbeat-ch url token shard event]
        (first event)))

    (ns-unmap *ns* 'handle-shard-communication!)
    (defmulti handle-shard-communication!
      "Processes a communication `event` on the given `shard` for side effects.
  Returns a map with the new :shard and bot-evel :effects to process."
      (fn [shard url event]
        (first event)))

    (defmethod handle-shard-communication! :default
      [shard url event]
      (log/warn "Unknown communication event recieved on a shard" event)
      {:shard shard
       :effects []})

    (defmethod handle-shard-communication! :guild-request-members
      [{:keys [heartbeat-ch event-ch] :as shard}
       url [_ & {:keys [guild-id query limit] :or {query "" limit 0}}]]
      (when guild-id
        (let [msg (json/write-str {:op 8
                                   :d {"guild_id" guild-id
                                       "query" query
                                       "limit" limit}})]
          (if-not (> (count msg) 4096)
            (do
              (log/trace "Sending message to retrieve guild members from guild"
                         guild-id "over shard" (:id shard)
                         "with query" query)
              (ws/send-msg (:ws (:websocket shard))
                           msg))
            (log/error "Message for guild-request-members was too large on shard" (:id shard)
                       "Check to make sure that your query is of a reasonable size."))))
      {:shard shard
       :effects []})

    (defmethod handle-shard-communication! :status-update
      [{:keys [heartbeat-ch event-ch]:as shard}
       url [_ & {:keys [idle-since activity status afk]
                 :or {afk false
                      status "online"}}]]
      (let [msg (json/write-str {:op 3
                                 :d {"since" idle-since
                                     "game" activity
                                     "status" status
                                     "afk" afk}})]
        (if-not (> (count msg) 4096)
          (do
            (log/trace "Sending status update over shard" (:id shard))
            (ws/send-msg (:ws (:websocket shard))
                         msg))
          (log/error "Message for status-update was too large."
                     "Use create-activity to create a valid activity"
                     "and select a reasonably-sized status message.")))
      {:shard shard
       :effects []})

    (defmethod handle-shard-communication! :voice-state-update
      [shard heartbeat-ch url event-ch [_ & {:keys [guild-id channel-id mute deaf]
                                             :or {mute false
                                                  deaf false}}]]
      (let [msg (json/write-str {:op 4
                                 :d {"guild_id" guild-id
                                     "channel_id" channel-id
                                     "self_mute" mute
                                     "self_deaf" deaf}})]
        (if-not (> (count msg) 4096)
          (do
            (log/trace "Sending voice-state-update over shard" (:id shard))
            (ws/send-msg (:ws (:websocket shard))
                         msg))
          (log/error "Message for voice-state-update was too large."
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
        (a/close! heartbeat-ch))
      (a/close! communication-ch)
      (when websocket
        (ws/close (:ws websocket))
        (.stop (:client websocket)))
      (log/info "Disconnecting shard"
                id
                "and closing connection")
      {:shard nil
       :effects []})

    (defmethod handle-connection-event! :connect
      [{:keys [heartbeat-ch event-ch] :as shard} url _]
      (log/info "Connecting shard" (:id shard))
      (when heartbeat-ch
        (a/close! heartbeat-ch))
      (let [event-ch (a/chan 100)
            websocket (try (connect-websocket! buffer-size url event-ch)
                           (catch Exception err
                             (log/warn "Failed to connect a websocket" err)
                             nil))]
        (when-not websocket
          (a/put! event-ch [:disconnect nil "Failed to connect"]))
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
      (log/trace "Stepping shard" (:id shard) shard)
      (let [{:keys [event-ch websocket heartbeat-ch communication-ch connections-ch] :or {heartbeat-ch (a/chan)}} shard
            connections-fn (fn [event]
                             (log/debug "Received connection event on shard" (:id shard))
                             (handle-connection-event! shard url event))
            communication-fn (fn [[event-type event-data :as value]]
                               (log/debug "Recieved communication value" value "on shard" (:id shard))
                               (handle-shard-communication! shard value))
            heartbeat-fn (fn []
                           (if (:ack shard)
                             (do (log/trace "Sending heartbeat payload on shard" (:id shard))
                                 (ws/send-msg (:ws websocket)
                                              (json/write-str {:op 1
                                                               :d (:seq shard)}))
                                 {:shard (dissoc shard :ack)
                                  :effects []})
                             (do
                               (when websocket
                                 (ws/close (:ws websocket))
                                 (.stop (:client websocket)))
                               (log/info "Reconnecting due to zombie heartbeat on shard" (:id shard))
                               (a/close! heartbeat-ch)
                               (a/put! connections-ch [:connect])
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
        (a/go
          (if (:websocket shard)
            (a/alt!
              connections-ch ([event] (connections-fn event))
              communication-ch ([args] (communication-fn args))
              heartbeat-ch (heartbeat-fn)
              event-ch ([event] (event-fn event))
              :priority true)
            (a/alt!
              connections-ch ([event] (connections-fn event))
              event-ch ([event] (event-fn event))
              :priority true)))))

    (defn get-websocket-gateway
      "Gets the shard count and websocket endpoint from Discord's API.

  Takes the `url` of the gateway and the `token` of the bot.

  Returns a map with the keys :url, :shard-count, and :session-start limit, or
  nil in the case of an error."
      [url token]
      (if-let [result
               (try
                 (when-let [response (:body @(http/get url
                                                       {:headers
                                                        {"Authorization" token}}))]
                   (when-let [json-body (clean-json-input (json/read-str response))]
                     {:url (:url json-body)
                      :shard-count (:shards json-body)
                      :session-start-limit (:session-start-limit json-body)}))
                 (catch Exception e
                   (log/error e "Failed to get websocket gateway")
                   nil))]
        (when (:url result)
          result)))

    (defn make-shard
      "Creates a new shard with the given `id` and `shard-count`."
      [intents id shard-count]
      {:id id
       :count shard-count
       :intents intents
       :event-ch (a/chan 100)
       :communication-ch (a/chan 100)
       :connections-ch (a/chan 1)})

    (defn after-timeout!
      "Calls a function of no arguments after the given `timeout`.
  Returns a channel which will have the return value of the function put on it."
      [f timeout]
      (a/go (a/<! (a/timeout timeout))
            (f)))

    (defmethod handle-shard-fx! :start-heartbeat
      [heartbeat-ch url token shard [_ heartbeat-interval]]
      (let [heartbeat-ch (a/chan (a/sliding-buffer 1))]
        (log/debug "Starting a heartbeat with interval" heartbeat-interval "on shard" (:id shard))
        (a/put! heartbeat-ch :heartbeat)
        (a/go-loop []
          (a/<! (a/timeout heartbeat-interval))
          (when (a/>! heartbeat-ch :heartbeat)
            (log/trace "Requesting heartbeat on shard" (:id shard))
            (recur)))
        {:shard (assoc shard
                       :heartbeat-ch heartbeat-ch
                       :ack true)
         :effects []}))

    (defmethod handle-shard-fx! :send-heartbeat
      [heartbeat-ch url token shard event]
      (when heartbeat-ch
        (log/trace "Responding to requested heartbeat signal")
        (a/put! heartbeat-ch :heartbeat))
      {:shard shard
       :effects []})

    (def identify-limiter (agent nil))
    (defn run-on-agent-with-limit
      "Runs the given function on the agent, then other actions wait `millis`."
      [a f millis]
      (send-off a (fn [_]
                    (f)
                    (a/<!! (a/timeout millis))
                    nil)))

    (def ^:private intent->intent-int
      {:guilds (bit-shift-left 1 0)
       :guild-members (bit-shift-left 1 1)
       :guild-bans (bit-shift-left 1 2)
       :guild-emojis (bit-shift-left 1 3)
       :guild-integrations (bit-shift-left 1 4)
       :guild-webhooks (bit-shift-left 1 5)
       :guild-invites (bit-shift-left 1 6)
       :guild-voice-states (bit-shift-left 1 7)
       :guild-presences (bit-shift-left 1 8)
       :guild-messages (bit-shift-left 1 9)
       :guild-message-reactions (bit-shift-left 1 10)
       :guild-message-typing (bit-shift-left 1 11)
       :direct-messages (bit-shift-left 1 12)
       :direct-message-reactions (bit-shift-left 1 13)
       :direct-message-typing (bit-shift-left 1 14)})
    (defn- intents->intent-int
      "Takes a set of intents and creates an intents-int to represent that set."
      [intents]
      (reduce #(bit-or %1 (intent->intent-int %2))
              0
              intents))

    (defmethod handle-shard-fx! :identify
      [heartbeat-ch url token shard event]
      (run-on-agent-with-limit
       identify-limiter
       (fn []
         (log/debug "Sending identify payload for shard" (:id shard))
         (let [payload {"token" token
                        "properties" {"$os" "linux"
                                      "$browser" "discljord"
                                      "$device" "discljord"}
                        "compress" false
                        "large_threshold" 50
                        "shard" [(:id shard) (:count shard)]}
               payload (if-let [intents (:intents shard)]
                         (assoc payload "intents" (intents->intent-int intents))
                         payload)]
           (log/trace "Identify payload:" payload)
           (ws/send-msg (:ws (:websocket shard))
                        (json/write-str {:op 2
                                         :d payload}))))
       5100)
      {:shard shard
       :effects []})

    (defmethod handle-shard-fx! :resume
      [heartbeat-ch url token shard event]
      (log/debug "Sending resume payload for shard" (:id shard)
                 "with session" (:session-id shard) "and seq" (:seq shard))
      (ws/send-msg (:ws (:websocket shard))
                   (json/write-str {:op 6
                                    :d {"token" token
                                        "session_id" (:session-id shard)
                                        "seq" (:seq shard)}}))
      {:shard shard
       :effects []})

    (defmethod handle-shard-fx! :reconnect
      [heartbeat-ch url token shard event]
      (when (:websocket shard)
        (ws/close (:ws (:websocket shard)))
        (.stop (:client (:websocket shard))))
      (when (:invalid-session shard)
        (log/warn "Got invalid session payload, reconnecting shard" (:id shard)))
      (when (:heartbeat-ch shard)
        (a/close! (:heartbeat-ch shard)))
      (let [retries (inc (or (:retries shard) 0))
            retry-wait (min (* 5100 (* retries retries)) (* 15 60 1000))]
        (log/debug "Will try to connect in" (int (/ retry-wait 1000)) "seconds")
        (after-timeout! (fn []
                          (log/debug "Sending reconnect signal to shard" (:id shard))
                          (a/put! (:connections-ch shard) [:connect]))
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

    (defmethod handle-shard-fx! :re-shard
      [heartbeat-ch url token shard event]
      {:shard shard
       :effects [[:re-shard]]})

    (defmethod handle-shard-fx! :error
      [heartbeat-ch url token shard [_ err]]
      (log/error err "Error encountered on shard" (:id shard))
      {:shard shard
       :effects []})

    (defmethod handle-shard-fx! :send-discord-event
      [heartbeat-ch url token shard [_ event-type event]]
      (log/trace "Shard" (:id shard) "recieved discord event:" event)
      {:shard shard
       :effects [[:discord-event event-type event]]})

    (defmethod handle-shard-fx! :disconnect-all
      [heartbeat-ch url token shard _]
      {:shard shard
       :effects [[:disconnect]]})

    (defmulti handle-bot-fx!
      "Handles a bot-level side effect triggered by a shard.
  This method should never block, and should not do intense computation. Takes a
  place to output events to the library user, the url to connect sockets, the
  bot's token, the vector of shards, a vector of channels which resolve to the
  shard's next state, the index of the shard the effect is from, and the effect.
  Returns a vector of the vector of shards and the vector of shard channels."
      (fn [output-ch url token shards shard-chs shard-idx effect]
        (first effect)))

    (defmulti handle-communication!
      "Handles communicating to the `shards`.
  Takes an `event` vector, a vector of `shards`, and a vector of channels which
  resolve to each shard's next state, and returns a vector of the vector of
  shards and the vector of channels."
      (fn [shards shard-chs event]
        (first event)))

    (defn- index-of
      "Fetches the index of the first occurent of `elt` in `coll`.
  Returns nil if it's not found."
      [elt coll]
      (first (first (filter (comp #{elt} second) (map-indexed vector coll)))))

    (defn connect-shards!
      "Connects a set of shards with the given `shard-ids`.
  Returns nil."
      [output-ch communication-ch url token intents shard-count shard-ids]
      (let [shards (mapv #(make-shard intents % shard-count) shard-ids)]
        (a/go-loop [shards shards
                    shard-chs (mapv #(step-shard! % url token) shards)]
          (if (some identity shard-chs)
            (do (log/trace "Waiting for a shard to complete a step")
                (let [[v p] (a/alts! (conj (remove nil? shard-chs)
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
            (do (log/trace "Exiting the shard loop")
                (a/put! output-ch [:disconnect]))))
        (doseq [[idx shard] (map-indexed vector shards)]
          (a/put! (:connections-ch shard) [:connect]))
        (after-timeout! #(a/put! output-ch [:connected-all-shards]) (+ (* (dec (count shard-ids)) 5100)
                                                                       100))
        nil))

    (defmethod handle-bot-fx! :discord-event
      [output-ch url token shards shard-chs shard-idx [_ event-type event]]
      (a/put! output-ch [event-type event])
      [shards shard-chs])

    (def ^:dynamic *handle-re-shard*
      "Determines if the bot will re-shard on its own, or require user coordination.
  If bound to true and a re-shard occurs, the bot will make a request to discord
  for the new number of shards to connect with and then connect them. If bound
  to false, then a :re-shard event will be sent to the user library and all
  shards will be disconnected."
      true)

    (defmethod handle-bot-fx! :re-shard
      [output-ch url token shards shard-chs shard-idx [_ event-type event]]
      (log/info "Stopping all current shards to prepare for re-shard.")
      (a/put! output-ch [:re-shard])
      (run! #(a/put! (:connections-ch %) [:disconnect]) shards)
      (run! #(a/<!! (step-shard! % url token))
            (remove nil?
                    (map (comp :shard a/<!!) shard-chs)))
      (if *handle-re-shard*
        (let [{:keys [url shard-count session-start-limit]} (get-websocket-gateway gateway-url token)]
          (when (> shard-count (:remaining session-start-limit))
            (log/fatal "Asked to re-shard client, but too few session starts remain.")
            (throw (ex-info "Unable to re-shard client, too few session starts remaining."
                            {:token token
                             :shards-requested shard-count
                             :remaining-starts (:remaining session-start-limit)
                             :reset-after (:reset-after session-start-limit)})))
          (let [shards (mapv #(make-shard (:intents (nth shards shard-idx)) % shard-count)
                             (range shard-count))
                shard-chs (mapv #(step-shard! % url token) shards)]
            (doseq [[idx shard] (map-indexed vector shards)]
              (after-timeout! #(a/put! (:connections-ch shard) [:connect]) (* idx 5100)))
            (after-timeout! #(a/put! output-ch [:connected-all-shards]) (+ (* (dec shard-count) 5100)
                                                                           100))
            [shards shard-chs]))
        [nil nil]))

    (defmethod handle-bot-fx! :disconnect
      [output-ch url token shards shard-chs shard-idx _]
      (log/info
       (if shard-idx
         (str "Full disconnect triggered from shard" shard-idx)
         "Full disconnect triggered from input"))
      (a/put! output-ch [:disconnect])
      (run! #(a/put! (:connections-ch %) [:disconnect]) shards)
      (run! #(a/<!! (step-shard! % url token))
            (remove nil?
                    (map (comp :shard a/<!!) shard-chs)))
      [nil nil])

    (defmethod handle-communication! :disconnect
      [shards shard-chs _]
      (run! #(a/put! (:connections-ch %) [:disconnect]) shards)
      [shards shard-chs [[:disconnect]]])

    (defmethod handle-communication! :send-debug-event
      [shards shard-chs [_ shard-id event]]
      (a/put! (:event-ch (get shards shard-id)) event)
      [shards shard-chs])

    (defn get-shard-from-guild
      [guild-id guild-count]
      (mod (bit-shift-right (Long. guild-id) 22) guild-count))

    (defmethod handle-communication! :guild-request-members
      [shards shard-chs [_ & {:keys [guild-id]} :as event]]
      (when guild-id
        (let [shard-id (get-shard-from-guild guild-id (:count (first (remove nil? shards))))
              shard (first (filter (comp #{shard-id} :id) shards))]
          (if shard
            (a/put! (:communication-ch shard) event)
            (when (seq (remove nil? shards))
              (log/error "Attempted to request guild members for a guild with no"
                         "matching shard in this process.")))))
      [shards shard-chs])

    (defmethod handle-communication! :status-update
      [shards shard-chs event]
      (when-let [shard (first (remove nil? shards))]
        (a/put! (:communication-ch shard) event))
      [shards shard-chs])

    (defmethod handle-communication! :voice-state-update
      [shards shard-chs [_ & {:keys [guild-id]}
                         :as event]]
      (when guild-id
        (let [shard-id (get-shard-from-guild guild-id (:count (first (remove nil? shards))))
              shard (first (filter (comp #{shard-id} :id) shards))]
          (if shard
            (a/put! (:communication-ch shard) event)
            (when (seq (remove nil? shards))
              (log/error "Attempted to send voice-state-update for a guild with no"
                         "matching shard in this process.")))))
      [shards shard-chs])
    )
  (in-ns old-ns))
