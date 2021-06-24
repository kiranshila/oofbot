(ns oofbot.core
  (:require [discljord.connections :as c]
            [discljord.messaging :as m]
            [discljord.events :as e]
            [clojure.core.async :as a]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [java.time Instant])
  (:gen-class))

;;;;;;;;;;;;;; BOT SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;

;; How many users in the "top" list
(def default-top-count 5)
(def top-count-max 25)
;; Timeout to query for new usename
(def username-timeout 3600000)
;; Data saving timeout
(def save-timeout 300000)
;; File where bot token is stored
(def token-filename "token.txt")
;; File where bot data is stored
(def data-filename "oof-data.edn")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grabs the token from the file
(def token (s/trim (slurp token-filename)))

;; Writes data to an edn file
(defn write-edn [data filename]
  (with-open [f (io/writer filename)]
    (binding [*out* f]
      (pr data))))

;; State Object
(defonce state (atom nil))

;; Bot User ID
(defonce bot-id (atom nil))

;; Atom for oofbot data
(def oof-data
  (if (.exists (io/as-file data-filename))
    (atom (edn/read-string (slurp data-filename)))
    (atom nil)))

;; Image base url
(def discord-base "https://cdn.discordapp.com/")

(defn user-avatar-url [user-id user-avatar]
  (str discord-base "avatars/" user-id "/" user-avatar
       (if (= "a_" (subs user-avatar 0 2))
         ".gif"
         ".png")))

;; Embed Template
(defn embed-template []
  {:color 0xAD2D3C
   :footer {:text "Built in Clojure with Discljord"}})

;; Check to see if a given str is a valid oof
(defn is-oof? [input]
  (some? (re-seq #"(?i)\bo+of\b" input)))

(defn get-top-count [guild-id]
  (get-in @oof-data [guild-id :top-count] default-top-count))

;; Get a vector of 5 messages before 
(defn get-messages-before [before-id channel-id]
  @(m/get-channel-messages! (:messaging @state) channel-id
                            :before before-id
                            :limit 5))

;; Gets new username or nickname
(defn get-new-user-or-nick [user-id guild-id]
  (let [member @(m/get-guild-member! (:messaging @state) guild-id user-id)]
    (if (:nick member)
      (:nick member)
      (get-in member [:user :username]))))

(defn get-username [user-id guild-id]
  (let [this-data @oof-data
        last-update (get-in this-data [guild-id user-id :last-updated])
        last-username (get-in this-data [guild-id user-id :username])]
    (if (and last-update ; check if nil
             last-username ; check if nil
             (<= (- (inst-ms (Instant/now))
                    last-update)
                 username-timeout))
      last-username
      ;; Never updated, update now
      (let [new-username (get-new-user-or-nick user-id guild-id)]
        (swap! oof-data assoc-in [guild-id user-id :username] new-username)
        (swap! oof-data assoc-in [guild-id user-id :last-updated] (inst-ms (Instant/now)))
        new-username))))

;; Get the user avatar has from the data, update data is we don't have it
(defn get-user-avatar [user-id guild-id]
  (if-let [user-avatar (get-in @oof-data [guild-id user-id :avatar])]
    user-avatar
    (let [new-user-avatar (:avatar @(m/get-user! (:messaging @state) user-id))]
      (swap! oof-data assoc-in [guild-id user-id :avatar] new-user-avatar)
      new-user-avatar)))

;; Get a message from a message-id and channel-id
(defn get-message [message-id channel-id]
  (m/get-channel-message! (:messaging @state) channel-id message-id))

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

(defn send-interaction-response [interaction-id interaction-token data]
  (m/create-interaction-response!
   (:messaging @state)
   interaction-id
   interaction-token
   4
   :data
   {:tts false
    :embeds [(merge (embed-template)
                    data)]}))

(defn send-secret-interaction-response [interaction-id interaction-token data]
  (m/create-interaction-response!
   (:messaging @state)
   interaction-id
   interaction-token
   4
   :data
   {:tts false
    :flags 64
    :embeds [(merge (embed-template)
                    data)]}))


;; Update the oof-data state from a message event


(defn do-oof-message [user-id message-id channel-id guild-id]
  (get-username user-id guild-id) ; For the side-effects
  (swap! oof-data update-in [guild-id user-id :given-oof] (fnil inc 0))
  (let [{{user-id :id} :author}
        (find-oof-parent message-id channel-id)]
    (get-username user-id guild-id) ; For the side-effects
    (swap! oof-data update-in [guild-id user-id :gotten-oofed] (fnil inc 0))))

;; Sort the oof data by key
(defn sort-oof-data [key guild-id]
  (take (get-top-count guild-id)
        (sort-by (comp key second)
                 (fnil > 0 0)
                 (into [] (get @oof-data guild-id)))))

(defn oof-table-data [key guild-id]
  (let [top (sort-oof-data key guild-id)]
    (s/join "\n"
            (for [[id data] top]
              (when-let [count (key data)]
                (str count
                     " - "
                     (get-username id guild-id)))))))

(defn get-oof-rank [key guild-id user-id]
  (->> (keep-indexed
        (fn [idx item]
          (when (= (first item)
                   user-id)
            idx))
        (sort-oof-data key guild-id))
       first
       ((fnil inc (count (keys (@oof-data guild-id)))))))

;; Get the summary of oof-data
(defn get-oofs [guild-id]
  {:title "Oof Leaderboard"
   :fields [{:name "Top Oofed Users"
             :value (oof-table-data :gotten-oofed guild-id)
             :inline true}
            {:name "Top Oofers"
             :value (oof-table-data :given-oof guild-id)
             :inline true}]})

;; Get the summary of the user oof data
(defn my-oofs [guild-id user-id]
  {:author {:name (str (get-username user-id guild-id) "'s oofs")
            :icon_url (user-avatar-url user-id (get-user-avatar user-id guild-id))}
   :fields [{:name "Given Oofs"
             :value (get-in @oof-data [guild-id user-id :given-oof] 0)
             :inline true}
            {:name "Rank"
             :value (str (get-oof-rank :given-oof guild-id user-id))
             :inline true}
            {:name "\u200b"
             :value "\u200b"
             :inline true}
            {:name "Gotten Oofed"
             :value (get-in @oof-data [guild-id user-id :gotten-oofed] 0)
             :inline true}
            {:name "Rank"
             :value (str (get-oof-rank :gotten-oofed guild-id user-id))
             :inline true}
            {:name "\u200b"
             :value "\u200b"
             :inline true}]})

(defn set-status []
  (c/status-update! (:connection @state)
                    :activity (c/create-activity :name "Emacs")))

(defn get-owner-id [guild-id]
  (if-let [owner-id (get-in @oof-data [guild-id :owner-id])]
    owner-id
    (let [new-owner-id (:owner-id @(m/get-guild! (:messaging @state) guild-id))]
      (swap! oof-data assoc-in [guild-id :owner-id] new-owner-id)
      new-owner-id)))

(defn register-commands []
  @(m/bulk-overwrite-guild-application-commands!
    (:messaging @state)
    @bot-id
    "533112383321669633"
    [{:name "oof"
      :description "Oofbot commands"
      :options [{:name "top"
                 :type 1
                 :description "Gets the oof leaderboard"}
                {:name "stats"
                 :type 1
                 :description "Gets your oof status"}
                {:name "topcount"
                 :type 1
                 :description "Sets the leaderboard size"
                 :options [{:name "count"
                            :type 4
                            :required true
                            :description "Size of the leaderboard"}]}]}]))

(defmulti handle-event
  (fn [event-type _]
    event-type))

(defmethod handle-event :default
  [_ _])

(defmethod handle-event :ready
  [_ {{id :id} :user}]
  (reset! bot-id id)
  (set-status)
  (register-commands))

(defmethod handle-event :guild-create
  [_ {:keys [owner-id id]}]
  (swap! oof-data assoc-in [id :owner-id] owner-id))

(defmethod handle-event :interaction-create
  [_ {{[{command-name :name
         [{attribute-name :name
           attribute-value :value}] :options}] :options} :data
      guild-id :guild-id
      {{user-id :id} :user} :member
      interaction-id :id
      interaction-token :token}]
  (letfn [(send-response [payload]
            (send-interaction-response interaction-id interaction-token payload))
          (send-secret-response [payload]
                                (send-secret-interaction-response interaction-id interaction-token payload))]
    (case command-name
      "top" (send-response (get-oofs guild-id))
      "stats" (send-secret-response (my-oofs guild-id user-id))
      "topcount" (send-secret-response (if (not= "count" attribute-name)
                                         {:description "Somehow got not a count? - File an issue"}
                                         (if (not= user-id (get-owner-id guild-id))
                                           {:description "You're not the server owner - you can't do this"}
                                           (if (< 0 attribute-value top-count-max)
                                             (do
                                               (swap! oof-data assoc-in [guild-id :top-count] attribute-value)
                                               {:description (str "Leaderboard size changed to: " attribute-value)})
                                             {:description "Leaderboard size out of range"}))))
      (println "Got malformed interaction"))))

(defmethod handle-event :message-create
  [_ {{bot :bot user-id :id} :author
      channel-id :channel-id
      content :content
      message-id :id
      guild-id :guild-id}]
  (a/go
    (when-not bot ; Bots can't oof
      (when (is-oof? content)
        (a/thread (do-oof-message user-id message-id channel-id guild-id))))))

; Oof reaction adds
(defmethod handle-event :message-reaction-add
  [_ {:keys [user-id channel-id message-id emoji guild-id]}]
  (a/go (when (= "OOF" (:name emoji))
          (let [{{oofee-id :id} :author} (a/<! (get-message message-id channel-id))]
            (swap! oof-data update-in [guild-id user-id :given-oof] (fnil inc 0))
            (swap! oof-data update-in [guild-id oofee-id :gotten-oofed] (fnil inc 0))))))

; Oof reaction removals
(defmethod handle-event :message-reaction-remove
  [_ {:keys [user-id channel-id message-id emoji guild-id]}]
  (a/go (when (= "OOF" (:name emoji))
          (let [{{oofee-id :id} :author} (a/<! (get-message message-id channel-id))]
            (swap! oof-data update-in [guild-id user-id :given-oof] (fnil dec 0))
            (swap! oof-data update-in [guild-id oofee-id :gotten-oofed] (fnil dec 0))))))

; Data saving (happens every save-timeout)
(defn save-data []
  (a/go-loop []
    (a/<! (a/timeout save-timeout))
    (print "Saving data\n")
    (write-edn @oof-data data-filename)
    (recur)))

(defn disconnect-bot [state]
  (m/stop-connection! (:messaging state))
  (c/disconnect-bot! (:connection state)))

(defn run []
  (let [event-ch (a/chan 100)
        connection-ch (c/connect-bot! token event-ch :intents #{:guilds
                                                                :guild-messages
                                                                :guild-message-reactions})
        messaging-ch (m/start-connection! token)
        init-state {:connection connection-ch
                    :event event-ch
                    :messaging messaging-ch}]
    (reset! state init-state)
    (save-data)
    (e/message-pump! event-ch #'handle-event)))

;; Program Entry Point
(defn -main
  [& _]
  (run)
  (shutdown-agents))
