(ns oofbot.core
  (:require [discljord.connections :as c]
            [discljord.messaging :as m]
            [discljord.events :as e]
            [clojure.core.async :as a]
            [clojure.string :refer [includes? lower-case trim]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import
   [java.time ZoneId ZonedDateTime Instant]
   [java.time.format DateTimeFormatter])
  (:gen-class))

;; Discord Bot API Token
(def token (trim (slurp "token.txt")))

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
  (if (.exists (io/as-file "oof-data.edn"))
    (atom (with-open [r (java.io.PushbackReader. (io/reader "oof-data.edn"))]
            (edn/read r)))
    (atom nil)))

;; How many users in the "top" list
(def default-top-count 5)
(def top-count-max 25)

(def default-command-prefix "!")

;; Timeout to query for new usename
(def username-timeout 3600000)

;; Data saving timeout
(def save-timeout 300000)

;; Image base url
(def discord-base "https://cdn.discordapp.com/")

;; User avatar url
(defn user-avatar-url [user-id user-avatar]
  (str discord-base "avatars/" user-id "/" user-avatar
       (if (= "a_" (subs user-avatar 0 2))
         ".gif"
         ".png")))

(defn current-time []
  (.format (ZonedDateTime/now (ZoneId/of "UTC")) DateTimeFormatter/ISO_OFFSET_DATE_TIME))

;; Embed Template
(defn embed-template []
  {:color 0xAD2D3C
   :timestamp (current-time)
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

;; Send a str as a message on the channel
(defn send-message [message channel-id]
  (m/create-message!
   (:messaging @state)
   channel-id :embed
   (merge (embed-template)
          message)))

;; Update the oof-data state from a message event
(defn do-oof-message [user-id message-id channel-id guild-id]
  (get-username user-id guild-id) ; For the side-effects
  (swap! oof-data update-in [guild-id user-id :given-oof] (fnil inc 0))
  (let [{{bot :bot user-id :id} :author
         content :content
         message-id :id}
        (find-oof-parent message-id channel-id)]
    (get-username user-id guild-id) ; For the side-effects
    (swap! oof-data update-in [guild-id user-id :gotten-oofed] (fnil inc 0))))

;; Sort the oof data by key
(defn sort-oof-data [key guild-id]
  (take (get-top-count guild-id) (sort-by (comp key second) (fnil > 0 0) (into [] (get @oof-data guild-id)))))

(defn oof-table-data [key guild-id]
  (let [top (sort-oof-data key guild-id)]
    (str/join "\n"
              (for [[id data] top]
                (if-let [count (key data)]
                  (str count
                       " - "
                       (get-username id guild-id)))))))

(defn get-oof-rank [key guild-id user-id]
  (->> (keep-indexed
        (fn [idx item]
          (if (= (first item)
                 user-id)
            idx))
        (sort-oof-data key guild-id))
       first
       ((fnil inc (count (keys (@oof-data guild-id)))))))

;; Get the string summary of oof-data
(defn get-oofs [guild-id]
  {:title "Oof Leaderboard"
   :fields [{:name "Top Oofed Users"
             :value (oof-table-data :gotten-oofed guild-id)
             :inline true}
            {:name "Top Oofers"
             :value (oof-table-data :given-oof guild-id)
             :inline true}]})

;; Get the string summary of the user oof data
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

(defn oofhelp [prefix]
  {:desription "Oofbot Help"
   :fields [{:name (str prefix "getoofs")
             :value "Oof leaderboard"}
            {:name (str prefix "myoofs")
             :value "Your oof count"}
            {:name (str prefix "oofhelp")
             :value "This help"}
            {:name (str prefix "topcount <count>")
             :value "Server owner: Changes the displayed leaderboard size"}
            {:name "@oofbot prefix <new prefix>"
             :value "Server owner: change the command prefix"}]})

(defn set-status []
  (c/status-update! (:connection @state)
                    :activity (c/create-activity :name "@oofbot")))

(defn get-command-prefix [guild-id]
  (get-in @oof-data [guild-id :command-prefix] default-command-prefix))

(defn get-owner-id [guild-id]
  (if-let [owner-id (get-in @oof-data [guild-id :owner-id])]
    owner-id
    (let [new-owner-id (:owner-id @(m/get-guild! (:messaging @state) guild-id))]
      (swap! oof-data assoc-in [guild-id :owner-id] new-owner-id)
      new-owner-id)))

(defmulti handle-event
  (fn [event-type event-data]
    event-type))

(defmethod handle-event :default
  [event-type event-data])

(defmethod handle-event :ready
  [event-type {{id :id} :user}]
  (reset! bot-id id)
  (set-status))

(defmethod handle-event :guild-create
  [event-type {:keys [owner-id id]}]
  (swap! oof-data assoc-in [id :owner-id] owner-id))

(defmethod handle-event :message-create
  [event-type {{bot :bot user-id :id username :username} :author
               channel-id :channel-id
               content :content
               message-id :id
               mentions :mentions
               guild-id :guild-id}]
  (a/go
    (when-not bot
      (let [prefix (get-command-prefix guild-id)]
        (cond
          (str/starts-with? content prefix) (cond
                                              (= (subs content (count prefix)) "getoofs") (a/thread (send-message (get-oofs guild-id) channel-id))
                                              (= (subs content (count prefix)) "myoofs") (a/thread (send-message (my-oofs guild-id user-id) channel-id))
                                              (= (subs content (count prefix)) "oofhelp") (send-message (oofhelp prefix) channel-id)
                                              (str/starts-with? (subs content (count prefix)) "topcount ") (a/thread (when-let [new-top-count (second (str/split content #" " 2))]
                                                                                                                       (when-let [new-top-count (and (re-matches #"\d+" new-top-count)
                                                                                                                                                     (Long. new-top-count))]
                                                                                                                         (when (and (= user-id (get-owner-id guild-id))
                                                                                                                                    (<= new-top-count top-count-max))
                                                                                                                           (swap! oof-data assoc-in [guild-id :top-count] new-top-count)
                                                                                                                           (send-message {:description (str "Leaderboard size changed to: " new-top-count)} channel-id)))))
                                              :else (if (is-oof? content)
                                                      (a/thread (do-oof-message user-id message-id channel-id guild-id))))
          (and (contains? (set (map :id mentions)) @bot-id)
               (re-matches #"<@!?\d+>" content)) (send-message (oofhelp (get-command-prefix guild-id)) channel-id)
          (contains? (set (map :id mentions)) @bot-id) (when-let [new-prefix (second (re-matches #"<@!?\d+>\s+prefix\s+(\S+)" content))]
                                                         (when (= user-id (get-owner-id guild-id))
                                                           (swap! oof-data assoc-in [guild-id :command-prefix] new-prefix)
                                                           (send-message {:description (str "Command prefix changed to: " new-prefix)}
                                                                         channel-id)))
          (is-oof? content) (a/thread (do-oof-message user-id message-id channel-id guild-id)))))))

(defmethod handle-event :message-reaction-add
  [event-type {:keys [user-id channel-id message-id emoji guild-id]}]
  (a/go (if (= "OOF" (:name emoji))
          (let [{{oofee-id :id} :author} (a/<! (get-message message-id channel-id))]
            (swap! oof-data update-in [guild-id user-id :given-oof] (fnil inc 0))
            (swap! oof-data update-in [guild-id oofee-id :gotten-oofed] (fnil inc 0))))))

(defmethod handle-event :message-reaction-remove
  [event-type {:keys [user-id channel-id message-id emoji guild-id]}]
  (a/go (if (= "OOF" (:name emoji))
          (let [{{oofee-id :id} :author} (a/<! (get-message message-id channel-id))]
            (swap! oof-data update-in [guild-id user-id :given-oof] (fnil dec 0))
            (swap! oof-data update-in [guild-id oofee-id :gotten-oofed] (fnil dec 0))))))
(defn save-data []
  (a/go-loop []
    (a/<! (a/timeout save-timeout))
    (print "Saving data\n")
    (write-edn @oof-data "oof-data.edn")
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

(defn -main
  [& args]
  (run)
  (shutdown-agents))
