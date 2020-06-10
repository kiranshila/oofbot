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
(def top-count 5)

;; Check to see if a given str is a valid oof
(defn is-oof? [input]
  (some? (re-seq #"(?)\bo+of\b" input)))

;; Get a vector of 5 messages before 
(defn get-messages-before [before-id channel-id]
  @(m/get-channel-messages! (:messaging @state) channel-id
                            :before before-id
                            :limit top-count))

;; Get a username from the data, update data if we don't have it
(defn get-username [user-id guild-id]
  (if-let [username (get-in @oof-data [guild-id user-id :username])]
    username
    (let [new-username (:username @(m/get-user! (:messaging @state) user-id))]
      (swap! oof-data assoc-in [guild-id user-id :username] new-username)
      new-username)))

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
(defn oof-row-to-table [oof-data-row key guild-id]
  {"Username" (get-username (first oof-data-row) guild-id)
   "Count" (get (second oof-data-row) key)})

;; Convert oof-data into an ascii table sorted by key
(defn oof-data-to-table [key guild-id]
  (str "```"
       (let [top (sort-oof-data key guild-id)]
         (with-out-str (print-table (map #(oof-row-to-table % key guild-id)
                                         top))))
       "```"))

;; Get the string summary of oof-data
(defn get-oofs [guild-id]
  (str
   "Top Oofed Users"
   (oof-data-to-table :gotten-oofed guild-id)
   "Top Oofers"
   (oof-data-to-table :given-oof guild-id)))

;; Get the string summary of the user oof data
(defn my-oofs [guild-id user-id]
  (str "```\n"
       "Given Oofs: "
       (get-in @oof-data [guild-id user-id :given-oof])
       "\n"
       "Gotten Oofed: "
       (get-in @oof-data [guild-id user-id :gotten-oofed])
       "```"))

(def oofhelp "```!getoofs - Display oof leaderboard\n!myoofs - Display user's oofs\n!oofhelp - This help```")

(defmulti handle-event
  (fn [event-type event-data]
    event-type))

(defmethod handle-event :default
  [event-type event-data])

(defmethod handle-event :ready
  [event-type {{id :id} :user}]
  (reset! bot-id id))

(defn set-status []
  (c/status-update! (:connection @state)
                    :activity (c/create-activity :name "!oofhelp")))

(defmethod handle-event :message-create
  [event-type {{bot :bot user-id :id username :username} :author
               channel-id :channel-id
               content :content
               message-id :id
               mentions :mentions
               guild-id :guild-id}]
  (when (not bot)
    (cond
      (is-oof? content) (do-oof-message user-id message-id channel-id guild-id)
      (and (contains? (set (map :id mentions)) @bot-id)
           (re-matches #"<@!?\d+>" content)) (send-message oofhelp channel-id)
     ;; Commands
      :else (when (and (= (first content) \!)
                       (not bot))
              (when (= (subs content 1) "getoofs")
                (send-message (get-oofs guild-id) channel-id))
              (when (= (subs content 1) "myoofs")
                (send-message (my-oofs guild-id user-id) channel-id))
              (when (= (subs content 1) "oofhelp")
                (send-message oofhelp channel-id))))))

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
    (print "Saving data\n")
    (write-edn @oof-data "oof-data.edn")
    (recur)))

(defn disconnect-bot [state]
  (m/stop-connection! (:messaging state))
  (c/disconnect-bot! (:connection state)))

(defn run []
  (let [event-ch (a/chan 100)
        connection-ch (c/connect-bot! token event-ch :intents #{:guild-messages
                                                                :guild-message-reactions})
        messaging-ch (m/start-connection! token)
        init-state {:connection connection-ch
                    :event event-ch
                    :messaging messaging-ch}]
    (reset! state init-state)
    (save-data)
    (set-status)
    (e/message-pump! event-ch #'handle-event)))

(defn -main
  [& args]
  (run)
  (shutdown-agents))
