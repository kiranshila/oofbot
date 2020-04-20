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

(defmethod handle-event :connected-all-shards
  [event-type event-data]
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
    (print "Saving data\n")
    (set-status) ;; This randomly gets changed?
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
