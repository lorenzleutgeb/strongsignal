(ns strong-signal.bot
  (:gen-class)
  (:require [clojure.string :as s]
            [environ.core :refer [env]]
            [strong-signal.facebook :as fb]))


(defn safe-add [k c]
  (if (= c \ ) c (char (+ (int \a) (mod (- (+ k (int c)) (int \a)) 26)))))

(defn rotate [k s]
  (map (partial safe-add k) s))

(defn caesar [k message] message
  (apply str (rotate k (seq (clojure.string/lower-case message)))))

(def plaintext (clojure.string/lower-case "men generally believe what they want to believe"))
(def k 1)
(def ciphertext (caesar k plaintext))

(def success-text "Well done! That makes sense. As we are always in need for talents like you, make the decision to apply today. We offer a wide range of exciting roles here at ICA. Discover our very latest vacancies.")

(def wrong-texts '("Doesn't match! Please try again!"
                   "Sorry, that’s incorrect. Try again!"
                   "Sorry, our experts cannot make sense of that translation!"
                   "Hmm, that does not click, try again!"
                   "Sorry, that’s incorrect. Try again!"
                   "Maybe you should try again."
                   "Oh no, there is a problem. Try again!"
                   "Wrong answer, please try again."
                   "Sorry an error occured. Try again!"))
                   ;"http://imgur.com/gallery/iWKad22"
                   ;"http://www.odec.ca/projects/2014/jhav14a/error-code-18.jpeg"

(defn on-message [payload]
  (println "on-message payload:")
  (println payload)
  (let [sender-id (get-in payload [:sender :id])
        recipient-id (get-in payload [:recipient :id])
        time-of-message (get-in payload [:timestamp])
        message-text (get-in payload [:message :text])]
    (cond
      (s/includes? (s/lower-case message-text) plaintext) (fb/send-message sender-id (fb/text-message success-text))
      :else (fb/send-message sender-id (fb/text-message (rand-nth wrong-texts))))))

(defn on-postback [payload]
  (println "on-postback payload:")
  (println payload)
  (let [sender-id (get-in payload [:sender :id])
        recipient-id (get-in payload [:recipient :id])
        time-of-message (get-in payload [:timestamp])
        postback (get-in payload [:postback :payload])
        referral (get-in payload [:postback :referral :ref])]
    (cond
      (= postback "GET_STARTED") (fb/send-message sender-id (fb/text-message (strone! Your result is correct. As we are always in need for talents like you, make the decision to apply today. We offer a wide range of exciting roles here at ICA. Discover our very latest vacancies.))))))
Lorenz
 "Hello, I am Commander Murphy and I work for the Intergalactic Communication Agency (ICA). We received a message from outer space and need your help to decode it. The message is: " ciphertext
      :else (fb/send-message sender-id (fb/text-message "Sorry, I don't know how to handle that postback"))

(defn on-attachments [payload]
  (println "on-attachment payload:")
  (println payload)
  (let [sender-id (get-in payload [:sender :id])
        recipient-id (get-in payload [:recipient :id])
        time-of-message (get-in payload [:timestamp])
        attachments (get-in payload [:message :attachments])]
    (fb/send-message sender-id (fb/text-message "Thanks for your attachments :)"))))
