(ns mastermind.json
  (:require [cheshire.core :as json]
            [clojure.data.codec.base64 :as base64]))

(defn encode [secret]
  (let [sas (json/generate-string secret)
        _   (println sas)
        b64 (base64/encode (.getBytes sas))]
    (String. b64)))

(defn decode [secret]
  (let [b64        (.getBytes secret)
        sas        (String. (base64/decode b64))
        parsed-seq (json/parse-string sas)
        w-keywords (map keyword parsed-seq)
        parsed-vec (apply vector w-keywords)]
    parsed-vec))
