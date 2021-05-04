(import [java.io BufferedReader InputStreamReader]
        [java.net URL])

(defn joc-www []
  (-> "https://duckduckgo.com"  URL.
      .openStream InputStreamReader. BufferedReader.))

;; (let [stream (joc-www)]
;;   (with-open [page stream]
;;     (println (.readLine page))
;;     (print "The stream will now close... "))
;;   (println "but let's read from it anyway.")
;;   (.readLine stream))
