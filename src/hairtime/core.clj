(ns hairtime.core
  (:use [pocket.core :as skt]
        [overtone.live])
  (:require
    [hairtime.dubstep :as dubstep]
    [clojure.data.json :as json]))

(def app-auth {"appName" "hairtime"
               "appKey" "9f54141b4b4c567c558d3a76cb8d715cbde03096"})

(def app-config {"enableRawOutput" false
             "format" "Json"})

(defonce *headset (atom nil))
(defonce *play (atom false))
(defonce *dubstep (atom nil))

;; 0 -> 1
;; 100 -> 64
(defn pct->wobble [pct]
  (Math/floor (* pct (/ 8 100))))

(def pentatonic-scale [0 2 4 7 9])
(def blues-scale [0 3 5 6 7 10])
(def major-scale [0 2 4 5 7 9 11])
(def chromatic [0])

(defn which-tone [register scale]
  (scale (mod register (count scale))))

(defn which-octave [register scale]
  (Math/floor (/ register (count scale))))

(defn scale-pct [pct low high]
  (+ low (* (/ pct 100) (- high low))))

(defn register->tone [register scale]
  (+ (* 12 (which-octave register scale))
     (which-tone register scale)))

(defn pct->tone [pct scale]
  (let [low 10
        high 40
        register (int (scale-pct pct low high))]
    (register->tone register scale)))

(defn init-dubstep! []
  (reset! *dubstep (dubstep/dubstep))
  ;;(ctl @*dubstep :snare-vol 0)
  ;;(ctl @*dubstep :kick-vol 0)
)

(defn change-dubstep [{{attention "attention"
                        meditation "meditation"} "eSense"}]
  (let [note (pct->tone meditation chromatic)
        wobble (pct->wobble attention)]
    (println "note: " note "\t wobble: " wobble)
    (ctl @*dubstep :note note)
    (ctl @*dubstep :wobble wobble)))

(defn send-json [msg]
  (skt/write-once @*headset (json/json-str msg)))

(defn read-headset-val []
  (try
    (let [in (skt/read-once @*headset)]
      (println "in: " in)
      (json/read-str in))
    (catch Exception e
      (println "Exception!" e))))

(defn flush-binary-data []
  (loop []
    (Thread/sleep 100)
    (when-not read-headset-val (recur))))

(defn read-loop []
  (loop []
    (when @*play
      (when-let [result (read-headset-val)]
        (if (contains? result "eSense")
          (change-dubstep result))
        (recur)))))

(defn start-read-loop! []
  (.start (Thread. (read-loop))))

(defn start! []
  (reset! *headset (skt/socket "127.0.0.1" 13854))
  (send-json app-config)
  (flush-binary-data)
  (reset! *play true)
  (init-dubstep!)
  (start-read-loop!))

(defn stop! []
  (stop)
  (reset! *play false))
