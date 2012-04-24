(ns halleck.core
  (:require [clojure.java.io :as io]))

(defn f [c excludes form]
  (apply
   merge
   (let [obj-methods (set (.getMethods Object))]
     (for [m (.getMethods c)
           :when (and (not (obj-methods m))
                      (not (excludes (symbol (.getName m)))))
           :let [argc (count (.getParameterTypes m))
                 args (repeatedly argc gensym)]]
       {[(symbol (.getName m))
         (vec (.getParameterTypes m))
         (.getReturnType m)]
        (list
         'defn
         (symbol (str "-" (.getName m)))
         (vec (list* 'this  args))

         `(. ~form
             ~(symbol (.getName m))
             ~@args))}))))


(defn f [c excludes form]
  (apply
   merge
   (let [obj-methods (set (.getMethods Object))]
     (for [m (.getMethods c)
           :when (and (not (obj-methods m))
                      (not (excludes (symbol (.getName m)))))
           :let [argc (count (.getParameterTypes m))
                 args (repeatedly argc gensym)]]
       {[(symbol (.getName m))
         (vec (.getParameterTypes m))
         (.getReturnType m)]
        (list
         'defn
         (symbol (str "-" (.getName m)))
         (list
          (vec (list* 'this  args))
          `(. ~form
              ~(symbol (.getName m))
              ~@args)))}))))


(defmacro delegate [delegates
                    excluded-methods
                    gen-class-form]
  (let [gc-class (apply hash-map (rest gen-class-form))
        interface-methods (set (for [c (:implements gc-class)
                                     m (.getMethods (resolve c))]
                                 (symbol (.getName m))))
        [methods funs] ((juxt keys vals)
                        (apply merge
                               (for [[class form] delegates]
                                 (f (resolve class) excluded-methods form))))]
    `(do
       ~(cons (first gen-class-form)
              (for [x (update-in gc-class
                                 [:methods]
                                 into
                                 (remove (comp interface-methods first)
                                         methods))
                    i x]
                i))
       ~@(for [[n impl] (group-by second funs)]
           `(defn ~n ~@(map #(nth % 2) impl))))))

(delegate {org.apache.log4j.FileAppender (.state this)}
          #{doAppend}
          (gen-class :name halleck.Appender
                     :implements [org.apache.log4j.Appender
                                  org.apache.log4j.spi.OptionHandler]
                     :constructors {[] []
                                    [org.apache.log4j.Layout String] []
                                    [org.apache.log4j.Layout String
                                     boolean] []
                                     [org.apache.log4j.Layout String boolean
                                      boolean int] []}
                     :init init
                     :state state))

(defn -activateOptions [this]
  (.activateOptions (.state this)))

(defn -init
  ([] [[] (org.apache.log4j.FileAppender.)])
  ([layout filename]
     [[] (org.apache.log4j.FileAppender.
          layout
          filename)])
  ([layout filename append]
     [[] (org.apache.log4j.FileAppender.
          layout
          filename
          append)])
  ([layout filename append buffered-io buf-size]
     [[] (org.apache.log4j.FileAppender.
          layout
          filename
          append
          buffered-io
          buf-size)]))

(defn clone-event [event & {:keys [message level]}]
  (org.apache.log4j.spi.LoggingEvent.
   (.getFQNOfLoggerClass event)
   (.getLogger event)
   (.getTimeStamp event)
   (or level (.getLevel event))
   (or message (.getMessage event))
   (.getThreadName event)
   (.getThrowableInformation event)
   (.getNDC event)
   (.getLocationInformation event)
   (.getProperties event)))


(defn index [f]
  (with-open [fins (io/input-stream f)]
    (let [index (java.util.ArrayList. 1000)]
      (loop [n (.read fins)
             n-1 nil
             n-2 nil
             start 0
             place 0]
        (cond
          (= -1 n) index
          (and (= (char n) \newline)
               (= (char n-1) \%)
               (= (char n) \newline))
          (do
            (.add index [f start (- (- place 2) start)])
            (recur (.read fins)
                   n
                   n-1
                   (inc place)
                   (inc place)))
          :else
          (recur (.read fins)
                 n
                 n-1
                 start
                 (inc place)))))))

(def places (.toArray (index (io/resource "fortunes"))))

(defn random-place []
  (aget places (rand-int (count places))))

;; (defn random-fortune []
;;   (let [[file start length] (random-place)]
;;     (with-open [fins (java.io.FileInputStream. file)
;;                 fc (.getChannel fins)]
;;       (.trim (.toString
;;               (.decode
;;                (java.nio.charset.Charset/forName "utf8")
;;                (.map fc
;;                      java.nio.channels.FileChannel$MapMode/READ_ONLY
;;                      start
;;                      length)))))))

(defn random-fortune []
  (let [[file start length] (random-place)]
    (with-open [fins (io/input-stream file)]
      (.skip fins start)
      (let [b (byte-array length)]
        (.read fins b 0 length)
        (.trim (String. b "utf8"))))))

(defn -doAppend [this event]
  (.doAppend (.state this) event)
  (when (= org.apache.log4j.Level/WARN
           (.getLevel event))
    (.doAppend (.state this)
               (clone-event
                event
                :message
                (str "\n" (random-fortune))))))
