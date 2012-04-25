(ns halleck.delegate)

(defn code-gen [c excludes form]
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
                                 (code-gen
                                  (resolve class) excluded-methods form))))]
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
