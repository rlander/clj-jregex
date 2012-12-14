(ns clj-jregex.core
   (:import [com.google.code.regexp.NamedMatcher]))

(defn re-pattern [s]
  (if (instance? jregex.Pattern s)
    s
  (jregex.Pattern. s)))

(defn re-matcher
  [^jregex.Pattern re s]
    (. re (matcher s)))

(defn re-groups
  [^jregex.Matcher m]
    (let [gc  (. m (groupCount))]
      (if (zero? gc)
        (. m (group))
        (loop [ret [] c 0]
          (if (< c gc)
            (recur (conj ret (. m (group c))) (inc c))
            ret)))))

(defn re-group 
  [^jregex.Pattern re s group-name]
  (let [m (re-matcher re s)]
    (do 
      (re-find m)
      (. m (group group-name)))))

(defn re-find
  ([^jregex.Matcher m]
   (when (. m (find))
     (re-groups m)))
  ([^jregex.Pattern re s]
   (let [m (re-matcher re s)]
     (re-find m))))