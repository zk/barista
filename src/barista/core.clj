(ns barista.core)


(def *full-inv* {:coffee 10
                 :decaf-coffee 10
                 :sugar 10
                 :cream 10})

(def *inv* (ref *full-inv*))

(def *prices* {:coffee 0.75M
               :decaf-coffee 0.75M
               :sugar 0.25M
               :cream 0.25M
               :steamed-milk 0.35M
               :foamed-milk 0.35M
               :espresso 1.10M
               :cocoa 0.90M
               :whipped-cream 1.00M})

(def *recipes* [(recipe "Coffee"
                        :coffee 3
                        :sugar 1
                        :cream 1)
                (recipe "Decaf Coffee" 
                        :decaf-coffee 3
                        :sugar 1
                        :cream 1)])

(defn restock [] (dosync (ref-set *inv* *full-inv*)))

(defn has-ingredient-amount? [ing]
  (let [type (first ing)
        amount-required (second ing)]
    (>= (get @*inv* type 0) amount-required)))

(defn has-ingredients? [rcp]
  (every? true? (map has-ingredient-amount? (:ingredients rcp))))

(defn make-drink [rcp]
  (println "Making " (:name rcp) "...")
  (let [res (dosync
             (when (has-ingredients? rcp)
               (do
                 (doall (map remove-ingredient (:ingredients rcp)))
                 @*inv*)))]
    res))

(defn remove-ingredient [ing]
  (let [type (first ing)
        inv-amt (type @*inv*)
        dec-by (second ing)]
    (alter *inv* assoc type (- inv-amt dec-by))))

(defn order [name]
  (if-let [rcp (->> *recipes*
                    (filter #(= name (:name %)))
                    first)]
    (if (make-drink rcp)
      (str "Your " (:name rcp) " is ready!")
      (str "Sorry, we were short some ingredients for " (:name rcp)))
    "not found..."))

(defn price [rcp]
  (reduce 
   + 
   (map #(let [type (first %)
               amt (second %)]
           (* (type *prices*) amt)) 
        (:ingredients rcp))))

(defn recipe [& args]
  {:name (first args)
   :ingredients (partition 2 (drop 1 args))})

(defn recipe-names []
  (map :name *recipes*))
