(ns barista.core)


(def *full-inv* {:coffee 10
                 :decaf-coffee 10
                 :sugar 10
                 :cream 10
                 :steamed-milk 10
                 :foamed-milk 10
                 :espresso 10
                 :cocoa 10
                 :whipped-cream 10})

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

(defn recipe [& args]
  {:name (first args)
   :ingredients (partition 2 (drop 1 args))})


(def *recipes* [(recipe "Coffee"
                        :coffee 3
                        :sugar 1
                        :cream 1)
                (recipe "Decaf Coffee" 
                        :decaf-coffee 3
                        :sugar 1
                        :cream 1)
                (recipe "Caffe Latte"
                        :espresso 2
                        :steamed-milk 1)
                (recipe "Caffe Americano"
                        :espresso 3)
                (recipe "Caffe Mocha"
                        :espresso 1
                        :cocoa 1
                        :steamed-milk 1
                        :whipped-cream 1)
                (recipe "Cappuccino"
                        :espresso 2
                        :steamed-milk 1
                        :foamed-milk 1)])
(defn price-for [rcp]
  (reduce 
   + 
   (map #(let [type (first %)
               amt (second %)]
           (* (type *prices*) amt)) 
        (:ingredients rcp))))

(defn recipe-names []
  (map :name *recipes*))

(defn restock [] (dosync (ref-set *inv* *full-inv*)))

(defn has-ingredient-amount? [ing]
  (let [type (first ing)
        amount-required (second ing)]
    (>= (get @*inv* type 0) amount-required)))

(defn has-ingredients? [rcp]
  (every? true? (map has-ingredient-amount? (:ingredients rcp))))

(defn remove-ingredient [ing]
  (let [type (first ing)
        inv-amt (type @*inv*)
        dec-by (second ing)]
    (alter *inv* assoc type (- inv-amt dec-by))))

(defn make-drink! [rcp]
  (dosync
   (when (has-ingredients? rcp)
     (do
       (doall (map remove-ingredient (:ingredients rcp)))
       @*inv*))))

(defn order-drink [name]
  (if-let [rcp (->> *recipes*
                    (filter #(= name (:name %)))
                    first)]
    (if (make-drink! rcp)
      (str "Dispensing: " (:name rcp))
      (str "Out of Stock: " (:name rcp)))
    "not found..."))

(defn prn-inv []
  (println "Inventory:")
  (doseq [[ing amt] @*inv*]
    (println (str (name ing) "," amt))))

(defn prn-menu []
  (println "Menu:")
  (doseq [[rcp idx] 
          (map list 
               *recipes* 
               (map inc (range (count *recipes*))))]
    (println
     (str idx "," (:name rcp) "," (price-for rcp) "," (has-ingredients? rcp)
      ))))

(defn app-startup []
  (prn-inv)
  (prn-menu))



