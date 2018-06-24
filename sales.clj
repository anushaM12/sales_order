(use 'clojure.java.io)
(def custdb (atom {}))
(def proddb (atom {}))
(def salesdb (atom {}))
(def totsalesdb (atom {}))
(defn  display_cust[]
     (with-open [rdr (reader "cust.txt")]
     (doseq [line (line-seq rdr)]
      (def splistring (clojure.string/split line #"\|"))
      (def custkeyq (get splistring 0))
      (def value (subvec splistring 1))
      (swap! custdb assoc custkeyq value))))
      
(defn sort_cust[]
      (def sort_custdb (into (sorted-map) @custdb))
      (doseq [[k v] sort_custdb] (prn k  v)))

(defn  display_prod[]
     (with-open [rdr (reader "prod.txt")]
     (doseq [line (line-seq rdr)]
      (def splistring (clojure.string/split line #"\|"))
      (def keyq (get splistring 0))
      (def value (subvec splistring 1))
      (swap! proddb assoc keyq value))))
      
(defn sort_prod[]
      (def sort_proddb (into (sorted-map) @proddb))
      (doseq [[k v] sort_proddb] (prn k  v)))

(defn  display_sales[]
     (with-open [rdr (reader "sales.txt")]
     (doseq [line (line-seq rdr)]
      (def splistring (clojure.string/split line #"\|"))
      (def keyq (get splistring 0))
      (def cust_id (get splistring 1))
      (def prod_id (get splistring 2))
      (def quantity (get splistring 3))
      (def listval (get @custdb cust_id))
      (def prodlist (get @proddb prod_id))
      (def cust_name (subvec listval 0 1))
      (def prod_name (subvec prodlist 0 1))
      (def prod_price (subvec prodlist 1 2))
      (swap! totsalesdb assoc prod_name prod_price)
      (def values (vector cust_name prod_name quantity))
      (swap! salesdb assoc keyq values))))
      
(defn sort_sales[]
      (def sort_saledb (into (sorted-map) @salesdb))
      (doseq [[k v] sort_saledb] (prn k  v)))

(defn total_sales[name]
 (def  counter 0)
     (def part (vals @salesdb))
     (def salesvect  (into [] part))
     (loop [i  (count salesvect)]
     (when (>= i 0)
     
     (def firstrec (get salesvect i))
     (def nameofcust (get firstrec 0))
     (def prodnm (get salesvect i))
     (def prodname (get prodnm 1))
     (def quant (get salesvect i))
     (def quanti (get quant 2))
     (def finalname (some #(= name %) nameofcust))
     (if
      (= true finalname) 
      (do 
       (def price (get @totsalesdb prodname))
       (def price1 (into [] price))
       (def pri (price1 0))
       (def priint (Float/parseFloat pri))
        (def quanint (Float/parseFloat quanti))
        (def cnt (* quanint priint ))
        (def counter (+ counter cnt))))
      (recur (- i 1))))
      (println name ":" "$"(format "%.2f" counter)))
(defn total_prod[prodname]
(def counter 0)
   (def saleval (vals @salesdb))
   (def salesvec (into [] saleval))
   (loop [i (count salesvec)]
   (when (>= i 0)
    (def firstrec (get salesvec i))
     (def nameofcust (get firstrec 0))
     (def prodnm (get salesvec i))
     (def prod-name (get prodnm 1))
     (def quant (get salesvec i))
     (def quanti (get quant 2))
     (def finalname (some #(= prodname %) prod-name))
     (if
      (= true finalname) 
      (do 
      (def counter (+ counter (Integer/parseInt quanti)))))
      (recur (- i 1))))
      (println prodname ":" counter))
     
    
(display_cust)
(display_prod)
(display_sales)
(defn func []
(println "***  SALES MENU  ***" )
(println "------------------------ ")
(println "1.Display customer table")
(println "2.Display product table")
(println "3.Display sales table")
(println "4.Total sales for the customer")
(println "5.Total count for the product")
(println "6.Exit")
(println "7.Enter your choice")
(def user_choice (Long/parseLong (read-line)))
        (cond
         (= user_choice 1) (do (sort_cust) (func))
         (= user_choice 2) (do (sort_prod) (func))
         (= user_choice 3) (do (sort_sales) (func))
         (= user_choice 4) 
             (do (println "enter customer name")
            (def namecust (read-line)) 
              (total_sales namecust)
              (func))
         (= user_choice 5) 
             (do (println "enter product name")
                (def prodname (read-line)) 
              (total_prod prodname)
              (func))
         (= user_choice 6) (println "GOOD BYE")
        
         :else (do (println "Please eneter a valid choice") (func))))

(func)