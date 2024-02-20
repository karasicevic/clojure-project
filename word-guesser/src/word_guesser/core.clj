(ns word-guesser.core
  (:gen-class)
  (:require [clojure.string :as str]))

  (defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "Hello, World!"))

  5

;; lokalno definisane reči i njihovi vektori
  (def words-vector
    {"jabuka" [0.5 0.8 0.2]
     "kruška" [0.6 0.7 0.3]
     "šljiva" [0.55 0.75 0.25]
     "breskva" [0.58 0.78 0.22]
     "kajsija" [0.54 0.72 0.28]
     "malina" [0.61 0.79 0.21]
     "kupina" [0.59 0.77 0.23]
     "borovnica" [0.62 0.82 0.18]
     "ribizla" [0.57 0.76 0.24]
     "jagoda" [0.63 0.81 0.19]
     "ananas" [0.65 0.83 0.17]
     "papaja" [0.64 0.84 0.16]
     "banana" [0.66 0.85 0.15]
     "mandarina" [0.55 0.71 0.29]
     "narandža" [0.56 0.74 0.26]

     "luk" [0.4 0.2 0.7]
     "krompir" [0.38 0.22 0.68]
     "grašak" [0.35 0.25 0.65]
     "pasulj" [0.33 0.23 0.67]
     "bob" [0.37 0.21 0.69]
     "boranija" [0.36 0.24 0.66]
     "šargarepa" [0.34 0.26 0.64]
     "ren" [0.39 0.19 0.71]
     "paštanak" [0.32 0.27 0.63]
     "tikvica" [0.31 0.28 0.62]

     "sto" [0.1 0.5 0.4]
     "stolica" [0.12 0.48 0.42]
     "sudopera" [0.08 0.52 0.38]
     "šporet" [0.15 0.45 0.35]
     "frižider" [0.11 0.49 0.41]
     "mikrotalasna" [0.14 0.46 0.36]
     "aspirator" [0.13 0.47 0.37]
     "šank" [0.09 0.51 0.39]
     "čaša" [0.07 0.53 0.37]
     "tanjir" [0.06 0.54 0.36]
     "escajg" [0.05 0.55 0.35]
     "kašika" [0.04 0.56 0.34]
     "viljuška" [0.03 0.57 0.33]
     "nož" [0.02 0.58 0.32]
     "kutlača" [0.01 0.59 0.31]

     "kapa" [0.9 0.3 0.1]
     "kačket" [0.88 0.32 0.12]
     "šešir" [0.87 0.33 0.13]
     "šal" [0.85 0.35 0.15]
     "majica" [0.84 0.36 0.16]
     "duks" [0.83 0.37 0.17]
     "bluza" [0.82 0.38 0.18]
     "šorc" [0.81 0.39 0.19]
     "pantalone" [0.8 0.4 0.2]
     "trenerka" [0.79 0.41 0.21]
     "suknja" [0.78 0.42 0.22]
     "haljina" [0.77 0.43 0.23]
     "kaput" [0.76 0.44 0.24]
     "jakna" [0.75 0.45 0.25]
     "mantil" [0.74 0.46 0.26]
     "cipele" [0.73 0.47 0.27]
     "čizme" [0.72 0.48 0.28]

     "glava" [0.2 0.7 0.5]
     "oči" [0.22 0.68 0.48]
     "nos" [0.24 0.66 0.46]
     "čelo" [0.26 0.64 0.44]
     "obraz" [0.28 0.62 0.42]
     "usne" [0.3 0.6 0.4]
     "brada" [0.32 0.58 0.38]
     "uši" [0.34 0.56 0.36]
     "vrat" [0.36 0.54 0.34]
     "obrve" [0.38 0.52 0.32]})

;funkcija koja određuje sličnost između dve prosleđene reči na osnovu
;koordinata njihovih vektora
  (defn similarity [word1 word2]
    (- 1.0 (apply + (map * (get words-vector word1) (get words-vector word2)))))

;funkcija koja pronalazi 3 najsličnije reči za reč koja je prosleđena
;kao prvi parametar
(defn most-similar-words [word words-vectors]
  (->> words-vectors
       (remove #(= word (first %))) ; uklanja datu reč iz liste jer će za nju sličnost biti potpuna
       (sort-by #(similarity word (first %))) ; sortira reči prema sličnosti
       (take 3))) ; uzima prve 3 reči

;funkcija koja vraća nasumičnu reč, pod uslovom da ta reč već nije odigrana u 
;trenutnoj partiji
(defn random-word [played-words]
  (let [available-words (remove #(contains? played-words %) (keys words-vector))]
    (when (seq available-words)
      (let [random-word (rand-nth available-words)]
        random-word))))

;funkcija za sugestiju koja prikazuje prvo slovo i prazna mesta za ostala slova
(defn add-first-letter [word]
  (apply str (concat [(first word)]
                     (repeat (- (count word) 1) "_ ");oduzima se 1, jer nam je potreban 
                     ;broj '_' za sva slova osim za prvo
                     )))

;funkcija za sugestiju koja prikazuje prvo i poslednje slovo 
;i prazna mesta za ostala slova
(defn add-first-and-last-letter [word]
  (apply str (concat [(first word)]
                     (repeat (- (count word) 2) "_ ");oduzima se 2, jer nam je potreban 
                     ;broj '_' za sva slova osim za 2, prvo i poslednje slovo
                     [(last word)])))

5
(defn game-round [played-words]
  (let [current-word (random-word played-words)
        similar-words (most-similar-words current-word words-vector)
        target-word (last similar-words)]
    (println "Slična reč vašoj zagodentnoj reči je:" target-word)

    (loop [attempts 5]
      (cond
        (= attempts 0)
        (do
          (println "Nazalost niste pogodili zadatu reč. Zagonetna reč je bila:" current-word)
          :exit)

        :else
        (do
          (print "Vaš odgovor za zagonetnu reč je? ")
          (flush)
          (let [user-input (clojure.string/trim (read-line))]
            (if (= user-input current-word)
              (do
                (println "Uspešno ste pogodili zadatu reč!")
                :exit)
              (do
                (println (case attempts
                           5 (str "Još jedna slična reč je: " (second similar-words))
                           4 (str "Još jedna slična reč je: " (first similar-words))
                           3 (str "Broj slova u zadatoj reči je: " (count current-word))
                           2 (str "Prvo slovo zadate reči je: " (add-first-letter current-word))
                           1 (str "Poslednje slovo zadate reči je: " (add-first-and-last-letter current-word))))
                (recur (dec attempts))))))))))

(defn start-game []
  (loop [played-words []]
    (println "------------------------")
    (recur (conj played-words (game-round played-words)))))

(start-game)