(ns cljs-lab.core
  (:require
    [clojure.browser.repl :as repl]
    [reagent.core :as r]))

; (defonce conn
;   (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(def cipher [
  "COOUSULYDUTQOHYSEELPEUTSTGTOAR"
  "IDTHMWPEERDTTEFEXUTOROSECUYCOU"
  "DUBEULUONLIKFTEYHCERLROTUESAOF"
  "ANRAIEQSORETLERHTFTEUISEISDQBY"
  "LSFOSERTINNRGEDAWTORKLEQAIASNT"
  "RFEXLOASMPTBOTWUHEERISTWDEOIUL"
  "ETASVLTAGIUYASTOTEARPNOINGNITI"
  "HOEDYTETCROTNQTIUPESROKEYMNTLA"
  "ITNKSHIITIOHNLCEYTDEANNDHTREIG"
  "ITNGOHTTODOONAHDTTETLMLIOIOENS"
  "LDLAOORFSYDMEFSARUOMILNGOLEENR"
  "OSCKOTBNEFHEECATORMPEYCLDDANRE"
  "KARUYNPBTCUONDCFSTSUEANRMSOHNE"
  "YLEENOTPRAIIONDTFSUNRNEAHDCAHT"
  "ETNETBFAITOEMPSCAHNYLMOYWPEEES"
  "OOSTILTHHETRIRNEERMEVSINNAGTSA"
  "OPVIERDDERFITOIELLOWMCECUOONNS"
  "SEINIRCMALNOIACRWFHOANFFTTOARD"
  "YPTHEAEOIRGEYBSRINLLVIESINTIED"
  "UYRCAOONMPEYCAIBUNSEIEEVNLEBDI"
  "ONNATRNEDINFANLICSIATTEMSAESNT"
  "NAOTKMSIEETNORVGEICOYRLONMSETI"
  "TEMEUSNVTBOIBEVDLTIEATTYEHOEUR"
  "OPYEELEMSDVEEMARUSCHRTTHYEEFIR"
  "SAOFDRHEARKOMAGRKWINONNAORNEDY"
  "CRHIWIKUNOWOILVULYNEERADSRTEHI"
  "IMLAOANEDCNLTBTDEUBOEEDWIRIHTH"
  "TBUNSULTESAORELUWYILTNOHYGAIVE"
  "NUAMDREOGOIWNAINMOERHAISYNTCOR"
  "NSEOSOFAITSOEBNRUWSICSRIASMEIN"
  "OSUHAYALVENHCETANCOWGRHTGIAONE"
  "IEOUOGSRWRNGDEHIENDWYLOUTENIET"
  "EDLLRWOEVELMLIAIOANMFYENSORNON"
  "LMOYWPEEESFRINIEAENCYLDETLVAAS"
  "DTWHHEEANTMCPAEONEYDRLEDKABCAN"
  "TUCYTPARNDREREEITHIRTEPLWNAMNS"
  "WRIPUEEEDODAENMNRTONADNAOESATR"
  "IOCAOMLNPRDIURTTIFNGAELINCFHOR"
  "NAERREGIYCSSLAEISITYSRARLAEASU"
  "ETRETHATRESOANFUDHSOSOUMWNECRS"
  "ROEULANHABPTAYIOTEHESBICRAERNE"
  "IYLLDBSGANLHARTEGTESLTITTIYUIN"
  "TEATBSEHISRNUPEKTATHYEORMWKNTI"
  "ESPODRRETETHYOLAUTSOLMLIOIODNW"
  "OTFENHNRROCTKWEOHSILRGESEGSAIV"
  "RYGIHUNLGTMCPAEONEYSOPYEOLEMST"
  "PEBUGEYKINOTNAHDTIETOSNETMYITO"
  "UENDTFSHSEOPHEETLUPRRATHVIEPLI"
  "FSTHAOOESEIECAURNMSHYTENSBRRON"
  "ENRHEDAUNDDANEINFDARDUEAGSLOIN"
  "HOERETWSISWOILVULYHASLTAEOLELR"
  "CPTFMERSOMDNECHYEANTWRORGDKAIN"
  "TNRUNDSATIEARISMCGANHFOMTWYOOU"
  "AODVAKAONTAEYGAMOGDHEERCYMYVON"
  "SUOUNRLOSIEELYIRCCHRATNWNICSHU"
  ])

(defn transpose [lines]
  (into []
    (for [i (range (count (get lines 0)))]
      (apply str
        (for [l lines] (get l i))))))

(defn column [props col]
  [:div.column props
    (for [[j char] (zipmap (range) col)]
      [:div.char {:key j} char])])

(defn toggle [sel i]
  (if (sel i)
    (disj sel i)
    (conj sel i)))

(defn col-swap [cols i j]
  (if (not= i j)
    (assoc cols i (cols j) j (cols i))
    cols))

(defn right-min [sel n]
  (loop [m n]
    (if (and (sel (dec m)) (not= 0 m))
      (recur (dec m))
      m)))

(defn left-max [sel n]
  (loop [m -1]
    (if (and (sel (inc m)) (not= n m))
      (recur (inc m))
      m)))

(defn movable-left [sel n]
  (let [lm (left-max sel n)]
    (sort < (filter #(> % lm) sel))))

(defn movable-right [sel n]
  (let [rm (right-min sel n)]
    (sort > (filter #(< % rm) sel))))

(defn move-left [{sel :sel cols :cols} n]
  (loop [moving (movable-left sel n) cols' cols sel' sel]
   (if (empty? moving)
    {:cols cols' :sel sel'}
    (recur
      (rest moving)
      (col-swap cols' (dec (first moving)) (first moving))
      (conj (disj sel' (first moving)) (dec (first moving)))))))

(defn move-right [{sel :sel cols :cols} n]
  (loop [moving (movable-right sel n) cols' cols sel' sel]
   (if (empty? moving)
    {:cols cols' :sel sel'}
    (recur
      (rest moving)
      (col-swap cols' (first moving) (inc (first moving)))
      (conj (disj sel' (first moving)) (inc (first moving)))))))

(defn inc-range [a b]
  (range (min a b) (inc (max a b))))

(defn select-range [sel add? a b]
  (let [sel' (set (inc-range a b))]
    (if add? (clojure.set/union sel sel') sel')))

;; To handle crtl+shift correctly we would need to remember the last
;; selected column. One more atom in transposition-solver. For now
;; we select a range if there is only one column selected, which
;; doesn't need any state.
(defn select-cols [i state]
  (fn [ev]
    (if (.-shiftKey ev)
      (when (= 1 (count (:sel @state)))
        (swap! state update-in [:sel] select-range true i (first (:sel @state))))
      (if (.-ctrlKey ev)
        (swap! state update-in [:sel] toggle i)
        (swap! state assoc-in [:sel] #{i})))))

(defn col-key-handler [i state]
  (fn [ev]
    (case (.-which ev)
      (72 37) ;; left
      (swap! state move-left (count (:cols @state)))
      
      (76 39) ;; right
      (swap! state move-right (count (:cols @state)))
      
      83 ;; s
      (let [sel (:sel @state)]
        (and (= 2 (count sel))
          (swap! state update-in [:cols] col-swap (first sel) (second sel))))

      32 ;; space
      (do
        (.preventDefault ev)
        ((select-cols i state) ev))

      nil)))

(defn transposition-solver [cipher]
  (let [state (r/atom {:sel #{} :cols (transpose cipher)})]
    (fn []
      [:div.transposition-solver
        (let [sel (:sel @state)]
          (for [[i col] (zipmap (range) (:cols @state))]
            [column {:key i
                     :tab-index (inc i)
                     :on-key-down (col-key-handler i state)
                     :on-click (select-cols i state)
                     :style (and (sel i) {:background-color "#a3c7fc"})}
              col]))])))

(r/render [(transposition-solver cipher)] (js/document.querySelector "#app"))