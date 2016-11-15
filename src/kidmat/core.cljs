(ns kidmat.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.pprint :refer [pprint]]))

(enable-console-print!)

(defonce app-state (atom {:text "Hello kidmat!"}))

(defn gen-data [exs-count]
  (into {}
        (doall
         (map
          (fn [idx a b]
            ^{:key idx}
            (let [operation (rand-nth [:times :division :plus :minus])]
              [idx {:a a :b b :oper operation :val nil}]))
          (take exs-count (range 0 exs-count))
          (take exs-count (repeatedly #(rand-int 11)))
          (take exs-count (repeatedly #(rand-int 11)))))))

(defn exs [exs-count]
  (let [opers [:times :division :plus :minus]
        opers-fns {:times * :division / :plus + :minus -}
        opers-lits {:times "x" :division ":" :plus "+" :minus "-"}
        data (reagent/atom (gen-data @exs-count))]
    (fn []
      [:div
       (doall
        (map
         (fn [[idx ex]]
           ^{:key idx}
           [:div (str (:a ex) " " ((:oper ex) opers-lits) " " (:b ex) " = ")
            [:input {:type "text"
                     :value (str (get-in @data [idx :val]))
                     :on-change #(swap! data assoc-in [idx :val] (-> % .-target .-value))}]])
         @data))

       [:input {:type "button" :value "Click me!"
                :on-click #(pprint @data)}]])))

(defn kidmat []
  (let [exs-count (reagent/atom 10)
        exs-count-val (reagent/atom (str @exs-count))]
    (fn []
      [:div
       [:h1 (:text @app-state)]
       [:div {:style {:margin "20px 0 20px 0"}}
        "Give me "
        [:input {:style {:width "30px"}
                 :type "text"
                 :value (str @exs-count-val)
                 :on-change #(reset! exs-count-val (-> % .-target .-value))}]
        [:span " new exercises "]
        [:input {:type "button" :value "now!"
                 :on-click #(reset! exs-count (js/parseInt @exs-count-val))}]]

       ^{:key @exs-count}
       [exs exs-count]])))


(reagent/render-component [kidmat]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
