(ns kidmat.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.pprint :refer [pprint]]))

(enable-console-print!)

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

(defn eval-result [correct-result player-result]
  (cond
    (= (js/parseInt player-result) correct-result)
    [:span {:style {:color "green"}} "Great!"]
    (nil? player-result)
    [:span [:b {:style {:color "red"}} "Missed!"]
     [:span " Should be "]
     [:b {:style {:color "green"}} (str correct-result)]]
    :else
    [:span [:b {:style {:color "red"}} "Wrong!"]
     [:span " Should be "]
     [:b {:style {:color "green"}} (str correct-result)]]))

(defn exs [app-state exs-count data]
  (let [opers [:times :division :plus :minus]
        opers-fns {:times * :division / :plus + :minus -}
        opers-lits {:times "x" :division ":" :plus "+" :minus "-"}
        result (reagent/atom nil)]
    (fn []
      [:div
       (when (:state @app-state)
         [:div
          [:span {:style {:color "green"}} (str "Correct " (:correct @result))]
          [:span " - "]
          [:span {:style {:color "red"}} (str "Wrong: " (:wrong @result))]
          [:hr]])
       (doall
        (map
         (fn [[idx ex]]
           ^{:key idx}
           [:div (str (:a ex) " " ((:oper ex) opers-lits) " " (:b ex) " = ")
            [:input (merge
                     {:type "text"
                      :value (str (get-in @data [idx :val]))
                      :on-change #(swap! data assoc-in [idx :val] (-> % .-target .-value))}
                     (when (:state @app-state)
                       {:disabled true}))]
            (when (:state @app-state)
              (let [correct-result (((:oper ex) opers-fns) (:a ex) (:b ex))
                    player-result (get-in @data [idx :val])]
                [eval-result correct-result player-result]))])
         @data))

       [:div {:style {:margin "20px 0"}}
        [:input {:type "button" :value "I'm done!"
                 :on-click #(do
                              (swap! app-state assoc :state :finished )
                              (reset! result (reduce
                                              (fn [out [idx ex]]
                                                ^{:key idx}
                                                (let [correct-result (((:oper ex) opers-fns) (:a ex) (:b ex))
                                                      player-result (get-in @data [idx :val])]
                                                  (cond
                                                    (= (js/parseInt player-result) correct-result)
                                                    (update-in out [:correct] inc)
                                                    (nil? player-result)
                                                    (update-in out [:wrong] inc)
                                                    :else
                                                    (update-in out [:wrong] inc))))
                                              {:correct 0 :wrong 0}
                                              @data)))}]]])))

(defn kidmat []
  (let [app-state (reagent/atom {})
        exs-count (reagent/atom 10)
        exs-count-val (reagent/atom (str @exs-count))
        data (reagent/atom (gen-data @exs-count))]
    (fn []
      [:div
       [:div {:style {:margin "20px 0 20px 0"}}
        "Give me "
        [:input {:style {:width "30px"}
                 :type "text"
                 :value (str @exs-count-val)
                 :on-change #(reset! exs-count-val (-> % .-target .-value))}]
        [:span " new exercises "]
        [:input {:type "button" :value "now!"
                 :on-click #(do
                              (swap! app-state assoc :state nil)
                              (reset! exs-count (js/parseInt @exs-count-val))
                              (reset! data (gen-data @exs-count)))}]]
       ^{:key @exs-count}
       [exs app-state exs-count data]])))


(reagent/render-component [kidmat]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
