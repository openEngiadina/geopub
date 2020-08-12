(ns geopub.app.browse.type
  "Browse objects by type"
  (:require [re-frame.core :as re-frame]

            [rdf.core :as rdf]
            [rdf.logic]
            [cljs.core.logic :as l]

            [geopub.db :as db]
            
            [geopub.app.browse.sidebar :refer [sidebar]]
            [geopub.rdf.view :refer [rdf-term-component
                                     description-label-component
                                     description-component
                                     sort-descriptions-by-date]])
  (:require-macros [cljs.core.logic :refer [run*]]))

(re-frame/reg-sub
 ::current-type-descriptions
 (fn [_] [(re-frame/subscribe [:geopub.db/graph])
          (re-frame/subscribe [:geopub.router/current-route])])
 (fn [input b]
   (let [[graph current-route] input
         ;; extract the type from the current route
         type (-> current-route
                     (get-in [:path-params :type])
                     (goog.string.urlDecode)
                     (rdf/iri))]
     (when type
       [(rdf/description type graph)
        (map #(rdf/description % graph)
             (run* [subject] (rdf.logic/graph-typeo graph subject type)))]))))

(defn view []
  (let [type-description-sub (re-frame/subscribe [::current-type-descriptions])
        [type-description objects] @type-description-sub]
    [:div.ui-page
      [sidebar]
      [:div.main-container
       [:main
        [:h1 "Objects with type " [description-label-component type-description]]
        [:table.browse-list
         [:thead
          [:tr
           [:td "Name"]
           [:td "IRI"]]]
         [:tbody
          (for [desc objects]
            ^{:key (rdf/iri-value (rdf/description-subject desc))}
            [:tr
             [:td [description-label-component desc]]
             [:td [rdf-term-component (rdf/description-subject desc)]]])]]]]]))
