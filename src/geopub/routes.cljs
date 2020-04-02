(ns geopub.routes
  (:require [reitit.frontend.easy :as rfe]
            [goog.string]
            [geopub.ui.activity]
            [geopub.ui.browse]
            [geopub.ui.store]
            [geopub.ui.map]
            [geopub.ui.settings]))

(def routes
  [["activity"
    {:name ::activity
     :view geopub.ui.activity/view}]

   ["browse/iri/:iri"
    {:name ::browse-iri
     :view geopub.ui.browse/description-view
     :parameters {:path {:iri string?}}}]

   ["browse/blank-node/:blank-node"
    {:name ::browse-blank-node
     :view geopub.ui.browse/description-view
     :parameters {:path {:blank-node string?}}}]

   ["browse"
    {:name ::browse
     :view geopub.ui.browse/browse-view
     :parameters {:query {:type string?}}}]

   ["store"
    {:name ::store
     :view geopub.ui.store/view}]

   ["map"
    {:name ::map
     :view geopub.ui.map/view}]

   ["settings"
    {:name ::settings
     :view geopub.ui.settings/view}]
   ])
