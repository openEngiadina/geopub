(ns geopub.app.browse
  (:require [geopub.app.browse.sidebar :refer [sidebar]]
            [geopub.app.browse.type]
            [geopub.app.browse.object]))

(def routes
  [["/object/:iri"
    {:name ::object
     :view geopub.app.browse.object/view
     :parameters {:path {:iri string?}
                  :query {:ec string?}}}]

   ["/type/:type"
    {:name ::type
     :view geopub.app.browse.type/view
     :parameters {:path {:type string?}}}]])

