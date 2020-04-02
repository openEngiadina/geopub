(ns geopub.ui.about
  (:require [shadow.resource :as rc]))

(defn view [state]
  [:div.ui-page
   [:main.about
    [:h1 "About"]

    [:p "GeoPub is an application for managing open local knowledge."]

    [:p "For more information see "
     [:a {:href "https://openengiadina.net/" :target "_blank"}
      "https://openengiadina.net/"] "."]

    [:p [:b "GeoPub is alpha software and only suitable for demonstration purposes."]]

    [:h2 "CORS and requests"]

    [:p "GeoPub retrieves content from multiple sources without server support. While doing that it makes a lot of GET requests to different hosts. Hosts generally do not allow such requests from other origins (cross-origin ressource sharing - CORS)."]

    [:p "To allow CORS for all hosts install a browser add-on such as "
     [:a {:href "https://addons.mozilla.org/en-US/firefox/addon/cors-everywhere/" :target "_blank"} "CORS Everywhere"] "."]

    [:p "Also check if any privacy add-on (e.g. Privacy Badger) is blocking requests."]

    [:p "We are thinking of ways to improve this situation. GeoPub is an unusual Web application."]

    [:h2 "License"]
    [:p "GeoPub is free software and is licensed under the GNU General Public License version 3 (GPLv3) or newer. For more information see the COPYING file in the " [:a {:href "https://gitlab.com/openengiadina/geopub/" :target "_blank"} "project repository."]]

    [:h2 "Changelog"]
    [:pre (rc/inline "./CHANGELOG.md")]]
   ])
