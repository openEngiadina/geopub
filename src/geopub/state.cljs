(ns geopub.state
  (:require [reagent.core :as r]))

(defonce state
  (r/atom {:actor-id "http://localhost:4000/users/alice"
           :actor {}
           :active-page :timeline
           :selected nil
           :hover nil
           :only-liked-status false}))
