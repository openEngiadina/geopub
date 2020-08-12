(ns geopub.dispatch-async
  (:require [re-frame.core :as re-frame]
            [cljs.core.async :as async :refer [<!]])
  (:require-macros [cljs.core.async :refer [go-loop]]))

;; helper re-frame fx to dispatch all events in an async chan
(re-frame/reg-fx
 :dispatch-async
 (fn [chan]
   (go-loop []
     (when-let [event (<! chan)]
       (re-frame/dispatch event)
       (recur)))))
