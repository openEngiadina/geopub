;; Copyright © 2020 pukkamustard <pukkamustard@posteo.net>
;;
;; This file is part of GeoPub.
;;
;; GeoPub is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GeoPub is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GeoPub.  If not, see <https://www.gnu.org/licenses/>.

(ns geopub.ns
  (:require-macros [rdf.core :refer [defns]])
  (:require [rdf.core]))

;; Commonly used namespaces

(defns as "https://www.w3.org/ns/activitystreams#")
(defns foaf "http://xmlns.com/foaf/0.1/")
(defns ldp "http://www.w3.org/ns/ldp#")
(defns schema "http://schema.org/")
(defns geo "http://www.w3.org/2003/01/geo/wgs84_pos#")
(defns dc "http://purl.org/dc/terms/")
