(ns geopub.rdf.view
  "Reagent components for displaying RDF data"
  (:require [rdf.core :as rdf]
            [rdf.logic :as rdf-logic]
            [rdf.graph.map]
            [rdf.ns :refer [rdf rdfs owl]]
            [cljs.core.logic :as l]

            [reagent.core :as r]
            [re-frame.core :as re-frame]
            [reitit.frontend.easy :as rfe]

            [geopub.ns :as ns :refer [as ogp schema foaf dc]]
            [geopub.view.link :refer [link-component]]

            ["date-fns" :as date-fns])
  (:require-macros [cljs.core.logic :refer [run fresh]]))

;; Reagent components

(defn iri-component
  "Render an IRI as a link that can be followed in the internal browser."
  [iri & [opts]]
  (cond
    (rdf/iri? iri)
    [:span.iri
     (if-not (:disable-href opts)

       ;; create a href
       [link-component
        (rdf/iri-value iri)
        :geopub.app.browse/browse-description
        {:iri (goog.string.urlEncode (rdf/iri-value iri))}]

       ;; only display the iri
       (rdf/iri-value iri))]

    (seq? iri)
    (iri-component (first iri))

    :else
    [:span.iri "-"]))

(defn literal-component [literal & [opts]]
  [:span {:dangerouslySetInnerHTML {:__html (rdf/literal-value literal)}}])

(defn blank-node-component [bnode & [opts]]
  (str "_:" (rdf/blank-node-id bnode)))

(defn rdf-term-component [term & [opts]]
  (cond
    (rdf/iri? term) [iri-component term opts]

    (rdf/literal? term) [literal-component term opts]

    (rdf/blank-node? term) [blank-node-component term opts]

    (seq? term) [rdf-term-component (first term) opts]

    :else "-"))

;; Description

(defn rdfs-type-labelo [graph subject label]
  (fresh [rdf-type]
    (rdf-logic/graph-tripleo graph (rdf/triple subject (rdf "type") rdf-type))
    (rdf-logic/graph-tripleo graph (rdf/triple rdf-type (rdfs "label") label))))

(defn description-label-term
  "Returns a suitable RDF Term that can be used as a label for the description."
  [desc & [opts]]
  (first
   (run 1 [label]
     (l/conda

      ;; use Activitystreams preferredUsername
      ((rdf-logic/description-tripleo desc (as "preferredUsername") label))

      ;; use foaf nick
      ((rdf-logic/description-tripleo desc (foaf "nick") label))

      ;; use Activitystreams name
      [(rdf-logic/description-tripleo desc (as "name") label)]
      
      ;; use dc:title
      [(rdf-logic/description-tripleo desc (dc "title") label)]

      ;; use schema.org name
      ((rdf-logic/description-tripleo desc (schema "name") label))

      ;; use ogp title
      ((rdf-logic/description-tripleo desc (ogp "title") label))

      ;; use rdfs label
      ((rdf-logic/description-tripleo desc (rdfs "label") label))

      ;; use the rdfs label of the type
      [(rdfs-type-labelo (rdf/description-graph desc)
                         (rdf/description-subject desc)
                         label)]

      ;; Fall back to using the subject IRI as label
      [(l/== (rdf/description-subject desc) label)]))))


(defn activity-streams-icono [desc image]
  (fresh [image-link]
    (rdf-logic/description-tripleo desc (as "icon") image-link)
    (rdf-logic/graph-tripleo (rdf/description-graph desc)
                             (rdf/triple image-link (as "url") image))))

(defn description-icon-src
  "Returns a string that can be used as src for an icon."
  [desc & [opts]]
  (->> (run 1 [image]
         (l/conda
          ((rdf-logic/description-tripleo desc (ogp "image") image))

          ((rdf-logic/description-tripleo desc (schema "image") image))

          ((rdf-logic/description-tripleo desc (rdf/iri "http://rdfs.org/sioc/ns#avatar") image))

          ((activity-streams-icono desc image))

          ((l/== image nil))))
       (map (fn [image-term]
              (if (rdf/literal? image-term)
                ;; for some reason ogp stores image as literal..wtf?
                (rdf/iri (rdf/literal-value image-term))
                image-term)))
       (first)))

(defn description-label-component [desc & [opts]]
  (let
      [subject (rdf/description-subject desc)
       label-term (description-label-term desc opts)]

    (if
        (and
         ;; label term is not an iri
         (not (rdf/iri? label-term))
         ;; or blank-node
         (not (rdf/blank-node? label-term))

         ;; subject is an iri or blank-node
         (or (rdf/iri? subject)
             (rdf/blank-node? subject))

         ;; we are not disabling hrefs in general
         (not (:disable-href opts)))

      ;; then make the component a clickable link
      [link-component
       [rdf-term-component label-term opts]
       :geopub.app.browse/browse-description
       {:iri
        (-> desc
            (rdf/description-subject)
            (rdf/iri-value)
            (goog.string.urlEncode))}]
      
      ;; else just display as rdf-term
      [rdf-term-component label-term opts])))


(defn description-property-list-component [desc & [opts]]
  [:dl
   (for
       [triple (cond->> (rdf/triple-seq desc)
                 (contains? opts :properties)
                 (filter #(contains? (:properties opts)
                                     (rdf/triple-predicate %))))]
     
       ^{:key (hash triple)}

       [:div.desc-property-list

        [:dt [description-label-component
              (rdf/description-move desc (rdf/triple-predicate triple))]]

        [:dd
         (let [object (rdf/triple-object triple)]
           (if (or (rdf/iri? object) (rdf/blank-node? object))
             [description-label-component (rdf/description-move desc object)]
             [rdf-term-component object]))]])])


(defn description-comment-term
  "Returns a short summary or comment of the description"
  [desc]
  (first
   (run 1 [content]
     (l/conda
      [(rdf-logic/description-tripleo desc (as "summary") content)]
      [(rdf-logic/description-tripleo desc (ogp "description") content)]
      [(rdf-logic/description-tripleo desc (rdfs "comment") content)]
      [(l/== content nil)]))))

(defn description-content-term
  "Returns content of the description"
  [desc]
  (first
   (run 1 [content]
     (l/conda
      [(rdf-logic/description-tripleo desc (as "content") content)]
      [(l/== content nil)]))))

(defn description-created-at
  "Return a date that describes when the description was created"
  [desc]
  (some->> (run 1 [c]
             (l/conda
              [(rdf-logic/description-tripleo desc (as "published") c)]
              [(l/== c nil)]))
           (first)
           (rdf/literal-value)
           (new js/Date)))

(defn description-creator
  "Returns a description of who created the description"
  [desc]
  (some->> (run 1 [c]
             (l/conda
              [(rdf-logic/description-tripleo desc (as "actor") c)]
              [(rdf-logic/description-tripleo desc (as "attributedTo") c)]
              [(l/== c nil)]))
           (first)
           (rdf/description-move desc)))

(def default-metadata
  #{(as "actor")
    (as "to")
    (as "cc")
    (as "inReplyTo")
    (as "attributedTo")
    (as "published")
    (as "outbox")
    (rdf "type")
    (schema "url")})

(defn description-component
  [desc & [opts]]
  [:section.description

   (let [label-term (description-label-term desc)]

     [:header

      ;; Icon
      (if-let [icon-src (description-icon-src desc)]
        [:img.icon {:src (rdf/iri-value icon-src)}])
    
      [:div
       [:h1 [rdf-term-component label-term]]

       ;; display the iri
       [:span.raw-id [rdf-term-component
                      (rdf/description-subject desc)
                      (merge opts {:external-href true})]]

       ;; if there is a comment (or short description) display it
       (if-let [comment (description-comment-term desc)]
         [:p.comment
          [rdf-term-component comment]])

       ;; display common meta data
       [:div.meta
        [description-property-list-component desc
         (merge {:properties default-metadata} opts)]]]])


   (if-let [content (description-content-term desc)]
     [:div.description-content
      [rdf-term-component content]])

   [:details
    [:summary "All Properties"]
    [description-property-list-component desc]]])
