(ns geopub.rdf.post)

(defn post-rdf [data url & [opts]]
  (go
    (let [body (<! (n3/encode data))]
      (<! (http/post (if (rdf/iri? url) (rdf/iri-value url) url)
                     (merge
                      {:headers {"Content-type" "text/turtle"
                                 "Access-Control-Expose-Headers" "Location"}
                       :body body} opts))))))
