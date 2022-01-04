(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* Provides types for handling Geolocation of posts and users (according to [XEP-0080: User Location](https://xmpp.org/extensions/xep-0080.html)). *)

type t = {
  latitude : float;
  longitude : float;
  accuracy : float option;
  altitude : float option;
  altitude_accuracy : float option;
}

let make ?accuracy ?altitude ?altitude_accuracy latitude longitude =
  { latitude; longitude; accuracy; altitude; altitude_accuracy }

(* Namespace helpers *)

let geoloc_uri = "http://jabber.org/protocol/geoloc"
let ns local = (geoloc_uri, local)

(* XML Serializer *)

let to_xml geoloc =
  Xmlc.Tree.(
    make_element
      ~attributes:[ (Xmpp.Ns.xmlns "xmlns", geoloc_uri) ]
      ~children:
        (List.filter_map
           (fun x -> x)
           [
             Option.some
             @@ make_element (ns "lat")
                  ~children:[ make_data @@ Float.to_string geoloc.latitude ];
             Option.some
             @@ make_element (ns "lon")
                  ~children:[ make_data @@ Float.to_string geoloc.longitude ];
             Option.map
               (fun accuracy ->
                 make_element (ns "accuracy")
                   ~children:[ make_data @@ Float.to_string accuracy ])
               geoloc.accuracy;
             Option.map
               (fun altitude ->
                 make_element (ns "altitude")
                   ~children:[ make_data @@ Float.to_string altitude ])
               geoloc.altitude;
             Option.map
               (fun altitude_accuracy ->
                 make_element (ns "altaccuracy")
                   ~children:[ make_data @@ Float.to_string altitude_accuracy ])
               geoloc.altitude_accuracy;
           ])
      (ns "geoloc"))

(* XML Parser *)

let parser =
  Xmlc.Parser.(
    complex (ns "geoloc")
      ~required:[ ns "lat"; ns "lon" ]
      ~ignore_other:true
      (fun _attributes ->
        return
          {
            latitude = 0.;
            longitude = 0.;
            accuracy = None;
            altitude = None;
            altitude_accuracy = None;
          })
      [
        element (ns "lat") (fun _ ->
            data >>| Float.of_string >>| fun latitude ->
            (ns "lat", fun geoloc -> { geoloc with latitude }));
        element (ns "lon") (fun _ ->
            data >>| Float.of_string >>| fun longitude ->
            (ns "lon", fun geoloc -> { geoloc with longitude }));
        element (ns "accuracy") (fun _ ->
            data >>| Float.of_string >>| fun accuracy ->
            ( ns "accuracy",
              fun geoloc -> { geoloc with accuracy = Some accuracy } ));
      ])

(* Convert to Leaflet.LatLng *)

let to_latlng geoloc = Leaflet.LatLng.create geoloc.latitude geoloc.longitude
