(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt.Syntax
open Lwt_react

(* Setup logging *)

let src = Logs.Src.create "GeoPub"

module Log = (val Logs.src_log src : Logs.LOG)

(* GeoPub components *)

module Database = Geopub_database
module Xmpp = Geopub_xmpp

let view ~update (model : Model.t) =
  match model.route with
  | Route.About -> return [ Ui.geopub_menu model; Ui.about ]
  | Route.Activity latlng ->
      let* activity = Activity.view ?latlng ~update model in
      return [ Ui.geopub_menu model; activity ]
  | Route.Map ->
      let* map = Geopub_map.view model.database model.map in
      return [ Ui.geopub_menu model; map ]
  | Route.Query query ->
      let* query_view = Query.view model query in
      return [ Ui.geopub_menu model; query_view ]
  | Route.Inspect iri -> Inspect.view model iri
  | Route.Settings -> Settings.view ~update model

(* A small hack to invalidate the size of the Leaflet map when it is
   dynamically loaded. If not it would not be displayed correctly until a
   manual window resize. *)
let observe_for_map el map =
  let observer records _obs =
    let on_node node =
      match Jv.(to_option to_string @@ get node "id") with
      | Some "map" -> Geopub_map.invalidate_size map
      | _ -> ()
    in

    records
    |> Jv.to_list (fun x -> x)
    |> List.map (fun record ->
           Jv.to_list (fun x -> x) @@ Jv.get record "addedNodes")
    |> List.flatten |> List.iter on_node
  in
  let mutation_observer = Jv.get Jv.global "MutationObserver" in
  let observer = Jv.new' mutation_observer [| Jv.repr observer |] in
  let opts = Jv.obj [| ("childList", Jv.true'); ("subtree", Jv.false') |] in
  ignore @@ Jv.call observer "observe" [| El.to_jv el; opts |]

let main () =
  (* Setup logging *)
  Logs.set_reporter @@ Logs_browser.console_reporter ();

  (* Logs.set_level @@ Some Logs.Debug; *)
  Logs.set_level @@ Some Logs.Info;

  (* Initialize the application *)
  let () = Log.app (fun m -> m "Initializing GeoPub.") in

  (* Show a loading screen *)
  let body = Document.body G.document in
  El.set_children body @@ Ui.loading "Initializing GeoPub ... ";

  (* Initialize random generator *)
  Random.self_init ();

  (* Initialize Route *)
  let route = Route.init () in

  let route_updater =
    Route.update
    |> E.map (fun route (model : Model.t) -> return { model with route })
  in

  (* Model updates *)
  let model_update_e, update = E.create () in

  (* Initialize the database *)
  El.set_children body
  @@ Ui.loading
       "Initializing Database. This might take some time on first run... ";
  let* database = Database.init () in
  El.set_children body @@ Ui.loading "Initializing GeoPub ... ";

  (* Get activities in database *)
  let* activities = Activity.get_activities database in

  (* Update activities on database updates. *)
  let activities_update_e =
    E.map_s
      (fun () ->
        let* activities = Activity.get_activities database in
        return (fun (model : Model.t) -> return { model with activities }))
      Database.on_update
  in

  (* Initialize Map *)
  let* map =
    Geopub_map.init
      ~set_route:(fun route ->
        ignore @@ Route.set_route route;
        update (fun (model : Model.t) -> return { model with route }))
      ()
  in

  (* Initialize XMPP *)
  let xmpp = Loadable.Idle in

  (* let* xmpp = Xmpp.login_dev () >|= Loadable.of_result in *)
  let () =
    E.map_s
      (fun stanza ->
        let* rdf_opt = Xmpp.rdf_of_stanza stanza in
        match rdf_opt with
        | Some rdf ->
            Log.debug (fun m ->
                m "GeoPub received RDF over XMPP: %a" Rdf.Graph.pp rdf);
            Database.add_graph database rdf
        | None -> return_unit)
      Xmpp.stanzas
    |> E.keep
  in

  (* Initialize model *)
  let model_s =
    S.accum_s ~eq:( == )
      (E.select [ route_updater; model_update_e; activities_update_e ])
      { database; activities; route; map; xmpp }
  in

  (* Set UI *)

  (* Invalidate map size when it is added to the DOM *)
  observe_for_map body map;

  let* () =
    model_s
    |> S.map_s ~eq:( == ) (fun model ->
           (* Log.debug (fun m -> m "calling view function"); *)
           view ~update model >|= El.set_children body)
    >|= S.keep
  in

  let* () = Database.test_datalog database in
  let () = Log.app (fun m -> m "GeoPub ready.") in
  return_unit

let () =
  (Lwt.async_exception_hook :=
     fun exn ->
       Log.err (fun m ->
           m "unexpected async exception: %s" @@ Printexc.to_string exn));
  Lwt.on_any (main ()) ignore (function
    | Jv.Error error ->
        Log.err (fun m ->
            m "unexpected JavaScript exception: %s"
            @@ Jstr.to_string @@ Jv.Error.message error)
    | exn ->
        Log.err (fun m ->
            m "unexpected exception: %s" @@ Printexc.to_string exn))
