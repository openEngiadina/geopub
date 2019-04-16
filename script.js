window.onload = () => {

    // Initialize map with OpenStreetMaps tiles
    var mymap = L.map('mapid').setView([46.8, 10.283333], 13);
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        'useCache': true,
        attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
        maxZoom: 18,
    }).addTo(mymap);


    // ActivityPub Client
    const url = "http://localhost:4000";
    const user = "admin";
    const password = "abc123";

    var headers = new Headers();
    headers.append('Authorization', 'Basic ' + btoa(user + ":" + password));


    // Process ActivityStream objects
    function handleObject(object){
        // Ignore if it is not a Create of a Note
        if (object.type != "Create"
            || object.object.type != "Note"
           ) { return; }

        const note = object.object;

        // Ignore if note does not have a location property
        if (!("location" in note)) { return; }

        // Ignore if location does not have GeoCoordinates property
        if (!("geo" in note.location
              && note.location.geo['@type'] == "GeoCoordinates"
             )) {return;}

        const coordinates = [note.location.geo.latitude, note.location.geo.longitude];

        console.log(coordinates);
        console.log(note);

        var marker = L.marker(coordinates).addTo(mymap);

        marker.bindPopup(note.content).openPopup();
    }

    // Get user inbox and process
    function getInbox(){
        fetch(url + "/users/" + user + "/inbox", {
            method: "GET",
            headers: headers
        }).then((response) => {
            return response.json();
        }).then((inbox) => {
            return inbox.first.orderedItems;
        }).then(R.map(handleObject));
    }

    getInbox();


    // Create a new Note
    function postNote(content, latlng){
        // A simple ActivityStream Event
        // TODO: remove a lot of hardcoded stuff
        const event = {
            "type": "Create",
            "to": [
                "https://www.w3.org/ns/activitystreams#Public"
            ],
            "published": "2019-04-03T09:48:51.845373Z",
            "object": {
                "type": "Note",
                "to": [
                    "https://www.w3.org/ns/activitystreams#Public"
                ],
                "tag": [],
                "summary": "A geo-located note",
                "sensitive": false,
                "content": content,
                "cc": [
                    "http://localhost/users/admin/followers"
                ],
                "attributedTo": "http://localhost/users/admin",
                "attachment": [],
                "actor": "http://localhost/users/admin",
                "location": {
                    "@type": "Place",
                    "geo": {
                        "@type": "GeoCoordinates",
                        "latitude": latlng.lat,
                        "longitude": latlng.lng
                    },
                    "name": "The place clicked on the map"
                }
            },
            "directMessage": false,
            "cc": [
                "http://localhost/users/admin/followers"
            ],
            "actor": "http://localhost/users/admin",
            "@context": [
                "https://www.w3.org/ns/activitystreams",
                "http://localhost:4000/schemas/litepub-0.1.jsonld"
            ]
        };

        myHeaders = headers.append('Content-type', 'application/activity+json');

        return fetch(url + "/users/" + user + "/outbox", {
            method: "POST",
            body: JSON.stringify(event),
            headers: headers
        });
    }

    mymap.on('click', (e) => {
        console.log(e.latlng);
        postNote("Hello from " + e.latlng.lat + "/" + e.latlng.lng, e. latlng).then((response) => {
            console.log("Note posted!");
            getInbox();
        }).catch((e) => {
            console.error(e);
        });
    });


}

