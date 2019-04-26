/* global L, Headers, btoa, fetch, R, prompt */
window.onload = () => {
  // Initialize map with OpenStreetMaps tiles
  var mymap = L.map('mapid').setView([46.8, 10.283333], 13)

  L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    'useCache': true,
    attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
    maxZoom: 18
  }).addTo(mymap)

  // Login to ActivityPub Server
  const location = new URL(window.location.href)
  const user = location.searchParams.get('user')
  const password = location.searchParams.get('password')
  const url = location.searchParams.get('url')

  if (!(user && password && url)) {
    document.getElementById('login').style.display = 'inline'
    return
  }

  var headers = new Headers()
  headers.append('Authorization', 'Basic ' + btoa(user + ':' + password))
  headers.append('Content-type', 'application/activity+json')

  // Check who I am
  fetch(url + '/api/ap/whoami', {
    method: 'GET',
    headers: headers
  }).then((response) => {
    if (response.status !== 200) {
      document.getElementById('login').style.display = 'inline'
      throw new Error('Could not login')
    } else {
      document.getElementById('logged_in').style.display = 'inline'
      return response.json()
    }
  }).then((actor) => {
    console.log(actor)
    return actor
  }).then(getInbox)
    .then(setupNoteCreation)
    .catch((e) => { console.error(e) })

  // Get user inbox and process
  function getInbox (actor) {
    var inbox = actor.inbox

    // Hack to deal with localhost
    if (actor.id === 'http://localhost/users/admin') {
      inbox = 'http://localhost:4000/users/admin/inbox'
    }

    return fetch(inbox, {
      method: 'GET',
      headers: headers
    }).then((response) => {
      return response.json()
    }).then((inbox) => {
      return inbox.first.orderedItems
    }).then(R.map(handleObject))
      .then(() => { return actor })
  }

  // Process ActivityStream objects
  function handleObject (object) {
    // Ignore if it is not a Create of a Note
    if (object.type !== 'Create' ||
            object.object.type !== 'Note'
    ) { return }

    const note = object.object

    // Ignore if note does not have a location property
    if (!('location' in note)) { return }

    // Ignore if location does not have GeoCoordinates property
    if (!('geo' in note.location &&
              note.location.geo['@type'] === 'GeoCoordinates'
    )) { return }

    const coordinates = [note.location.geo.latitude, note.location.geo.longitude]

    console.log(coordinates)
    console.log(note)

    var marker = L.marker(coordinates).addTo(mymap)

    marker.bindPopup(note.content + '<hr><a href="' + note.actor + '"><i>' + note.actor + '</i></a>').openPopup()
  }

  // Create a new Note
  function postNote (actor, content, latlng) {
    // A simple ActivityStream Event
    const event = {
      'type': 'Create',
      'to': [
        'https://www.w3.org/ns/activitystreams#Public'
      ],
      'object': {
        'type': 'Note',
        'to': [
          'https://www.w3.org/ns/activitystreams#Public'
        ],
        'sensitive': false,
        'content': content,
        'cc': [
          actor.followers
        ],
        'attributedTo': actor.id,
        'attachment': [],
        'actor': actor.id,
        'location': {
          '@type': 'Place',
          'geo': {
            '@type': 'GeoCoordinates',
            'latitude': latlng.lat,
            'longitude': latlng.lng
          },
          'name': 'The place clicked on the map'
        }
      },
      'directMessage': false,
      'cc': [
        actor.followers
      ],
      'actor': actor.id,
      '@context': [
        'https://www.w3.org/ns/activitystreams',
        'http://localhost:4000/schemas/litepub-0.1.jsonld'
      ]
    }

    return fetch(url + '/users/' + user + '/outbox', {
      method: 'POST',
      body: JSON.stringify(event),
      headers: headers
    })
  }

  // Hook up clicking on map with posting a note
  function setupNoteCreation (actor) {
    mymap.on('click', (e) => {
      console.log(e.latlng)
      const content = prompt('Enter your message', 'Hi!')
      if (content) {
        postNote(actor, content, e.latlng).then((response) => {
          console.log('Note posted!')
          getInbox(actor)
        }).catch((e) => {
          console.error(e)
        })
      }
    })
  }
}
