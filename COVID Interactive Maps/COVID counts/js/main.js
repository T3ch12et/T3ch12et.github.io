mapboxgl.accessToken =
    'pk.eyJ1IjoiamFrb2J6aGFvIiwiYSI6ImNpcms2YWsyMzAwMmtmbG5icTFxZ3ZkdncifQ.P9MBej1xacybKcDN_jehvw';
let map = new mapboxgl.Map({
    container: 'map', // container ID
    style: 'mapbox://styles/mapbox/dark-v10',
    zoom: 3.6, // starting zoom
    center: [-95, 37], // starting center
    projection:'albers'
});

const grades = [10, 100, 1000],
    colors = ['rgb(246,216,19)', 'rgb(253,141,60)', 'rgb(222,45,38)'],
    radii = [5, 12, 20];

map.on('load', () => {
    map.addSource('cases', {
        type: 'geojson',
        data: 'assets/us-covid-2020-counts.geojson'
});

map.addLayer({
    'id': 'cases-point',
        'type': 'circle',
        'source': 'cases',
        //'minzoom': 4,
        'paint': {
            // increase the radii of the circle as the zoom level and dbh value increases
            'circle-radius': {
                'property': 'cases',
                'stops': [
                    [{
                        zoom: 4,
                        value: grades[0]
                    }, radii[0]],
                    [{
                        zoom: 4,
                        value: grades[1]
                    }, radii[1]],
                    [{
                        zoom: 4,
                        value: grades[2]
                    }, radii[2]]
                ]
            },
            'circle-color': {
                'property': 'cases',
                'stops': [
                    [grades[0], colors[0]],
                    [grades[1], colors[1]],
                    [grades[2], colors[2]]
                ]
            },
            'circle-stroke-color': 'white',
            'circle-stroke-width': 1,
            'circle-opacity': 0.6
        }
    },
    'waterway-label'
);

map.on('click', 'cases-point', (event) => {
    new mapboxgl.Popup()
        .setLngLat(event.features[0].geometry.coordinates)
        .setHTML(`<strong>COVID Case Count:</strong> ${event.features[0].properties.cases}`)
        .addTo(map);
});


});

// create legend
const legend = document.getElementById('legend');

//set up legend grades and labels
var labels = ['<strong>Magnitude</strong>'], vbreak;
//iterate through grades and create a scaled circle and label for each
for (var i = 0; i < grades.length; i++) {
vbreak = grades[i];
// you need to manually adjust the radius of each dot on the legend 
// in order to make sure the legend can be properly referred to the dot on the map.
dot_radii = 2 * radii[i];
labels.push(
    '<p class="break"><i class="dot" style="background:' + colors[i] + '; width: ' + dot_radii +
    'px; height: ' +
    dot_radii + 'px; "></i> <span class="dot-label" style="top: ' + dot_radii / 2 + 'px;">' + vbreak +
    '</span></p>');

}
// add the data source
const source =
'<p style="text-align: right; font-size:10pt">Source: <a href="https://github.com/nytimes/covid-19-data/blob/43d32dde2f87bd4dafbb7d23f5d9e878124018b8/live/us-counties.csv">USGS</a></p>';
// combine all the html codes.
legend.innerHTML = labels.join('') + source;