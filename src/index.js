const {Howl, Howler} = require('howler');
const {Elm} = require('./Main.elm');

import './styles.css'

var WebFont = require('webfontloader');

 WebFont.load({
   google: {
     families: ['Nanum Gothic']
   }
 });

var app = Elm.Main.init({
    node: document.getElementById('elm-app')
  });

console.log(app);

var sound = new Howl({
  src: [require('./assets/0.wav')],
  loop: true,
  volume: 0.5
});

var sounds = [];

for (var pc = 0; pc <= 11; pc++) {
  sounds.push(
    new Howl({
      src: [require(`./assets/${pc}.wav` )],
      loop: true,
      volume: 0.5,
    })
  );
  sounds[pc].mute(true);
  sounds[pc].play();
}

app.ports.audioControl.subscribe(function(data) {
  var pc = data[0];
  var audible = data[1];
  if (sounds[pc]) sounds[pc].mute(!audible);
});
