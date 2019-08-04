const {Howl, Howler} = require('howler');
const {Elm} = require('./Main.elm');
import './styles.css'

// FONTS
var WebFont = require('webfontloader');

WebFont.load({
 google: {
   families: ['Nanum Gothic']
 }
});

// ELM
var app = Elm.Main.init({
    node: document.getElementById('elm-app')
  });

var sound = new Howl({
  src: [require('./assets/0.wav')],
  loop: true,
  volume: 0.5
});

// AUDIO
var sounds = [];

for (var pc = 0; pc <= 11; pc++) {
  sounds.push(
    new Howl({
      src: [require(`./assets/${pc}.wav` )],
      loop: true,
      volume: 0,
    })
  );
  sounds[pc].play();
}

app.ports.audioControl.subscribe(function(data) {
  var pc = data[0];
  var audible = data[1];
  console.log(sounds[pc].volume());
  if (audible && sounds[pc].volume() == 0) {
    sounds[pc].fade(0, 0.5, 20);
  } else if (sounds[pc].volume() > 0) {
    sounds[pc].fade(sounds[pc].volume(), 0, 20);
  }
});
