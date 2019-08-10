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

var maxVolume = 0.3

var sound = new Howl({
  src: [require('./assets/0.wav')],
  loop: true,
  volume: maxVolume
});

// AUDIO
var sounds = [];

for (var tone = 0; tone <= 11; tone++) {
  sounds.push(
    new Howl({
      src: [require(`./assets/${tone}.wav` )],
      loop: true,
      volume: 0,
    })
  );
  sounds[tone].play();
}

app.ports.playTones.subscribe(function(tones) {
  for (var tone = 0; tone < tones.length; tone++) {
    if (sounds[tone].volume() > 0 && tones[tone] === false) {
      sounds[tone].fade(sounds[tone].volume(), 0, 100);
    } else if (sounds[tone].volume() === 0 && tones[tone] === true) {
      sounds[tone].fade(0, maxVolume, 100);
    }
  }
});
