import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
    node: document.getElementById('root')
});

const wordToSpeech = word => {
    const utterance = new SpeechSynthesisUtterance(word)
    utterance.lang = "ja-JP";

    try {
        window.speechSynthesis.speak(utterance)
    } catch (e) {
        console.error(e, "Could not synthesize speech")
    }
}

const portMappings = {
    speakWord: wordToSpeech
}

if (app && app.ports) {
    const portNames = Object.keys(app.ports)

    portNames.forEach(key => {
        app.ports[key].subscribe(portMappings[key])
    })
}

registerServiceWorker();
