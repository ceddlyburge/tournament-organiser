// import './fade-in.css';
// import './loading.css';
// import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const main = Elm.Main.init({
  node: document.getElementById('root')
});

main.ports.copyAnalysedGames.subscribe((text) => {
  console.log('hello', text);
  Navigator.clipboard.writeText(text);
})

window.addEventListener('paste', (event) => {
  const text = event.clipboardData?.getData('text/plain');
  
  main.ports.paste.send(text);
    
  event.preventDefault();
;});


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
