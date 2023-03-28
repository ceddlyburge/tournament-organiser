import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

const main = Elm.Main.init({
  node: document.getElementById("root"),
});

main.ports.copyAnalysedGames.subscribe((text) => {
  navigator?.clipboard?.writeText(text);
});

window.addEventListener("paste", (event) => {
  const text = event.clipboardData?.getData("text/plain");

  main.ports.paste.send(text);

  event.preventDefault();
});

serviceWorker.register();
