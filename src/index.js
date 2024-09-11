import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

// read from local storage here and pass in to app
var storedModel = localStorage.getItem("model");
var flags = storedModel ? JSON.parse(storedModel) : null;

const main = Elm.Main.init({
  node: document.getElementById("root"),
  flags,
});

main.ports.copyAnalysedGames.subscribe((text) => {
  navigator?.clipboard
    ?.writeText(text)
    ?.then(() => alert("Copied to clipboard"))
    ?.catch(() => "Failed to copy to clipboard, please copy manually");
});

main.ports.saveToLocalStorage.subscribe((modelJson) => {
  window.localStorage?.setItem("model", modelJson);
});

window.addEventListener("paste", (event) => {
  const text = event.clipboardData?.getData("text/plain");

  main.ports.paste.send(text);

  event.preventDefault();
});

main.ports.releasePointerCapture.subscribe((event) => {
  event.target.releasePointerCapture(event.pointerId);
});

window.addEventListener("pointermove", (event) => {
  main.ports.onPointerMove.send(event);
});

window.addEventListener("pointerup", (event) => {
  main.ports.onPointerUp.send(event);
});

serviceWorker.register();
