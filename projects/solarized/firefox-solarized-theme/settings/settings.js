function saveOptions(e) {

  console.log('save called');
  e.preventDefault();
  console.log('save step 2');
  browser.storage.local.set({
    "method": document.querySelector("#method").value,
    "hourStart": document.querySelector("#hourStart").value,
    "hourEnd": document.querySelector("#hourEnd").value,
    "accentColorForDark": document.querySelector("#accentColorForDark").value,
    "accentColorForLight": document.querySelector("#accentColorForLight").value,
    "apiKey": document.querySelector("#apiKey").value,
    "lat": document.querySelector("#lat").value,
    "long": document.querySelector("#long").value

  });
  console.log('save complete');
}

function restoreOptions() {
  console.log('restore start');

  function setCurrentChoice(result) {
    document.querySelector("#method").value = result["method"];
    document.querySelector("#hourStart").value = result["hourStart"];
    document.querySelector("#hourEnd").value = result["hourEnd"];
    document.querySelector("#accentColorForDark").value = result["accentColorForDark"];
    document.querySelector("#accentColorForLight").value = result["accentColorForLight"];
    document.querySelector("#apiKey").value = result["apiKey"];
	document.querySelector("#lat").value = result["lat"];
	document.querySelector("#long").value = result["long"]
  }

  function onError(error) {
    console.log(`Error: ${error}`);
  }

  let method = browser.storage.local.get("method");
  method.then(setCurrentChoice, onError);
  let hourStart = browser.storage.local.get("hourStart");
  hourStart.then(setCurrentChoice, onError);
  let hourEnd = browser.storage.local.get("hourEnd");
  hourEnd.then(setCurrentChoice, onError);
  let accentColorForDark = browser.storage.local.get("accentColorForDark");
  accentColorForDark.then(setCurrentChoice, onError);
  let accentColorForLight = browser.storage.local.get("accentColorForLight");
  accentColorForLight.then(setCurrentChoice, onError);
  let apiKey = browser.storage.local.get("apiKey");
  apiKey.then(setCurrentChoice, onError);
  let lat = browser.storage.local.get("lat");
  lat.then(setCurrentChoice, onError);
  let long = browser.storage.local.get("long");
  long.then(setCurrentChoice, onError);

  console.log('restore complete');
}

document.addEventListener("DOMContentLoaded", restoreOptions);
document.querySelector("form").addEventListener("submit", saveOptions);