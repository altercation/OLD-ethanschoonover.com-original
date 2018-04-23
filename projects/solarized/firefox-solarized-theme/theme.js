//theming
var base03 = "#002b36";
var base02 =  "#073642";
var base01 =  "#586e75";
var base00 =  "#657b83";
var base0 =   "#839496";
var base1 =   "#93a1a1";
var base2 =   "#eee8d5";
var base3 =   "#fdf6e3";
var yellow =  "#b58900";
var orange =  "#cb4b16";
var red =     "#dc322f";
var magenta = "#d33682";
var violet =  "#6c71c4";
var blue =    "#268bd2";
var cyan =    "#2aa198";
var green =   "#859900";

var currentTheme = '';

const themes = {
  'light': {
    colors: {
      "accentcolor": base3,
      "textcolor": base01,
      "toolbar": base2,
      "toolbar_text": base00,
      "toolbar_field": base3,
      "toolbar_field_text": base01,
      "tab_line": magenta,
      "popup": base3,
      "popup_text": base01,
      "tab_loading": magenta,
      "icons": base00,
      "icons_attention": magenta,
      "toolbar_vertical_separator": base3,
      "toolbar_field_separator": base2
    },
  },
  'dark': {
    colors: {
      "accentcolor": base03,
      "textcolor": base1,
      "toolbar": base02,
      "toolbar_text": base0,
      "toolbar_field": base03,
      "toolbar_field_text": base1,
      "tab_line": cyan,
      "popup": base03,
      "popup_text": base1,
      "tab_loading": cyan,
      "icons": base0,
      "icons_attention": cyan,
      "toolbar_vertical_separator": base02,
      "toolbar_field_separator": base03
    }, 
  }
};

function setTheme(theme) {
  currentTheme = theme;
  browser.theme.update(themes[theme]);
}

function applyLight() {
  setTheme('light');
  console.log('light theme applied')
}
function applyDark() {
  setTheme('dark');
  console.log('dark theme applied');
}

////////////////////////////////////METHODS///////////////////////////////////////////

const date = new Date();
const hours = date.getHours();

async function timeMethod() {
  console.log('time method started')  

  let hourStart = await browser.storage.local.get("hourStart");
  let hourStartProp = hourStart["hourStart"];
  let hourEnd = await browser.storage.local.get('hourEnd');
  let hourEndProp = hourEnd["hourEnd"];

  console.log(hourStartProp);
  console.log(hourEndProp);

  if ((hours > hourStartProp) && (hours < hourEndProp)) {
      applyLight();
    } else {
      applyDark();
    }

  console.log('timeMethod complete');
  browser.alarms.onAlarm.addListener(timeMethod);
  browser.alarms.create('timeMethod', {periodInMinutes: 5});
}

var i = 1;
function manualMethod() {
  console.log('manual method started');

  if (i % 2 === 0) {
    applyLight();
    i++;
    console.log("light theme applied, i iterated to:" + i);
  } else {
    applyDark();
    i++;
    console.log("dark theme applied, i iterated to:" + i);
  }
}

async function weatherMethod() {
  console.log('weather method started')

  var lat = await browser.storage.local.get("lat");
  var latProp = lat["lat"];
  console.log(latProp);
  var long = await browser.storage.local.get("long");
  var longProp = long["long"];
  console.log(longProp);
  var apiKey = await browser.storage.local.get('apiKey');
  var apiKeyProp = apiKey["apiKey"];
  console.log(apiKeyProp);

  var URL = 'http://api.openweathermap.org/data/2.5/weather?lat='+latProp+'&lon='+longProp+'&APPID='+apiKeyProp;
  console.log("URL: "+ URL);

  fetch(URL)
  .then(response => response.json())
  .then(data => {
    var cloudPercent = data.clouds.all;
    console.log('cloud %: ' + cloudPercent);

    if (cloudPercent > 50) {
      applyLight();
    } else {
      applyDark();
    }

    browser.alarms.onAlarm.addListener(weatherMethod);
    browser.alarms.create('weatherMethod', {periodInMinutes: 5});
    console.log('weather method complete');
  });
}

/////////////////////////////ACTUAL WORK/////////////////////////////////////

async function accentHandler() {
    console.log('accent handler called');
    let accentColorLight = await browser.storage.local.get('accentColorForLight');
    let accentColorLightProp = accentColorLight["accentColorForLight"];
    let accentColorDark = await browser.storage.local.get('accentColorForDark');
    let accentColorDarkProp = accentColorDark["accentColorForDark"];
    console.log(accentColorLightProp);
    console.log(accentColorDarkProp);

    themes['light'].colors["tab_line"] = accentColorLightProp;
    themes['light'].colors["tab_loading"] = accentColorLightProp;
    themes['light'].colors["icons_attention"] = accentColorLightProp;

    themes['dark'].colors["tab_line"] = accentColorDarkProp;
    themes['dark'].colors["tab_loading"] = accentColorDarkProp;
    themes['dark'].colors["icons_attention"] = accentColorDarkProp;

    console.log('accents set');
}

function openSettings() {
  browser.runtime.openOptionsPage();
  console.log('settings opened');
}

async function methodHandler() {
  console.log("method handler called");
  let method = await browser.storage.local.get("method");
    
  let methodProp = method["method"];
  console.log(methodProp);

  if (methodProp == "manual") {
    console.log("manual method selected");
    browser.browserAction.setTitle({title: "Zen Fox: Manual"});
    browser.browserAction.onClicked.removeListener(openSettings)
    browser.browserAction.onClicked.addListener(manualMethod);
  } 
  else if (methodProp == "time") {
    console.log("time method selected");
    browser.browserAction.setTitle({title: "Zen Fox: Time"});
    timeMethod();
    browser.browserAction.onClicked.removeListener(manualMethod);
    browser.browserAction.onClicked.addListener(openSettings);
  } 
  else if (methodProp == "weather") {
    console.log("weather method selected");
    browser.browserAction.setTitle({title: "Zen Fox: Weather"});
    weatherMethod();
    browser.browserAction.onClicked.removeListener(manualMethod);
    browser.browserAction.onClicked.addListener(openSettings);
  }
}

function apply() {
  console.log('started apply');
  accentHandler();
  methodHandler();
}

apply();
openSettings();
browser.storage.onChanged.addListener(apply);