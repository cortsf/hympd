{-# LANGUAGE QuasiQuotes #-}
module Javascript where
import Language.Javascript.JMacro
import Data.Text as T

jsblock :: T.Text
jsblock = T.show $ renderJs $ 
  [jmacro| 
         ///////////////////////////////// Global vars
         
         var refreshIntervalId;
         var currentTime = 0.0;

         ///////////////////////////////// Utility

         function updateAll (){
           if(confirm('Do you really want to update the entire database?')){
             socket.send('updateAll');
           };
         };

         function getCookie (name) {
           var re = new RegExp(name + "=([^;]+)");
           var value = re.exec(document.cookie);
           return (value != null) ? unescape(value[1]) : null;
         };

         function setCookie(name, value) {
           document.cookie=name + "=" + escape(value) + "; path=/; max-age=31536000";
           };

         function saveConfig() {
           setCookie('showArtistOnNavbar', document.querySelector('#showArtistOnNavbar').checked);
           setCookie('showPathOnNavbar', document.querySelector('#showPathOnNavbar').checked);
           setCookie('useDark', document.querySelector('#useDark').checked);
           location.reload()
         };

         function swapClasses (condition, fst_class, snd_class, node) { // Bool -> [String] -> [String] -> ()
           if(condition){
             node.classList.remove.apply(node.classList, snd_class);
             node.classList.add.apply(node.classList, fst_class);
           } else {
             node.classList.remove.apply(node.classList, fst_class);
             node.classList.add.apply(node.classList, snd_class);
           };
         };


         function time_from_seconds(seconds){ 
           if(seconds > 3600){
             return ('0'+Math.floor(seconds/3600) % 24).slice(-2)+':'+('0'+Math.floor(seconds/60)%60).slice(-2)+':'+('0' + seconds % 60).slice(-2);
           }else{
             return ('0'+Math.floor(seconds/60)%60).slice(-2)+':'+('0' + seconds % 60).slice(-2);
           };
         };

         ///////////////////////////////// Set UI funs

         function setProgress (stTime, stState) {  
           progressBar = document.querySelector('#playerProgressInput');
           
           if(stState == "Stopped"){
             clearInterval(refreshIntervalId);
             currentTime = 0;
             progressBar.disabled = true;
             progressBar.value=0;
             progressBar.style.backgroundSize = '0%';
             document.querySelector('#elapsedTime').innerHTML="00:00";
             document.querySelector('#totalTime').innerHTML="00:00";
           } else if (stState == "Paused"){
             clearInterval(refreshIntervalId);
             currentTime = stTime[0];
             progressBar.disabled = false;
             progressBar.value = (stTime[0] / stTime[1]) * 100.0;
             progressBar.style.backgroundSize = ((stTime[0] / stTime[1]) * 100.0) + '%';
             document.querySelector('#elapsedTime').innerHTML=time_from_seconds(Math.floor(stTime[0]));
             document.querySelector('#totalTime').innerHTML=time_from_seconds(Math.floor(stTime[1]));
           } else if (stState == "Playing"){
             clearInterval(refreshIntervalId);
             currentTime = stTime[0];
             progressBar.disabled = false;
             document.querySelector('#elapsedTime').innerHTML=time_from_seconds(Math.floor(stTime[0]));
             document.querySelector('#totalTime').innerHTML=time_from_seconds(Math.floor(stTime[1]));
             refreshIntervalId = setInterval(function(ival_totalTime) {
               if(currentTime >= ival_totalTime){
                     clearInterval(refreshIntervalId);
                     progressBar.value = 0;
                     currentTime = 0;
               } else {
                 currentTime = currentTime + 0.2;
                 document.querySelector('#elapsedTime').innerHTML=time_from_seconds(Math.floor(currentTime));
                 progressBar.value = (currentTime / ival_totalTime) * 100.0;
                 progressBar.style.backgroundSize = ((currentTime / ival_totalTime) * 100.0) + '%';
               };
             },200, stTime[1]);
           };
         }; 


         function setVolume (stVolume) { 
           document.querySelector('#navVolume').value=stVolume 
         };

         function setSongTitle (stState,  currentSong) { 
           if((stState != "Stopped")){
             document.querySelector('#currentSongTitle').innerHTML = currentSong.title;
             document.querySelector('#currentSongArtist').innerHTML = currentSong.artist;
             document.querySelector('#currentSongPath').innerHTML = currentSong.path;
           // } else {
           //   document.querySelector('#currentSongTitle').innerHTML='';
           //   document.querySelector('#currentSongArtist').innerHTML='';
           //   document.querySelector('#currentSongPath').innerHTML='';
           };
         };

         function highlightCurrentSongOnQueue (stSongID, stState) { 
           var current_item_classes = ["text-amber-500", "dark:text-orange-300"];
           if ( window.location.pathname == "/queue" ){
             if(stState == "Stopped"){
               document.querySelectorAll(".song-item").forEach(function(x){swapClasses(true, [], current_item_classes, x)});
             } else {
               document.querySelectorAll(".song-item").forEach(function(x){swapClasses(true, [], current_item_classes, x)});
               swapClasses(true, current_item_classes, [], document.querySelector("button[data-songId='"+stSongID+"']").parentNode);
             };
           };
         };

         function setPlaybackState (stState) { 
           if(stState == "Playing"){
             document.querySelector('#navPlayPause').innerHTML = '<i data-feather="pause"></i>'; 
           } else{
             document.querySelector('#navPlayPause').innerHTML = '<i data-feather="play"></i>'; 
           }
           feather.replace(); 
         };

         function setQueueButtons (status) {
           swapClasses(status.stRandom, ["text-lime-400", "hover:text-lime-600"], ["text-slate-400", "hover:text-slate-200"], document.querySelector('#btnRandom'));
           swapClasses(status.stConsume, ["text-lime-400", "hover:text-lime-600"], ["text-slate-400", "hover:text-slate-200"], document.querySelector('#btnConsume'));
           swapClasses(status.stSingle, ["text-lime-400", "hover:text-lime-600"], ["text-slate-400", "hover:text-slate-200"], document.querySelector('#btnSingle'));
           swapClasses(status.stRepeat, ["text-lime-400", "hover:text-lime-600"], ["text-slate-400", "hover:text-slate-200"], document.querySelector('#btnRepeat'));
         };

         function setUI (status, currentSong) {
           setSongTitle(status.stState, currentSong);
           setProgress(status.stTime, status.stState);
           setVolume(status.stVolume);
           setPlaybackState(status.stState);
           highlightCurrentSongOnQueue(status.stSongID, status.stState);
           if(window.location.pathname == "/queue"){
            setQueueButtons(status);
           };
         }

         function setConfig() {
           if(getCookie("showArtistOnNavbar") == 'true'){
             document.querySelector('#showArtistOnNavbar').checked = true;
           };
           if(getCookie("showPathOnNavbar") == 'true'){
             document.querySelector('#showPathOnNavbar').checked = true;
           };
           if(getCookie("useDark") == 'true'){
             document.querySelector('#useDark').checked = true;
           };
         };

         ///////////////////////////////// Socket

         socket = new WebSocket("ws://" + window.location.host + "/websocket");
         socket.onopen = function() {
           document.querySelector('#navPrevious').addEventListener('click', function() {socket.send('previous')}, false);
           document.querySelector('#navStop').addEventListener('click', function (){socket.send('stop')}, false);
           document.querySelector('#navPlayPause').addEventListener('click', function (){socket.send('toggle')}, false);
           document.querySelector('#navNext').addEventListener('click', function () {socket.send('next')}, false);
           addEventListener("visibilitychange", function () {if(!document.hidden){socket.send('status');};});
           socket.send('status')
         };

         socket.onmessage = function(event) {
           var msg_data = JSON.parse(event.data);
           // console.log('new_message: ' + JSON.stringify(msg_data));
           if(msg_data.payloadType == 'Payload'){
             if(msg_data.payload[0].includes("PlaylistS") && window.location.pathname == "/queue"){
               location.reload();
             }else{
               setUI(msg_data.payload[1], msg_data.payload[2]);
             };
           } else if(msg_data.payloadType == 'ClientResponse'){
             setUI(msg_data.payload);
           } else if(msg_data.payloadType == 'Error'){
             alert('ERROR: ' + msg_data.payload);
           };
         };

         socket.onerror = function(error_msg) {
           alert('ERROR: ' + error_msg);
         };

         addEventListener("load", function () {
           if ( window.location.pathname == "/settings" ){
             document.querySelector('#updateAll').addEventListener('click', updateAll);
             document.querySelector('#showArtistOnNavbar').addEventListener('click', saveConfig);
             document.querySelector('#showPathOnNavbar').addEventListener('click', saveConfig);
             document.querySelector('#useDark').addEventListener('click', saveConfig);
             setConfig();
           };
         });

         |]
