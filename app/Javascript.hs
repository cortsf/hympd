{-# LANGUAGE QuasiQuotes #-}
module Javascript where
import Language.Javascript.JMacro
import Data.Text as T

jsblock :: T.Text
jsblock = T.show $ renderJs $ [jmacro| 
                                     var refreshIntervalId;
                                     var currentTime = 0.0;

                                     function time_from_seconds(seconds){ 
                                       if(seconds > 3600){
                                         return ('0'+Math.floor(seconds/3600) % 24).slice(-2)+':'+('0'+Math.floor(seconds/60)%60).slice(-2)+':'+('0' + seconds % 60).slice(-2);
                                       }else{
                                         return ('0'+Math.floor(seconds/60)%60).slice(-2)+':'+('0' + seconds % 60).slice(-2);
                                       };
                                     };

                                     function setProgressInput (stTime, stState) {  
                                       progressBar = document.querySelector('#playerProgressInput');
                                       document.querySelector('#totalTime').innerHTML=time_from_seconds(stTime[1]);
                                       
                                       if(stState == "Stopped"){
                                         clearInterval(refreshIntervalId);
                                         currentTime = 0;
                                         progressBar.disabled = true;
                                         progressBar.value=0;
                                         progressBar.style.backgroundSize = '0%';
                                       } else if (stState == "Paused"){
                                         clearInterval(refreshIntervalId);
                                         currentTime = stTime[0];
                                         progressBar.disabled = false;
                                         progressBar.value = (stTime[0] / stTime[1]) * 100.0;
                                         progressBar.style.backgroundSize = ((stTime[0] / stTime[1]) * 100.0) + '%';
                                       } else if (stState == "Playing"){
                                         clearInterval(refreshIntervalId);
                                         currentTime = stTime[0];
                                         progressBar.disabled = false;
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

                                     function setSongTitleOnPlayer (stSongID, stState) { 
                                       console.log('This implementation only works on queue. ' + stSongID + '_' + stState)
                                       //if(stState == "Stopped"){
                                       //  document.querySelector('#currentSong').innerHTML='..';
                                       //} else {
                                       //  document.querySelector('#currentSong').innerHTML=document.querySelector("div.songId[data-songId='"+stSongID+"']").innerHTML;
                                       //};
                                     };

                                     function highlightCurrentSongOnQueue (stSongID, stState) { 
                                       var current_class = 'text-blue-400';
                                       if ( window.location.pathname == "/queue" ){
                                         if(stState == "Stopped"){
                                           document.querySelectorAll(".song-item").forEach(function(x){x.classList.remove(current_class)});
                                         } else {
                                           document.querySelectorAll(".song-item").forEach(function(x){x.classList.remove(current_class)});
                                           document.querySelector("button[data-songId='"+stSongID+"']").parentNode.classList.add(current_class);
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

                                     // SOCKET ////
                                     socket = new WebSocket("ws://" + window.location.host + "/websocket");
                                     socket.onopen = function() {
                                       document.querySelector('#navPrevious').addEventListener('click', function() {socket.send('previous')}, false);
                                       document.querySelector('#navStop').addEventListener('click', function (){socket.send('stop')}, false);
                                       document.querySelector('#navPlayPause').addEventListener('click', function (){socket.send('toggle')}, false);
                                       document.querySelector('#navNext').addEventListener('click', function () {socket.send('next')}, false);
                                       socket.send('status')
                                     };

                                     socket.onmessage = function(event) {
                                       var status = JSON.parse(event.data);
                                       // console.log('new_message: ' + JSON.stringify(status));
                                       setProgressInput(status.stTime, status.stState);
                                       setVolume(status.stVolume);
                                       setPlaybackState(status.stState);
                                       highlightCurrentSongOnQueue(status.stSongID, status.stState);
                                       //setSongTitleOnPlayer(status.stSongID, status.stState);
                                     };

                                     socket.onerror = function(error_msg) {
                                       alert('ERROR: ' + error_msg);
                                     };

                                     |]
