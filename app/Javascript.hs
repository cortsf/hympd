{-# LANGUAGE QuasiQuotes #-}
module Javascript where
import Language.Javascript.JMacro
import Data.Text as T

jsblock :: T.Text
jsblock = T.show $ renderJs $ [jmacro| 
                                     var refreshIntervalId;
                                     var currentTime = 0.0;

                                     function startInterval(startTime, totalTime) {
                                         clearInterval(refreshIntervalId);
                                         currentTime = startTime;
                                         refreshIntervalId = setInterval(function(ival_totalTime) {
                                           progressBar = document.querySelector('#playerProgress');
                                           if(progressBar.value >= 100){
                                                 //console.log(progressBar.value + ' is gt 100___ '+ival_totalTime+'___'+currentTime);
                                                 clearInterval(refreshIntervalId);
                                                 progressBar.value = 0;
                                                 currentTime = 0;
                                           } else {
                                             //console.log(progressBar.value + ' is lt 100___ '+ival_totalTime+'___'+currentTime);
                                             progressBar.value = (currentTime / ival_totalTime) * 100.0;
                                             currentTime = currentTime + 0.5;
                                           };
                                         },500, totalTime);
                                     };


                                     function stopInterval() {
                                         clearInterval(refreshIntervalId);
                                         currentTime = 0;
                                     };

                                     function setProgress (stTime, stState) {  
                                       progressBar = document.querySelector('#playerProgress');
                                       if(stState == "Stopped"){
                                         progressBar.disabled = true;
                                         progressBar.value=0;
                                         stopInterval();
                                       } else if (stState == "Paused"){
                                         progressBar.disabled = false;
                                         stopInterval();
                                       } else if (stState == "Playing"){
                                         progressBar.disabled = false;
                                         startInterval(stTime[0], stTime[1]);
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
                                       console.log('new_message: ' + JSON.stringify(status));
                                       setVolume(status.stVolume);
                                       //setSongTitleOnPlayer(status.stSongID, status.stState);
                                       setPlaybackState(status.stState);
                                       setProgress(status.stTime, status.stState);
                                       highlightCurrentSongOnQueue(status.stSongID, status.stState);
                                     };

                                     socket.onerror = function(error_msg) {
                                       alert('ERROR: ' + error_msg);
                                     };

                                     |]
