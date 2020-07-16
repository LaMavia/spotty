[%raw "require('isomorphic-fetch')"]
open ChildReprocess.Util;
open ChildReprocess.StdStream;
open Utils;
Dotenv.config(());
/*
let process =
  ChildReprocess.spawn(
    "youtube-dl", 
    [|"https://www.youtube.com/watch?v=15CXY6JAqbk"|],
    (),
  ); 

process
  ->child_stdout
  ->Readable.on_data(Js.log <.> Node_buffer.toString);
*/

Spotify.fetch_access_token()
>>- Js.log
// Spotify.make_auth->Lazy.force->Js.log
// (Env.client_id, Env.client_secret) <%> Js.log