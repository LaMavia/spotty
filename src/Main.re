[%raw "require('isomorphic-fetch')"];
open ChildReprocess.Util;
open ChildReprocess.StdStream;
open Utils;
Dotenv.config();
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
let main = () => {
  Spotify.fetch_access_token()
  >>- Belt.Result.getExn
  >>= Spotify.fetch_playlist(_, "37i9dQZF1DWVzZlRWgqAGH")
  >>- Belt.Result.map(_, Youtube.get_playlist);
} /* Youtube.time_of_string("2:05")->Js.lo*/;

// Spotify.make_playlist_url("5ecOjPPqAXt7jwjQrS7A7T")->Js.log
// Spotify.make_auth->Lazy.force->Js.log
// (Env.client_id, Env.client_secret) <%> Js.log

// Utils.sync_future_consume(List.init(5000, a => a*2), a =>
//   Future.delay(5, () => Js.log(a*2))
// );
main()