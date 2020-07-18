open Utils;
open ChildReprocess.Util;

[@decco.decode]
type token_res = 
  { access_token : string
  , token_type   : string
  , expires_in   : int
  };

[@decco.decode]
type artist =
  { name : string
  , href : string
  };

[@decco.decode]
type album = { artists : array(artist) };

[@decco.decode]
type track = 
  { name        : string
  , duration_ms : int
  , album       : album
  };

[@decco.decode]
type item = { track: track };

[@decco.decode]
type playlist_res = { items : array(item) }

let encode_base64 = s => Node_buffer.(
  fromString(s)
    -> toStringWithEncoding(`base64)
);

let make_playlist_url = (id: string) => {j|https://api.spotify.com/v1/playlists/$id/tracks?market=ES&fields=items.track(album(images%2C%20artists(name%2C%20href))%2C%20name%2C%20duration_ms)|j};

let make_auth = lazy({
  open Env;
  let encoded = encode_base64({j|$client_id:$client_secret|j});

  {j|Basic $encoded|j}
});

let id_of_link = link => {
  Js.Re.exec_(Regex.playlist_id, link)
    ->Belt.Option.flatMap( 
      Belt.Array.get(_, 0) 
      <.> Js.Re.captures
    )
}

let fetch_access_token = () => {
  open Fetch;

  fetchWithInit(
    "https://accounts.spotify.com/api/token?grant_type=client_credentials",
    RequestInit.make(
      ~method_ = Post,
      ~headers = HeadersInit.make({
        "Content-Type"  : "application/x-www-form-urlencoded",
        "Authorization" : make_auth -> Lazy.force
      }),
      ()
    )
  )
  >>= Response.json
  >>- token_res_decode
}

let fetch_playlist = (token, id) => {
  open Fetch;
  let token = token.access_token;
  Js.log(token)

  fetchWithInit(
    make_playlist_url(id),
    RequestInit.make(
      ~headers = HeadersInit.make({
        "Content-Type"  : "application/json",
        "Accept"        : "application/json",
        "Authorization" : {j|Bearer $token|j}
      }),
      ()
    )
  )
  >>= Response.json
  >>- playlist_res_decode
}
