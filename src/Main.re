[%raw "require('isomorphic-fetch')"];
open ChildReprocess.Util;
open ChildReprocess.StdStream;
open Utils;
Dotenv.config();

/**
 * Notes:
 * 1. clean up the `genExn` shit-storm
 * 2. clean up dead code
 * 3. make clear stage divisions
 * 4. fix `undefine-s` in log while downloading
 * 5. make a better, cleaner, downloading interface (title (and/or) dist, percent, ?speed? ; clearing console (one-line output))
 */
let main = () => {
  Spotify.fetch_access_token()
  >>- Belt.Result.getExn
  >>= Spotify.fetch_playlist(_, "2qsfiBNlB3HYOsOmXTk1aS")
  >>= Youtube.get_playlist
  >>- (
    f =>
      f->Future.get(ts => {
        ts
        ->sync_future_consume(t =>
            Dl.get_format("https://www.youtube.com/watch?v=MeIv1AaxCa0")
            ->Future.flatMap(Dl.download("output/", t))
          )
        ->ignore
      })
  );
};

main();
