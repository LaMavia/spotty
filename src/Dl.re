open Utils;

type info = 
  { code : int
  , ext  : string
  , size : int
  };

/**
 * Queries youtube-dl for available formats of the given url. Returns one with the greatest **size**
 */
let get_format = url =>
  Future.make(resolve => {
    open ChildReprocess;
    open StdStream;
    let cp = spawn("youtube-dl", [|"-F", url|], ());
    let std_out = cp->child_stdout;

    let process_line = line => {
      Js.Re.exec_(Regex.dl_info, line)
      ->Belt.Option.flatMap(m => {
          let is_audio_only = Regex.dl_is_audio_only->Js.Re.test_(line)
          let matches =
            m
            ->Js.Re.captures
            ->Belt.Array.keepMap(Js.Nullable.toOption)
            ->Belt.Array.sliceToEnd(1);

          switch (matches) {
          | [|code, ext, size|] when is_audio_only =>
            Some({
              code: int_of_string(code),
              ext,
              size: int_of_string(size),
            })
          | _ => None
          };
        });
    };

    std_out->Readable.on_data(c => {
      let c = c->Node_buffer.toString;

      if (Regex.dl_is_info->Js.Re.test_(c)) {
        let arr =
          c
          ->Js.String2.split("\n")
          ->Belt.Array.sliceToEnd(2)
          ->Belt.Array.keepMap(process_line);

        if (Array.length(arr) >= 1) {
          arr
          ->foldl1_arr((a, b) =>
              if (a.size > b.size) {
                a;
              } else {
                b;
              }
            )
          ->resolve;
        };
      };
    });
  });

let download = (dist: string, track: Youtube.track, info: info) =>
  Future.make(resolve => {
    open ChildReprocess;
    open StdStream;
    open Youtube;

    let dist    = Regex.normalize_dist(dist);
    let title   = track.title; 
    let ext     = info.ext;
    let code    = info.code;
    let cp      = spawn(
                    "youtube-dl",
                    [|"-f"
                    , {j|$code|j}
                    , "-o"
                    , {j|$dist/$title.$ext|j}
                    , track.url
                    |], 
                    ()
                  );

    child_stdout(cp)
    ->Readable.on_data(c => {
      open Belt.Option;
        let c = Node_buffer.toString(c)

        Js.Re.exec_(Regex.dl_download_status, c)
        ->flatMap(m =>
            Js.Re.captures(m)
            ->Array.get(1)
            ->Js.Nullable.toOption
          )
        ->map(status => {
            ReadLine.clear_line(ReadLine.std_out);
            ReadLine.cursor_to(ReadLine.std_out, 0);

            ReadLine.std_out->Readable.write(
              {j|[$title : $code, $ext]> $status%|j},
            );
          })
        ->ignore;
      })
    ->Readable.on_close(_ => resolve() );
  });
