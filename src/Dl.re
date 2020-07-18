open Utils;

type message =
  | DownloadDest(string)
  | DownloadStatus(int)
  | Youtube(string)
  | Info(string)
  | Error(string);

type info = {
  code: int,
  ext: string,
  size: int,
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
          let matches =
            m
            ->Js.Re.captures
            ->Belt.Array.keepMap(Js.Nullable.toOption)
            ->Belt.Array.sliceToEnd(1);

          switch (matches) {
          | [|code, ext, size|] =>
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
        c
        ->Js.String2.split("\n")
        ->Belt.Array.sliceToEnd(2)
        ->Belt.Array.keepMap(process_line)
        ->foldl1_arr((a, b) =>
            if (a.size > b.size) {
              a;
            } else {
              b;
            }
          )
        ->resolve;
      };
    });
  });

let download = (dist, track, info) =>
  Future.make(resolve => {
    open ChildReprocess;
    open StdStream;
    open Youtube;

    let dist = Regex.normalize_dist(dist);
    let title = track.title;
    let ext = info.ext;
    let code = info.code;
    let cp =
      spawn(
        "youtube-dl",
        [|"-f", {j|$code|j}, "-o", {j|$dist/$title.$ext|j}, track.url|],
        // ~options={"stdio": [|"", "pipe"|]},
        (),
      );
    let std_out = cp->child_stdout;

    std_out
    ->Readable.on_data(c => {
        let c = c->Node_buffer.toString;
        Js.Re.exec_(Regex.dl_download_status, c)
        ->Belt.Option.flatMap(m => Js.Re.captures(m)[1]->Js.Nullable.toOption)
        ->Js.log;
        ();
      })
    ->Readable.on_end(resolve);
  });
