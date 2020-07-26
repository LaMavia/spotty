let playlist_id           = [%re "/playlist\/(\w+)\??/"];
let dl_info               = [%re "/(\d+)\s+(\w+)\s+(?:\w+\s+){2,4}\s+(\d+)/"];
let dl_is_info            = [%re "/^\[info\]/i"];
let dl_is_yt              = [%re "/^\[youtube\]/i"];
let dl_is_download_status = [%re "/^\[download\] \s+ \d+\.\d+%/"];
let dl_download_status    = [%re "/(\d+\.\d+)%/"];
let dl_is_audio_only      = [%re "/audio only/i"];
let yt_title_debris       = [%re "/(-|\\\w|\s{2,}|[\(\)\[\]])/ig"]
let yt_is_cover           = [%re "/\(cover\)/i"]

let normalize_dist = dist => {
  open Js.Re;
  open Belt.Option;

  exec_([%re "/(.+)\/?$/"], dist)
  ->flatMap(result => 
    captures(result)
    ->Array.get(1)
    ->Js.Nullable.toOption
  )
  ->getWithDefault(dist);
};
