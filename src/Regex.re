let playlist_id = [%re "/playlist\/(\w+)\??/"];
let dl_info = [%re "/(\d+)\s+([a-z\d]+)\s+\w+\s\w+\s+\w+\s+(\d+)/"];
let dl_is_info = [%re "/^\[info\]/i"]
let dl_is_yt = [%re "/^\[youtube\]/i"]
let dl_is_download_status = [%re "/^\[download\] \s+ \d+\.\d+%/"]

let dl_download_status = [%re "/(\d+\.\d+)%/"]

let normalize_dist = d => {
  let r = [%re "/(.+)\/?$/"]

  Js.Re.exec_(r, d)
  ->Belt.Option.getExn
  ->Js.Re.captures[1]
  ->Js.Nullable.toOption
  ->Belt.Option.getExn
}