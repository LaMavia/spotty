open Utils;

type track = {
  title: string,
  url: string,
  duration: int,
};

let get_track: (BsPuppeteer.Page.t, Spotify.track) => Future.t(option(track)) =
  (p, tr) => {
    open BsPuppeteer;
    open Webapi;

    let title = tr.name;
    let artist = tr.album.artists[0].name;

    let title = {j|$title $artist|j};
    let duration = tr.duration_ms;

    let time_selector = "[aria-label].ytd-thumbnail-overlay-time-status-renderer";
    let title_selector = "a#video-title";

    let get_best_track = ((acc_cos, acc_t), t) => {
      let dt = abs(duration - t.duration)->float_of_int;
      let t_cos =
        (acc_t.title, t.title)
        ->vectors_of_words
        ->cos
        ->Belt.Option.getWithDefault(0.0);

      let t_cos = t_cos /. dt;

      if (t_cos > acc_cos) {
        (t_cos, t);
      } else {
        (acc_cos, acc_t);
      };
    };

    let extract_video_info = (body, title_selector, time_selector) => {
      // Using javascript bindings due to the lack of reason-specific packages (Array, Belt etc.) in browser context
      let time_of_string = str =>
        str
        ->Js.String2.split(":")
        ->Js.Array.map([%raw "x => Number(x)"], _)
        ->Js.Array.reduce((a, n) => a * 60 + n, 0, _);

      let title_link =
        Dom.Element.querySelectorAll(title_selector, body)
        ->Dom.NodeList.toArray
        ->Js.Array.map(
            e =>
              (
                Dom.Node.textContent(e)->Js.String.trim,
                e->[%raw "e => e.href.trim()"],
              ),
            _,
          );

      let duration =
        Dom.Element.querySelectorAll(time_selector, body)
        ->Dom.NodeList.toArray
        ->Js.Array.map(a => a->Dom.Node.textContent->time_of_string, _);

      (title_link, duration);
    };

    let pr =
      p->Page.goto(
        {j|https://www.youtube.com/results?sp=EgIQAQ%253D%253D&search_query=$title|j},
        ~options=Navigation.makeOptions(~waitUntil=`networkidle0, ()),
        (),
      )
      >>= (p |? Page.waitForSelector(_, time_selector, ()))
      >>= (
        p
        |? Page.selectOneEval2(
             _,
             "body",
             extract_video_info,
             title_selector,
             time_selector,
           )
      )
      >>- uncurry(Belt.Array.zip)
      >>- Belt.Array.map(_, (((title, url), duration)) =>
            {title, url, duration: duration * 1000}
          );

    pr->FutureJs.fromPromise(err => {
      Js.Console.error({j|[$title | error]> $err|j})
    })
    ||= Belt.Result.getWithDefault(_, [||])
    ||= Belt.List.fromArray
    ||= foldl1h(_, 0.0, get_best_track)
    ||= (
      a =>
        switch (a) {
        | Some((_c_t, t)) =>
          let t = {...t, title};
          Js.log(t);
          Some(t);
        | None => None
        }
    );
  };

let get_playlist = (res: Spotify.playlist_res) => {
  open BsPuppeteer;
  open Belt;
  let p =
    Puppeteer.launch()
    >>= Browser.newPage
    >>- (
      p => {
        res.items
        ->List.fromArray
        ->List.map(it => it.track)
        ->Utils.sync_future_map(get_track(p))
        ->Future.map(a => {
            BsPuppeteer.Browser.close(p->Page.browser)->ignore;
            Js.log("we done, baby!");
            a->Belt.List.keepMap(a => a);
          });
      }
    );
  p
  // p
  // ->FutureJs.fromPromise(Js.Console.error)
  // ->Future.flatMap(Belt.Result.getExn);
};
