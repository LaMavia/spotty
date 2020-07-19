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

    let title          = tr.name;
    let artist         = tr.album.artists[0].name;
      
    let title          = {j|$title $artist|j};
    let duration       = tr.duration_ms;

    let time_selector  = "[aria-label].ytd-thumbnail-overlay-time-status-renderer";
    let title_selector = "a#video-title";

    let get_best_track = ((acc_score, acc_t), t) => {
      let dt    = abs(duration - t.duration)->float_of_int;
      let t_cos =
        (acc_t.title, t.title)
        ->vectors_of_words
        ->cos
        ->Belt.Option.getWithDefault(0.0);

      let score = t_cos /. dt;

      if (score > acc_score) {
        (score, t);
      } else {
        (acc_score, acc_t);
      };
    };

    let extract_video_info = (body, title_selector, time_selector) => {
      open Js.Array2;
      open Dom;

      // Using javascript bindings due to the lack of 
      // reason-specific packages (Array, Belt etc.) in browser context
      let time_of_string = str =>
        str
        ->Js.String2.split(":")
        ->map([%raw "x => Number(x)"])
        ->reduce((a, n) => a * 60 + n, 0);

      let title_link =
        Element.querySelectorAll(title_selector, body)
        ->NodeList.toArray
        ->map(e =>
            (
              Node.textContent(e)->Js.String.trim,
              e->[%raw "e => e.href.trim()"],
            )
          );

      let duration =
        Element.querySelectorAll(time_selector, body)
        ->NodeList.toArray
        ->map(node => node->Node.textContent->time_of_string);

      (title_link, duration);
    };

    let promise_tracks =
      p->Page.goto(
        {j|https://www.youtube.com/results?sp=EgIQAQ%253D%253D&search_query=$title|j},
        ~options=Navigation.makeOptions(~waitUntil=`networkidle0, ()),
        (),
      )
      >>= (_ => Page.waitForSelector(p, time_selector, ()))
      >>= (_ => Page.selectOneEval2(
             p,
             "body",
             extract_video_info,
             title_selector,
             time_selector,
           )
          )
      >>- uncurry(Belt.Array.zip)
      >>- Belt.Array.map(_, (((title, url), duration)) =>
            { title
            , url
            , duration: duration * 1000
            }
          );

    FutureJs.fromPromise(promise_tracks, err => {
      Js.Console.error({j|[$title | error]> $err|j})
    })
    ||= Belt.Result.getWithDefault(_, [||])
    ||= Belt.List.fromArray
    ||= foldl1h(_, 0.0, get_best_track)
    ||= (
      a =>
        switch (a) {
        | Some((_c_t, t)) =>
          let url = t.url;
          Js.log({j|[Found] $title ($url)|j})
          
          Some({...t, title});

        | None => None
        }
    );
  };

let get_playlist = (res: Spotify.playlist_res) => {
  open BsPuppeteer;
  open Belt;

  Puppeteer.launch()
  >>= Browser.newPage
  >>- (
    p => {
      res.items
      ->List.fromArray
      ->List.map(item => item.track)
      ->Utils.sync_future_map(get_track(p))
      ->Future.map(tracks => {
          BsPuppeteer.Browser.close(p->Page.browser)->ignore;
          tracks->Belt.List.keepMap(const);
        });
    }
  )
};
