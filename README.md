# Spotty
A Spotify playlist downloader

# How and why
Given a playlist link, it fetches the list of songs, searches ofr them on Youtube (using [puppeteer](https://github.com/zploskey/bs-puppeteer)). After that, they are downloaded using [youtube-dl](https://github.com/ytdl-org/youtube-dl) (for the time being, you need to install it manually). 
You also need to get your own spotify api credentials (*CLIENT_SECRET*, *CLIENT_ID*).

As to why: I simply got bored of doing it myself :shrug:

# Usage
1. Clone the repo, and compile it (`npm i; npm run build`)
2. Download youtube-dl ([here's how](https://ytdl-org.github.io/youtube-dl/download.html)), and make sure it's in your PATH
3. Get your Spotify api credentials, and, preferably, put them inside of a `.env` file; like that:
  ```
  CLIENT_SECRET=<secret, very secret indeed>
  CLIENT_ID=<just like in a liquor store>
  ```
4. Get the url of a playlist you'd like to download (i.e. https://open.spotify.com/playlist/37i9dQZF1DWVzZlRWgqAGH)
5. Run `node src/Main.bs.js <playlist_link> <output_dir>`