
# Table of Contents

1.  [Muse](#org89caa33)
    1.  [Requirements](#orgd5f6333)
    2.  [Installation](#orgfce1ba4)
    3.  [Notes](#orgffb44e0)
    4.  [Todo](#orgcd8f8d8)



<a id="org89caa33"></a>

# Muse

Search for videos on Youtube, view them, and download their audio tracks.

This package contains an Emacs major mode \`muse-mode\`, and Rust cli.


<a id="orgd5f6333"></a>

## Requirements

-   \`geckodriver\`
-   \`ascii-image-converter\`


<a id="orgfce1ba4"></a>

## Installation

-   Install \`geckodriver\`: \`sudo apt install geckodriver\`
-   Install \`ascii-image-converter\`: \`sudo apt install ascii-image-converter\`
-   Clone this repo and \`cargo install &#x2013;path .\`
-   In Emacs, \`M-x load-file\` and then load \`muse.el\` in this repo
-   \`muse-search\` and add the query, then press \`d\` on a video listed in the resulting buffer to download the audio track, \`v\` to view the video and its related videos in another buffer, \`o\` to open the video link in the system browser.


<a id="orgffb44e0"></a>

## Notes

The \`muse\` cli does not manage the \`geckodriver server\`, that is responsibility of \`muse-mode\`. If you want to use the cli independently, start a geckodriver server with \`geckodriver &#x2013;port=4444\`.


<a id="orgcd8f8d8"></a>

## Todo

-   [ ] Refactor \`Muse\` to hold a \`Option<WebDriver>\` so that the cli help can be printed
-   [ ] Add \`Muse\` implementations for other sources (Spotify, etc)
-   [ ] Stream video to terminal as ascii art
-   [ ] Add a more sophisticated \`transient\` menu to control the download options
-   [ ] Add a command to go to the channel for a video, bound to \`c\`
-   [ ] Add tests

