
# Table of Contents

1.  [Muse](#org3db36c1)
    1.  [Requirements](#org9a38e09)
    2.  [Installation](#orgaae05bd)
    3.  [Notes](#org1a28eb8)
    4.  [Todo](#org411a5d6)



<a id="org3db36c1"></a>

# Muse

Search for videos on Youtube, view them, and download their audio tracks.

This package contains an Emacs major mode \`muse-mode\`, and Rust cli.

    ❯ muse help
    Muse 0
    someone
    Manage Youtube music
    
    USAGE:
        muse <SUBCOMMAND>
    
    OPTIONS:
        -h, --help       Print help information
        -V, --version    Print version information
    
    SUBCOMMANDS:
        download
        help        Print this message or the help of the given subcommand(s)
        search
        view


<a id="org9a38e09"></a>

## Requirements

-   \`geckodriver\`
-   \`ascii-image-converter\`


<a id="orgaae05bd"></a>

## Installation

-   Install \`geckodriver\`: \`sudo apt install geckodriver\`
-   Install \`ascii-image-converter\`: \`sudo apt install ascii-image-converter\`
-   Clone this repo and \`cargo install &#x2013;path .\`
-   In Emacs, \`M-x load-file\` and then load \`muse.el\` in this repo
-   \`M-x muse-search\` and add the query, then press \`d\` on a video listed in the resulting buffer to download the audio track, \`v\` to view the video and its related videos in another buffer, \`o\` to open the video link in the system browser.


<a id="org1a28eb8"></a>

## Notes

The \`muse\` cli does not manage the \`geckodriver server\`, that is responsibility of \`muse-mode\`. If you want to use the cli independently, start a geckodriver server with \`geckodriver &#x2013;port=4444\`.


<a id="org411a5d6"></a>

## Todo

-   [ ] Handle errors gracefully
-   [ ] Allow the cli help to be printed without a \`geckodriver\` server
-   [ ] Add &rsquo;preview&rsquo; under \`p\`, slicing with <https://unix.stackexchange.com/a/388148>
    -   [ ] Would be good to be able to see the video length
-   [ ] Stream video to terminal as ascii art under \`w\` (watch), on the view page
-   [ ] Add a command to go to the channel for a video, bound to \`c\`
-   [ ] Add a more sophisticated \`transient\` menu to control the download options
    -   [ ] Have a \`magit\` style \`transient\` menu on \`muse-mode\` buffers
-   [ ] Add \`Muse\` implementations for other sources (Spotify, etc)
-   [ ] Add tests

