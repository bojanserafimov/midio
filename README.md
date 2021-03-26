# midio
Tools for midi generation and playback from within emacs. This project is in beta and everything is subject to change. I built this in my spare time, and I'm not even sure why. Development is not active.

## Quick start

1. This library runs `fluidsynth` as an emacs subprocess by default so make sure you have `fluidsynth` installed. Run  `fluidsynth -a portaudio` to see if it's working. If you want to configure your `fluidsynth` server differently or run it outside of emacs, there are configuration options for that available.
2. Get a soundfont file compatible with your fluidsynth install. You will be prompted to choose a soundfont before any sound plays.
3. Download the `midio` source
4. Add the source to your emacs load path and require the main package
``` elisp
(add-load-path! "relative/path/to/wherever/you/put/the/source")
(require 'midio)
```
You can either put this code in your main config file or put it anywhere and execute it (tip: You can execute any expression by placing the cursor to the closing paren and hitting `C-x C-e`)

5. Open `tests/the-lick.el` and execute the expression. Sound will play and a hydra control panel will show up so you can interact with the player.

## Contributing
Bug reports (via github issue) and bug fixes (via pull request) are welcome. Feature requests (via github issue) are welcome, though I can't promise anything. Feature contributions will likely be rejected until I get a better idea of what I'm doing with this. Don't email me or any other contributor without explicit permission.
