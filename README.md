# midio
Tools for midi generation and playback from within emacs.

## Quick start

1. This library runs `fluidsynth` as an emacs subprocess by default so make sure you have `fluidsynth` installed. Run  `fluidsynth -a portaudio` to see if it's working. If you want to configure your `fluidsynth` server differently or run it outside of emacs, there are configuration options for that available.
2. Download the `midio` source
3. Add the source to your emacs load path and require the main package
``` elisp
(add-load-path! "relative/path/to/wherever/you/put/the/source")
(require 'midio)
```
You can either put this code in your main config file or put it anywhere and execute it (tip: You can execute any expression by placing the cursor to the closing paren and hitting `C-x C-e`)
4. Open `tests/the-lick.el` and execute the expression. Sound will play and a hydra control panel will show up so you can interact with the player.
