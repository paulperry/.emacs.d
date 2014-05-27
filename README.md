# Emacs Configuration
Paul Perry
pop@paulperry.net

Tested on Emacs 24.3 (9.0)
On OS X 10.9.2

This configuration automatically installs several plugins using ELPA and Marmalade.
It is based on Andrea Crotti's [minimal emacs configuration](https://github.com/AndreaCrotti/minimal-emacs-configuration/) (check his [youtube](https://www.youtube.com/watch?v=0cZ7szFuz18) presentation), with parts of his full [emacs configuration](https://github.com/AndreaCrotti/Emacs-configuration) and parts of Jessica Hamrick's [emacs configuration](https://github.com/jhamrick/emacs/). It adds [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell). 

## Emacs plugins

<!--- * `auctex` -- LaTeX plugin -->
* `ein` -- [IPython notebook](http://ipython.org/notebook) plugin
* `jedi` -- general Python support
* `pydoc-info` -- Python documentation
* `auto-complete` -- auto completion
* `color-theme-solarized` -- the [solarized](http://ethanschoonover.com/solarized) color theme
* `magit` -- git plugin
* `markdown-mode` -- support for [Markdown](http://daringfireball.net/projects/markdown/) files
* `matlab-mode` -- support Matlab files

### pycheckers

`pycheckers` is a little script to check that your Python code
conforms to the
[PEP8 style guide](http://legacy.python.org/dev/peps/pep-0008/) using
the [pep8](https://pypi.python.org/pypi/pep8) and
[pyflakes](https://pypi.python.org/pypi/pyflakes/0.8.1) Python
packages.

If you do not want this functionality, you can comment out the block
of code in `python-settings.el` that starts with "pyflakes flymake
integration". Otherwise, read on.

1. In your `~/.bashrc`, add `$HOME/bin` to your `$PATH` environment variable like so:
  
  ```
  export PATH="$HOME/bin:$PATH"
  ```

2. Create a file in `~/bin/pycheckers` with the following contents:

  ```
  #!/bin/bash
  
  pyflakes "$1"
  pep8 --ignore=E261 --repeat "$1"
  true
  ```

3. Make it executable with `chmod +x ~/bin/pycheckers`.

4. Make sure you have `pep8` and `pyflakes` installed (run `pip
   install pep8 pyflakes`).

5. Now it should work! 


