setting up language server
	pip install "python-lsp-server[all]"
	pip install pylsp-mypy
	apt/brew install ispell
setting up project-specific configuration (.dir-locals.el)
	...
keys
	M-up - select previous suggestion
	M-down - select next suggesion
	M-enter - apply current suggestion
    M-m - Moves the cursor to the true beginning of a line, in other words it moves the cursor to the indentation or to the first character ignoring the white space.
    M-<,M-> Jumps to the beginning and end of the buffer. Keep in mind that a mark is set before the jump takes place.
    C-v, M-v Moves the cursor a up a page or down a page, useful for faster vertical movement for file scanning.
    M-g M-g Moves the cursor to the specified line number. You can also prefix this command to avoid the mini buffer prompt. (We'll cover prefixing commands later)
    C-l Will cycle the cursors line between the center, top and bottom of the current window's view port.
    M-r Will cycle the cursor position between the center, top and bottom of the current windows view port.
	we can move the cursor to an outer block, using C-M-u (think of it as, up)
- editing
	M-k = kill sentence
	M-/ = dabbrev (very useful, especially if you’re allergic to company).
	C-M-u - go up in syntax tree structure (loop->function->class etc)
	M-? - find references
	C-x p f - project find file (M-up, M-down to iterate over, M-v to switch to the buffer)
	C-c s - select sting (between " ")
	C-c w - select word (mode-defined, c/c++/python add _ to consider a word)
	C-x v g - git blame
	C-x C-b - ibuffer
	S-f3 - start macro
	f3 - end macro or call macro
	f5 - flyspell-mode
	f12 - flymake mode on/off
	S-f12 - show flymake diagnostics buffer and switch to it
	M-f12 - close flymake diagnostics buffer
