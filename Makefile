.PHONY: install emacs clean

# Install dependencies
install:
	cask install

# Run Emacs with only project deps (isolated)
emacs:
	cask exec emacs \
		--no-init-file \
		--load ncol-mode.el \
		--eval '(ncol-mode +1)'

clean:
	rm -rf .cask
