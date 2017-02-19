default: make

install:
	bundle install

make:
	elm make src/Hangman.elm --output assets/js/hangman.js

serve:
	bundle exec jekyll serve --host 0.0.0.0
