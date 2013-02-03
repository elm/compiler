all: mario_mp Mario.html Clicks.html Object.html Values.html

mario_mp: *.go
	go build

Mario.html: Mario.elm
	elm -r elm-runtime.js Mario.elm

Clicks.html: Clicks.elm
	elm -r elm-runtime.js Clicks.elm

Object.html: Object.elm
	elm -r elm-runtime.js Object.elm

Values.html: Values.elm
	elm -r elm-runtime.js Values.elm

clean:
	rm -rf *.html mario_mp
