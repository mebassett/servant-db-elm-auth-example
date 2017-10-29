all: src/Generated/RestApi.elm main.html

src/Generated/RestApi.elm: ../elm-code-generator/Main.hs ../src/Api.hs
	(cd .. ; stack exec code-gen)

main.html: src/Generated/RestApi.elm src/Main.elm
	elm make src/Main.elm --output=main.html
