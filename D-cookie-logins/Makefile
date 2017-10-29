all: setup build

setup: client-setup server-setup

build: client-build server-build

client-setup:
	(cd elm ; elm package install -y)

client-build:
	(touch elm/main.html ; stack build ; cd elm ; rm main.html ; make ; make main.html)

server-setup:
	stack setup

server-build:
	stack build

run: server-build
	stack exec exe
