all: setup build

setup: client-setup server-setup

build: server-build client-build

client-setup:
	(cd elm ; elm package install -y)

client-build:
	(cd elm ; make ; make main.html)

server-setup:
	stack setup

server-build:
	(touch elm/main.html ; stack build ; rm elm/main.html)

run: server-build
	stack exec exe
