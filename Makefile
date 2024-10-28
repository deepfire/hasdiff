CMD := hasdiff --cdOrigin /etc/group --cdModified /etc/group-
RTS := +RTS -xc -RTS

all: cls
	cabal run -- ${CMD}
prof: cls
	cabal run -- ${CMD} ${RTS}
clean:
	cabal clean
cls:
	echo -en "\ec"

shell: cls
	nix develop
prof-shell: cls
	nix develop .#profiling

