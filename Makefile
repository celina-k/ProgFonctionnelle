.PHONY: all install clean
REFRESH_RATE = 0.01
LEVEL = 00015-hell-and-suffering
AUTHOR = AHMED
MESSAGE = Un petit peu avancé

all:
	eval $$(opam env)
	test -d _opam || opam switch create .
	dune build @install
	ln -sf _build/default/src/lambdaDriver.exe lambda
	./lambda server -s $(REFRESH_RATE) -v -w tests/$(LEVEL).json './lambda man -n 1 -v'

install:
	dune install

become_ahmed:
	git config --global user.name "Ahmed"
	git config --global user.email "ahmed.mimouni@etu.univ-paris-diderot.fr"
	git config --global ssh.identity /home/netbook/.ssh/id_rsa

become_celina:
	git config --global user.name "KHALFAT celina"
	git config --global user.email "khalfat.celina@gmail.com"
	git config --global ssh.identity /home/netbook/.ssh/celina/id_rsa



clean:
	dune clean

run :
	./lambda server -s $(REFRESH_RATE) -v -w tests/$(LEVEL).json './lambda man -n 1 -v'

commit_and_push :
ifeq ($(AUTHOR), AHMED)
	git config --global user.name "Ahmed"
	git config --global user.email "ahmed.mimouni@etu.univ-paris-diderot.fr"
	git config --global ssh.identity /home/netbook/.ssh/id_rsa
	git commit src/decision.ml -m "$(MESSAGE)"
	git push
else
	git config --global user.name "KHALFAT celina"
	git config --global user.email "khalfat.celina@gmail.com"
	git config --global ssh.identity /home/netbook/.ssh/celina/id_rsa
	git commit src/decision.ml -m "$(MESSAGE)"
	git push
	git config --global ssh.identity /home/netbook/.ssh/id_rsa
	git config --global user.name "Ahmed"
	git config --global user.email "ahmed.mimouni@etu.univ-paris-diderot.fr"
	git config --global ssh.identity /home/netbook/.ssh/id_rsa
endif
