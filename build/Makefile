emacs ?= emacs
wget ?= wget
python ?= python
ruby ?= ruby

datadir= ../data

ifeq ($(OS), Windows_NT)
  python="C:/Program Files/Anaconda/python.exe"
endif

json= cheatsheets_org.json awesome.json
dat= $(json:.json=.dat)

.PHONY: clean
all: data

data: $(datadir)/$(dat)
%.dat: %.json
	$(emacs) -batch -l build.el -f batch-convert $<
	cp $@ $(datadir)

cheatsheets_org.json:
	$(python) cheatsheets_org.py -o cheatsheets_org.json

awesome.json:
	$(ruby) awesome.rb

clean:
	$(RM) $(json) $(dat) *~ *.pyc
