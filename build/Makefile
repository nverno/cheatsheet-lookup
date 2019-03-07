EMACS ?= emacs
PYTHON ?= python
RUBY ?= ruby

DATADIR = ../data

ifeq ($(OS), Windows_NT)
  PYTHON = "C:/Program Files/Anaconda/python.exe"
endif

JSON = cheatsheets_org.json awesome.json
DAT  = $(JSON:.json=.dat)

.PHONY: clean
all: data

data: $(DATADIR)/$(DAT)
%.dat: %.json
	$(EMACS) -Q --batch -l build.el -f batch-convert $<
	cp $@ $(DATADIR)

cheatsheets_org.json:
	$(PYTHON) cheatsheets_org.py -o cheatsheets_org.json

awesome.json:
	$(RUBY) awesome.rb

clean:
	$(RM) $(json) $(dat) *~ *.pyc
