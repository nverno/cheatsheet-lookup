emacs ?= emacs
wget ?= wget
python ?= python
ruby ?= ruby

datadir= ../data

ifeq ($(OS), Windows_NT)
  python="C:/Program Files/Anaconda/python.exe"
endif

json= cheatsheets_org.json
dat= $(json:.json=.dat)

.PHONY: clean
all: data

data: $(datadir)/$(dat)
$(datadir)/$(dat): $(json)
	$(emacs) -batch -l build.el -f batch-convert $<
	cp $(dat) $(datadir)

$(json):
	$(python) cheatsheets_org.py -o cheatsheets_org.json

clean:
	$(RM) $(json) $(dat) *~ *.pyc
