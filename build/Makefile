EMACS  ?= $(shell command -v emacs)
PYTHON ?= $(shell command -v python)
RUBY   ?= $(shell command -v ruby)

DATADIR =  ../data
JSON    := $(shell jq -r 'keys[]' sources.json)
DAT     =  $(JSON:%.json=%.dat)
DATA    =  $(addprefix $(DATADIR)/,$(DAT))

.PHONY: clean
all: install $(DATA)
	@

install:
	pip install -r requirements.txt

$(DATADIR)/%.dat: ./%.dat
	mv $^ $@

%.dat: %.json
	$(EMACS) -Q --batch -l build.el -f batch-convert $<

.INTERMEDIATE: $(JSON)
$(JSON):
	$(RUBY) sources.rb
# $(PYTHON) cheatsheets-org.py -o cheatsheets-org.json

# awesome.json:
# 	$(RUBY) awesome.rb

clean:
	$(RM) $(json) $(dat) *~ *.pyc *.dat $(JSON)
