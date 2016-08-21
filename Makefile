emacs ?= emacs
wget ?= wget
python ?= python

ifeq ($(OS), Windows_NT)
  python="C:/Program Files/Anaconda/python.exe"
endif

elpa_dir ?= ~/.emacs.d/elpa
auto ?= cheatsheet-lookup-autoloads.el

el = $(filter-out $(auto) .dir-locals.el,$(wildcard *.el))
elc = $(el:.el=.elc)

batch_flags = -batch \
	--eval "(let ((default-directory                   \
                      (expand-file-name \"$(elpa_dir)\"))) \
                  (normal-top-level-add-subdirs-to-load-path))"

auto_flags ?= \
	--eval "(let ((generated-autoload-file                       \
                      (expand-file-name (unmsys--file-name \"$@\"))) \
                      (wd (expand-file-name default-directory))      \
                      (backup-inhibited t)                           \
                      (default-directory                             \
	                (expand-file-name \"$(elpa_dir)\")))         \
                   (normal-top-level-add-subdirs-to-load-path)       \
                   (update-directory-autoloads wd))"

json= build/cheatsheet-lookup.json

.PHONY: $(auto) clean distclean
all: compile $(auto)

compile : $(elc)
%.elc : %.el
	$(emacs) $(batch_flags) -f batch-byte-compile $<

$(auto):
	$(emacs) -batch $(auto_flags)

README.md: el2markdown.el cheatsheet-lookup.el
	$(emacs) -batch -l $< cheatsheet-lookup.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"


scrape: $(json)
	cd build && $(emacs) -batch -l build.el -f build-el

$(json):
	cd build; \
	$(python) cheatsheet-lookup.py

TAGS: $(el)
	$(RM) $@
	touch $@
	ls $(el) | xargs etags -a -o $@

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
