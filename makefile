SUBDIRS = analysis

RSYNC=/usr/bin/rsync

.PHONY: $(SUBDIRS)

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) --directory=$@
