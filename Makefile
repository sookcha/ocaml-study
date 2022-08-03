SUBDIRS = $(subst /, ,$(shell ls -d */))

all:
	@for dir in $(SUBDIRS) ; do \
		make -C  $$dir ; \
	done
	@echo "Done!"

submit:
	@for dir in $(SUBDIRS) ; do \
		echo "[ $${dir//-/_}.ml ] submit"; \
		echo "\t$x\c"; exercism submit $$dir/$${dir//-/_}.ml | true; \
	done
	@echo "Done!"

download:
	exercism download --exercise=$(problem) --track=ocaml && mv ocaml/$(problem) ./
