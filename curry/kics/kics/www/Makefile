# Translate programs for managing cgi server processes:

# Required:
# - installed curry2prolog system

COMPILE =../bin/kics -executable -q

.PHONY: all
all: submitform registry

submitform: ../src/lib/HtmlCgi.curry ../src/lib/CPNS.curry
	$(COMPILE) -o submitform HtmlCgi

registry: Registry.curry ../src/lib/HtmlCgi.curry
	$(COMPILE) -o registry Registry

.PHONY: cleanServers
cleanServers:
	registry clean 

.PHONY: show
show:
	registry show

.PHONY: sketch
sketch:
	registry sketch 

.PHONY: showAll
showAll:
	registry showAll 

.PHONY: stop
stop:
	registry stop 

.PHONY: kill
kill:
	registry kill 

.PHONY: clean
clean: 
	rm -f submitform
	../bin/cleancurry


