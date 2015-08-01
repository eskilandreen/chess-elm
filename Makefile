TARGETS=Element.html Collage.html VirtualDom.html

all: $(TARGETS)


%.html: %/Main.elm
	elm-make $< --output $@


clean:
	rm $(TARGETS)
