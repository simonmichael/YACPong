# build with sdl wrapper for mac osx (see mainc.c or hssdl/Examples/MacOSX)
PROGNAME=YACPong
SRCNAME=YACPong.hs
BUILD=ghc -no-hs-main --make mainc.o MainWrapper.hs -DMAKE
$(PROGNAME): mainc.o MainWrapper.hs $(SRCNAME)
	$(BUILD) -o $@
$(PROGNAME)p: mainc.o MainWrapper.hs $(SRCNAME)
	$(BUILD) -o $@ -prof -auto-all
mainc.o: mainc.c MainWrapper_stub.h
	ghc -no-hs-main `sdl-config --cflags` -Wall $*.c -c
MainWrapper_stub.h: MainWrapper.hs
	ghc -no-hs-main --make $< -c
clean:
	rm -f *.hi *.o *_stub.c *_stub.h $(PROGNAME)
.PHONY: clean
#
