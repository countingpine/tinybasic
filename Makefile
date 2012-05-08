AR=ar
ARFLAGS=rcs
CPP=g++
CPPFLAGS=-O2 -g
LD=g++
LDFLAGS=
RM=rm

OUTFILE0=tbc
OUTFILE1=tbc1
OUTFILE2=tbc2
OUTFILE3=tbc3
OUTFILE4=tbc4

SRC=compiler/src/tinybasic.bas
INC=$(wildcard compiler/src/inc/*.bi)
ALLSRC=$(INC) $(wildcard compiler/src/*.bas)

RTLIBSRC=$(wildcard rtlib/src/*.cpp)
RTLIBINC=$(wildcard rtlib/inc/*.h)
RTLIBOBJ=$(patsubst rtlib/src/%.cpp,o/%.o,$(RTLIBSRC))

all: rtlib $(OUTFILE4)

################################################################################
rtlib: $(RTLIBOBJ)

o/%.o: rtlib/src/%.cpp $(RTLIBINC)
	$(CPP) $(CPPFLAGS) -c $< -o $@

################################################################################
$(OUTFILE0): $(ALLSRC) rtlib
	fbc -g -x $@ $(SRC)

$(OUTFILE1): $(ALLSRC) $(OUTFILE0) rtlib
	./$(OUTFILE0) -O -g -o $@ $(SRC)

$(OUTFILE2): $(ALLSRC) $(OUTFILE1) rtlib
	./$(OUTFILE1) -O -g -o $@ $(SRC)

$(OUTFILE3): $(ALLSRC) $(OUTFILE2) rtlib
	./$(OUTFILE2) -O -g -o $@ $(SRC)

$(OUTFILE4): $(ALLSRC) $(OUTFILE3) rtlib
	./$(OUTFILE3) -O -g -o $@ $(SRC)

clean:
	@$(RM) -f $(OUTFILE0) $(OUTFILE1) $(OUTFILE2) $(OUTFILE3) $(OUTFILE4)
	@$(RM) -f $(RTLIBOBJ)  _____temp.c
