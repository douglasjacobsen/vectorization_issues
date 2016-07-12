OBJS=main.F
EXE=run_tests.x
CPPFLAGS=

ifeq "$(CONTIG)" "true"
	override CPPFLAGS += -D_CONTIG
endif

ifeq "$(CONCUR)" "true"
	override CPPFLAGS += -D_CONCUR
endif

gfortran:
	gfortran $(CPPFLAGS) -o $(EXE) -O3 -ffree-form -ffree-line-length-none $(OBJS)

ifort:
	ifort $(CPPFLAGS) -o $(EXE) -O3 -FR $(OBJS)

clean:
	rm *.o *.mod $(EXE)
