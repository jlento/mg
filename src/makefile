FC           = gfortran
MAKEDEPF90   = /usr/bin/makedepf90

FCFLAGS = -g -pg -march=native -Wall -fcheck=all
SRC_test_1d = test_1d.f90 mod_gnuplot.f90 mod_defs.f90 \
              mod_poisson_1d.f90 mod_poisson_1d_GS.f90 \
              mod_poisson_1d_Jacobi.f90 \
              mod_poisson_1d_MG.f90
OBJ_test_1d = $(SRC_test_1d:.f90=.o)

vpath %.f90 : ../src

.PHONY : clean

test_1d : $(OBJ_test_1d)
	$(FC) $(FCFLAGS) -o $@ $^

%.o : %.f90
	$(FC) $(FCFLAGS) -c $<

clean :
	rm -f *.o *.mod test_1d

deps.mk : $(SRC_test_1d)
	[ -x $(MAKEDEPF90) ] && $(MAKEDEPF90) -b '' $^ > $@

include deps.mk
