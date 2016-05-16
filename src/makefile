FC           = gfortran
MAKEDEPF90   = /usr/bin/makedepf90

FCFLAGS = -g -pg -march=native -Wall -fcheck=all

PROG = poisson_identity_test_1d

SRC_$(PROG) = \
    $(PROG).f90  mod_defs.f90 \
    mod_poisson_1d.f90            mod_poisson_solver_1d.f90 \
    mod_poisson_1d_Jacobi.f90     mod_poisson_1d_GS.f90 \
    mod_poisson_1d_MG.f90 \
    mod_$(PROG).f90

OBJ_$(PROG) = $(SRC_$(PROG):.f90=.o)

vpath %.f90 : ../src

.PHONY : clean

$(PROG) : $(OBJ_$(PROG))
	$(FC) $(FCFLAGS) -o $@ $^

%.o : %.f90
	$(FC) $(FCFLAGS) -c $<

clean :
	rm -f *.o *.mod test_1d

deps.mk : $(SRC_$(PROG))
	[ -x $(MAKEDEPF90) ] && $(MAKEDEPF90) -b '' $^ > $@

include deps.mk

# Bug in GCC 5.3.1 Ubuntu
ifeq ($(shell $(FC) -dumpversion),5.3.1)
  mod_poisson_1d.o : FCFLAGS = -g -pg -march=native -Wall
endif

