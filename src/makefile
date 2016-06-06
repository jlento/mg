MAKEDEPF90   = /usr/bin/makedepf90
PKG_CONFIG   = pkg-config
FCFLAGS      = -g -pg -march=native -Wall -fcheck=all

PETSC_VERSION = $(shell $(PKG_CONFIG) --silence-errors --modversion PETSc)
ifdef PETSC_VERSION
  FC = mpif90
  FCFLAGS += -DHAVE_PETSC $(shell $(PKG_CONFIG) --cflags PETSc)
  LDFLAGS += $(shell $(PKG_CONFIG) --libs PETSc)
  SRC_PETSC_SOLVER = mod_poisson_1d_PETSc_MG.F90
else
  FC = gfortran
endif

PROG = poisson_1d_identity_test

SRC_$(PROG) = \
    $(PROG).F90  mod_defs.F90 \
    mod_poisson_1d.F90            mod_poisson_1d_solver.F90 \
    mod_poisson_1d_Jacobi.F90     mod_poisson_1d_GS.F90 \
    mod_poisson_1d_MG.F90         $(SRC_PETSC_SOLVER) \
    mod_$(PROG).F90

OBJ_$(PROG) = $(addsuffix .o,$(basename $(SRC_$(PROG))))

vpath %.F90 : ../src

.PHONY : clean

$(PROG) : $(OBJ_$(PROG))
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o : %.F90
	$(FC) $(FCFLAGS) -c $<

clean :
	rm -f *.o *.mod deps.mk $(PROG)

deps.mk : $(lastword $(MAKEFILE_LIST)) $(SRC_$(PROG))
	[ -x $(MAKEDEPF90) ] && $(MAKEDEPF90) $(filter -D%,$(FCFLAGS)) -b '' $^ > $@

include deps.mk

# Bug in GCC 5.3.1 Ubuntu
ifeq ($(shell $(FC) -dumpversion),5.3.1)
  mod_poisson_1d.o : FCFLAGS = -g -pg -march=native -Wall
endif

