..o : ../src/makefile 
poisson_1d_identity_test.o : ../src/poisson_1d_identity_test.F90 mod_poisson_1d_identity_test.o mod_defs.o 
mod_defs.o : ../src/mod_defs.F90 
mod_poisson_1d.o : ../src/mod_poisson_1d.F90 mod_defs.o 
mod_poisson_1d_solver.o : ../src/mod_poisson_1d_solver.F90 mod_poisson_1d_PETSc_MG.o mod_poisson_1d_MG.o mod_poisson_1d_GS.o mod_poisson_1d_Jacobi.o mod_poisson_1d.o 
mod_poisson_1d_Jacobi.o : ../src/mod_poisson_1d_Jacobi.F90 mod_poisson_1d.o mod_defs.o 
mod_poisson_1d_GS.o : ../src/mod_poisson_1d_GS.F90 mod_poisson_1d.o mod_defs.o 
mod_poisson_1d_MG.o : ../src/mod_poisson_1d_MG.F90 mod_poisson_1d_GS.o mod_poisson_1d.o mod_defs.o 
mod_poisson_1d_identity_test.o : ../src/mod_poisson_1d_identity_test.F90 mod_poisson_1d_solver.o mod_poisson_1d.o mod_defs.o 
mod_poisson_1d_PETSc_MG.o : ../src/mod_poisson_1d_PETSc_MG.F90 mod_poisson_1d.o mod_defs.o 
