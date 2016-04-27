poisson_identity_test_1d.o : ../src/poisson_identity_test_1d.f90 mod_poisson_solver_1d.o mod_poisson_identity_test_1d.o mod_defs.o 
mod_defs.o : ../src/mod_defs.f90 
mod_poisson_1d.o : ../src/mod_poisson_1d.f90 mod_defs.o 
mod_poisson_solver_1d.o : ../src/mod_poisson_solver_1d.f90 mod_poisson_1d_MG.o mod_poisson_1d_GS.o mod_poisson_1d_Jacobi.o mod_poisson_1d.o 
mod_poisson_1d_Jacobi.o : ../src/mod_poisson_1d_Jacobi.f90 mod_poisson_1d.o mod_defs.o 
mod_poisson_1d_GS.o : ../src/mod_poisson_1d_GS.f90 mod_poisson_1d.o mod_defs.o 
mod_poisson_1d_MG.o : ../src/mod_poisson_1d_MG.f90 mod_poisson_1d_GS.o mod_poisson_1d.o mod_defs.o 
mod_poisson_identity_test_1d.o : ../src/mod_poisson_identity_test_1d.f90 mod_poisson_1d.o mod_defs.o 
