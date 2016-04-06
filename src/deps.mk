test_1d.o : test_1d.f90 mod_gnuplot.o mod_poisson_1d_MG.o mod_poisson_1d_GS.o mod_poisson_1d_Jacobi.o mod_poisson_1d.o mod_defs.o 
mod_gnuplot.o : mod_gnuplot.f90 mod_defs.o 
mod_defs.o : mod_defs.f90 
mod_poisson_1d.o : mod_poisson_1d.f90 mod_defs.o 
mod_poisson_1d_GS.o : mod_poisson_1d_GS.f90 mod_poisson_1d.o mod_defs.o 
mod_poisson_1d_Jacobi.o : mod_poisson_1d_Jacobi.f90 mod_poisson_1d.o mod_defs.o 
mod_poisson_1d_MG.o : mod_poisson_1d_MG.f90 mod_gnuplot.o mod_poisson_1d_GS.o mod_poisson_1d.o mod_defs.o 
