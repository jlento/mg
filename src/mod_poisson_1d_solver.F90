module mod_poisson_1d_solver
  use iso_fortran_env, only : eu => error_unit
  use mod_poisson_1d
  use mod_poisson_1d_Jacobi
  use mod_poisson_1d_GS
  use mod_poisson_1d_MG
  use mod_poisson_1d_PETSc_MG
  implicit none

#ifndef HAVE_PETSC
  integer, parameter :: nsolvers = 3
#else
  integer, parameter :: nsolvers = 4
#endif

  integer, parameter ::  JACOBI = 1, GAUSS_SEIDEL = 2, MULTIGRID = 3, &
       PETSC_MG = 4
  character ( 12 ), parameter :: method ( 4 ) = [ &
       'Jacobi      ', &
       'Gauss-Seidel', &
       'Multigrid   ', &
       'PETSC MG    ' ]
  
contains

  subroutine solve ( system, method )
    class ( poisson_1d ) , intent ( inout ) :: system
    integer, intent ( in ) :: method
    real ( fp ) :: e, e_prev, e_plot
    e_prev = huge ( e_prev )
    e_plot = huge ( e_prev )
    do
       select case ( method )
       case ( JACOBI )
          call poisson_Jacobi_iteration_1d ( system )
       case ( GAUSS_SEIDEL )
          call poisson_GS_iteration_1d ( system )
       case ( MULTIGRID )
          call poisson_MG_W_cycle_1d ( system )
       case ( PETSC_MG )
          call poisson_PETSc_MG_1d ( system )
       case default
          write ( eu, '(a, """",a,""".")' ) &
               'Unknown solver ', method
          error stop
       end select
       system % it = system % it + 1
       e = rms ( residual ( system ) )
       if (  abs ( e - e_prev ) .lt. epsilon ( e ) ) exit
       e_prev = e
    end do
  end subroutine solve
  
end module mod_poisson_1d_solver
