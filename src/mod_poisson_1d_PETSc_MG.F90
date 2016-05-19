module mod_poisson_1d_PETSc_MG
  use iso_fortran_env, only : eu => error_unit
  use mod_defs
  use mod_poisson_1d
  implicit none

contains

  subroutine poisson_PETSc_MG_1d ( x )
    type ( poisson_1d ), intent ( inout ) :: x
#ifndef HAVE_PETSC
    write ( eu, '(a)' ) 'Compile with PETSC_DIR=...'
    error stop
#endif
  end subroutine poisson_PETSc_MG_1d

end module mod_poisson_1d_PETSc_MG
