module mod_poisson_1d_PETSc_MG
  use iso_fortran_env, only : eu => error_unit
  use mod_defs
  use mod_poisson_1d
  implicit none
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscpc.h>
#include <petsc/finclude/petscksp.h>
#include <petsc/finclude/petscdm.h>
#include <petsc/finclude/petscdmda.h>
#include <petsc/finclude/petscvec.h90>

  ! Sequential, single process version

  private
  public   :: poisson_PETSc_MG_1d
  external :: ComputeRHS, ComputeMatrix
    
contains

  subroutine poisson_PETSc_MG_1d ( system )
    type ( poisson_1d ), target, intent ( inout ) :: system
    
    PetscErrorCode   ierr
    DM               da
    KSP              ksp
    PetscInt         n, dof, s
    Vec              u
    PetscScalar, pointer :: up ( : )
    
    n   = size ( system % f )
    s   = 1
    dof = 1
    call KSPCreate ( MPI_COMM_SELF, ksp, ierr )
    call DMDACreate1d ( PETSC_COMM_SELF, DM_BOUNDARY_NONE, &
         n, dof, s, PETSC_NULL_INTEGER, da, ierr )
    call KSPSetComputeRHS ( ksp, ComputeRHS, system, ierr )
    call KSPSetComputeOperators( ksp, ComputeMatrix, system, ierr )
    call KSPSetDM ( ksp, da, ierr )
    call KSPSetFromOptions ( ksp, ierr )
    call KSPSolve ( ksp, PETSC_NULL_OBJECT, PETSC_NULL_OBJECT, ierr )
    call KSPGetSolution ( ksp, u, ierr )
    call VecGetArrayReadF90 ( u, up, ierr )
    system % u = real ( up, fp )
    call VecRestoreArrayReadF90 ( u, up, ierr )
    call KSPDestroy( ksp, ierr )
    call DMDestroy( da, ierr )

  end subroutine poisson_PETSc_MG_1d

end module mod_poisson_1d_PETSc_MG


subroutine ComputeRHS ( ksp, b, ctx, ierr )
  use mod_poisson_1d
  implicit none
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
  KSP          ksp
  Vec          b
  type ( poisson_1d ) :: ctx
  PetscErrorCode  ierr
  integer :: i, n
  PetscScalar c
  n = size ( ctx % f )
  call VecSetValues ( b, n, [ ( i , i = 0, n - 1 ) ], &
       real ( ctx % f, kind ( c ) ), INSERT_VALUES, ierr )
end subroutine ComputeRHS


subroutine ComputeMatrix(ksp,JJ,jac,ctx,ierr)
  use mod_poisson_1d
  implicit none
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscdm.h>
#include <petsc/finclude/petscdmda.h>
  Mat               jac,JJ
  PetscErrorCode    ierr
  KSP               ksp
  DM                da
  PetscInt          i, mx
  PetscInt          xs, xm,  i1, i7
  PetscScalar       v(3)
  MatStencil   row(4,1), col(4,3)
  type ( poisson_1d ) :: ctx
  i1 = 1
  i7 = 3
  call KSPGetDM ( ksp, da, ierr )
  call DMDAGetInfo ( da, PETSC_NULL_INTEGER, mx, &
       PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, &
       PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, &
       PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, &
       PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, &
       PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, &
       PETSC_NULL_INTEGER, ierr )
  call DMDAGetCorners( da, &
       xs, PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, &
       xm, PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, ierr)
  do i = xs, xs+xm-1
     row(MatStencil_i,1) = i
     if ( i.eq.0 .or. i.eq.mx-1 ) then
        v(1) = 1.0
        call MatSetValuesStencil(jac,i1,row,i1,row,v,INSERT_VALUES,ierr)
     else
        v(1) = -1.0
        v(2) =  2.0
        v(3) = -1.0
        col(MatStencil_i,1) = i-1
        col(MatStencil_i,2) = i
        col(MatStencil_i,3) = i+1
        call MatSetValuesStencil(jac,i1,row,i7,col,v,INSERT_VALUES,ierr)
     endif
  end do
  call MatAssemblyBegin(jac,MAT_FINAL_ASSEMBLY,ierr)
  call MatAssemblyEnd(jac,MAT_FINAL_ASSEMBLY,ierr)
end subroutine ComputeMatrix
