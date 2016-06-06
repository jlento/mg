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
    
    PetscErrorCode       :: ierr
    DM                   :: da
    KSP                  :: ksp
    PetscInt             :: n, dof, s
    Vec                  :: u
    PetscScalar, pointer :: up ( : )
    
    n   = size ( system % f )
    s   = 1
    dof = 1
    
    call KSPCreate ( MPI_COMM_SELF, ksp, ierr )
    CHKERRQ(ierr)
    call DMDACreate1d ( PETSC_COMM_SELF, DM_BOUNDARY_NONE, &
         n, dof, s, PETSC_NULL_INTEGER, da, ierr )
    CHKERRQ(ierr)
    call KSPSetComputeRHS ( ksp, ComputeRHS, system, ierr )
    CHKERRQ(ierr)
    call KSPSetComputeOperators( ksp, ComputeMatrix, system, ierr )
    CHKERRQ(ierr)
    call KSPSetDM ( ksp, da, ierr )
    CHKERRQ(ierr)
    call KSPSetFromOptions ( ksp, ierr )
    CHKERRQ(ierr)
    call KSPSolve ( ksp, PETSC_NULL_OBJECT, PETSC_NULL_OBJECT, ierr )
    CHKERRQ(ierr)
    call KSPGetSolution ( ksp, u, ierr )
    CHKERRQ(ierr)
    call VecGetArrayReadF90 ( u, up, ierr )
    CHKERRQ(ierr)
    system % u = real ( up, fp )
    call VecRestoreArrayReadF90 ( u, up, ierr )
    CHKERRQ(ierr)
    call KSPDestroy( ksp, ierr )
    CHKERRQ(ierr)
    call DMDestroy( da, ierr )
    CHKERRQ(ierr)

  end subroutine poisson_PETSc_MG_1d

end module mod_poisson_1d_PETSc_MG


subroutine ComputeRHS ( ksp, b, ctx, ierr )
  use mod_poisson_1d
  implicit none
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
  KSP                 :: ksp
  Vec                 :: b
  type ( poisson_1d ) :: ctx
  PetscErrorCode      :: ierr
  PetscInt            :: i, n
  PetscScalar         :: c
  n = size ( ctx % f )
  call VecSetValues ( b, n, [ ( i , i = 0, n - 1 ) ], &
       real ( ctx % f, kind ( c ) ), INSERT_VALUES, ierr )
  CHKERRQ(ierr)
  if ( ctx % bc .eq. BC_DN ) then
     i = 1
     call VecSetValues ( b, i, [ int ( n - 1, kind ( i ) ) ], &
          real ( 2.0_fp * ctx % bc_bx, kind ( c ) ), &
          ADD_VALUES, ierr )
     CHKERRQ(ierr)
  end if
end subroutine ComputeRHS


subroutine ComputeMatrix(ksp,JJ,jac,ctx,ierr)
  use mod_poisson_1d
  implicit none
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscdm.h>
#include <petsc/finclude/petscdmda.h>
  KSP                 :: ksp
  Mat                 :: jac, JJ
  type ( poisson_1d ) :: ctx
  PetscErrorCode      :: ierr

  DM                  :: da
  PetscInt            :: i, mx, nullint
  PetscInt            :: xs, xm,  i1 = 1, i2 = 2, i3 = 3
  PetscScalar         :: v(3)
  MatStencil          :: row(4,1), col(4,3)

  nullint = PETSC_NULL_INTEGER
  
  call KSPGetDM ( ksp, da, ierr )
  CHKERRQ(ierr)
  call DMDAGetInfo ( da, nullint, mx, &
       nullint, nullint, nullint, nullint, nullint, nullint, &
       nullint, nullint, nullint, nullint, nullint, ierr )
  CHKERRQ(ierr)
  call DMDAGetCorners( da, xs, nullint, nullint, &
       xm, nullint, nullint, ierr)
  CHKERRQ(ierr)
  do i = xs, xs+xm-1
     row(MatStencil_i,1) = i
     if ( i.eq.0 .or. i.eq.mx-1 ) then
        v(1) = 1.0
        select case ( ctx % bc )
        case ( BC_DD, BC_DP )
           call MatSetValuesStencil( jac, i1, row, i1, row, v, &
                INSERT_VALUES,ierr)
           CHKERRQ(ierr)
        case ( BC_DN )
           if ( i .eq. 0 ) then
              call MatSetValuesStencil( jac, i1, row, i1, row, v, &
                   INSERT_VALUES,ierr)
              CHKERRQ(ierr)
           else
              v ( 1:2 ) = [ -2.0, 2.0 ]
              col( MatStencil_i, 1:2 ) = [ i - 1, i ]
              call MatSetValuesStencil( jac, i1, row, i2, col, v, &
                   INSERT_VALUES, ierr)
              CHKERRQ(ierr)
           end if
        end select
     else
        v = [ -1.0, 2.0, -1.0 ]
        col ( MatStencil_i, 1:3 ) = [ i-1, i, i+1 ]
        call MatSetValuesStencil(jac,i1,row,i3,col,v,INSERT_VALUES,ierr)
        CHKERRQ(ierr)
     endif
  end do
  call MatAssemblyBegin(jac,MAT_FINAL_ASSEMBLY,ierr)
  CHKERRQ(ierr)
  call MatAssemblyEnd(jac,MAT_FINAL_ASSEMBLY,ierr)
  CHKERRQ(ierr)

end subroutine ComputeMatrix
