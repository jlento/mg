module mod_poisson_1d_PETSc_MG
  use iso_fortran_env, only : eu => error_unit
  use mod_defs
  use mod_poisson_1d
  implicit none

  ! Based on PETSc ex29.c
  
contains

  subroutine poisson_PETSc_MG_1d ( system )
    type ( poisson_1d ), intent ( inout ) :: system
#ifndef HAVE_PETSC
    write ( eu, '(a)' ) 'Compile with PETSC_DIR=...'
    error stop
  end subroutine poisson_PETSc_MG_1d
end module mod_poisson_1d_PETSc_MG
#else
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscpc.h>
#include <petsc/finclude/petscksp.h>
#include <petsc/finclude/petscdm.h>
#include <petsc/finclude/petscdmda.h>

    ! Modified from PETSc example
    ! ksp/examples/tutorials/ex22f.F

    external         ComputeRHS, ComputeMatrix

    PetscErrorCode   ierr
    DM               da
    KSP              ksp
!    Vec              x
!    PetscInt         i1,i3
    Vec              ctx
    PetscInt         M, dof, s
    PetscReal        xmin, xmax
    
    call  PetscInitialize( PETSC_NULL_CHARACTER, ierr )

    M = size ( system % f )
    s = 1
    dof = 1
    xmin = 0.0_fp
    xmin = 1.0_fp
    
    call KSPCreate ( MPI_COMM_WORLD, ksp, ierr )
    call DMDACreate1d ( PETSC_COMM_WORLD, DM_BOUNDARY_NONE, &
         M, dof, s, PETSC_NULL_INTEGER, da, ierr )
!    call DMDASetUniformCoordinates( da, xmin, xmax, &
!         PETSC_NULL_REAL, PETSC_NULL_REAL, &
!         PETSC_NULL_REAL, PETSC_NULL_REAL, ierr )
    
    call VecCreate ( PETSC_COMM_WORLD, ctx, ierr )
    call VecSetSizes ( ctx, PETSC_DECIDE, M, ierr )
    call VecSetFromOptions ( ctx, ierr )
    call CopyArray2Vec ( system % f, ctx )

    call KSPSetComputeRHS ( ksp, ComputeRHS, ctx, ierr )
    call KSPSetComputeOperators( ksp, ComputeMatrix, ctx, ierr )
    call KSPSetDM ( ksp, da, ierr )
!    call KSPSetFromOptions ( ksp, ierr )
    call KSPSolve ( ksp, PETSC_NULL_OBJECT, PETSC_NULL_OBJECT, ierr )
    call KSPGetSolution ( ksp, ctx, ierr )
    call CopyVec2Array ( ctx, system % u )
!      call VecView(x,PETSC_NULL_OBJECT,ierr)
    call KSPDestroy( ksp, ierr )
    call DMDestroy( da, ierr )
!    call PetscFinalize( ierr )

  end subroutine poisson_PETSc_MG_1d

  
  subroutine CopyVec2Array ( x, a )
    implicit none
    
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h90>

    Vec             x
    real ( fp ), dimension ( : ) :: a
    PetscScalar, pointer :: xx_v ( : )
    PetscErrorCode  ierr
    PetscInt        n

!    call VecGetSize ( x, n, ierr )
!    allocate ( a ( n ) )
    call VecGetArrayReadF90 ( x, xx_v, ierr )
    a = xx_v
    call VecRestoreArrayReadF90 ( x, xx_v, ierr )

  end subroutine CopyVec2Array

  subroutine CopyArray2Vec ( a, x )
    implicit none
    
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h90>

    real ( fp ), dimension ( : ) :: a
    Vec             x
    PetscScalar, pointer :: xx_v ( : )
    PetscErrorCode  ierr

    call VecGetArrayF90 ( x, xx_v, ierr )
    xx_v = a
    call VecRestoreArrayF90 ( x, xx_v, ierr )

  end subroutine CopyArray2Vec

end module mod_poisson_1d_PETSc_MG


subroutine ComputeRHS ( ksp, b, ctx, ierr )
 implicit none

#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscdm.h>
#include <petsc/finclude/petscdmda.h>

 PetscErrorCode  ierr
 PetscInt mx,my,mz
 PetscScalar  h
 Vec          b
 KSP          ksp
 DM           da
 Vec          ctx

! call KSPGetDM ( ksp, da, ierr )
! call DMDAGetInfo( da, PETSC_NULL_INTEGER, mx, &
!      PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, &
!      PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, &
!      PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, &
!      PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, &
!      PETSC_NULL_INTEGER, ierr )
 call VecCopy ( ctx, b )
end subroutine ComputeRHS


subroutine ComputeMatrix(ksp,JJ,jac,ctx,ierr)
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
  PetscInt     ctx
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

#endif
