module mod_poisson_1d_GS
  use mod_defs
  use mod_poisson_1d
  implicit none

contains

  subroutine poisson_GS_iteration_1d ( x )
    type ( poisson_1d ), intent ( inout ) :: x

    integer :: i

    associate ( &
         u => x % u, &
         f => x % f, &
         n => size ( x % u ) - 1 )
      
      ! Here we have always Dirichlet boundary condition at x = 0
      u ( 1 ) = x % bc_ax
      
!#define REDBLACK
#ifndef REDBLACK

      ! Interior points, natural ordering Gauss-Seidel
      do i = 2, n
         u ( i ) = 0.5_fp * ( f ( i ) + u ( i - 1 ) + u ( i + 1 ) )
      end do

#else
      
      ! Interior points, red-black Gauss-Seidel
      if ( size ( u ) > 3 ) then
         u ( 3 : n : 2 ) = 0.5_fp * ( f ( 3 : n : 2 ) &
              + u ( 2 : n - 1 : 2 ) + u ( 4 : n + 1: 2 ) )
      end if
      u ( 2 : n : 2 ) = 0.5_fp * ( f ( 2 : n : 2 ) &
           + u ( 1 : n - 1  : 2 ) + u ( 3 : n + 1 : 2 ) )

#endif

      ! Boundary x = 1
      select case ( x % bc )
      case ( BC_DN )
         u ( n+1 ) = u ( n )  + x % bc_bx - 0.5_fp * f ( n+1 )
      end select

    end associate

  end subroutine poisson_GS_iteration_1d

end module mod_poisson_1d_GS
