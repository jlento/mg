module mod_poisson_1d_Jacobi
  use mod_defs
  use mod_poisson_1d
  implicit none

contains

  subroutine poisson_Jacobi_iteration_1d ( x )
    type ( poisson_1d ), intent ( inout ) :: x

    associate ( &
         u => x % u, &
         f => x % f, &
         n => size ( x % u ) - 1 )

      ! Here we have always Dirichlet boundary condition at x = 0
      u ( 1 ) = x % bc_ax

      ! Interior points
      u ( 2:n ) = 0.5_fp * ( f ( 2:n ) + u ( 1:n-1 ) + u ( 3:n+1 ) )

      ! Boundary x = 1
      select case ( x % bc )
      case ( BC_DD )
         u ( n+1 ) = x % bc_bx
      case (BC_DP )
         u ( n+1 ) = u ( 1 )
      case ( BC_DN )
         u ( n+1 ) = u ( n )  + x % bc_bx - 0.5_fp * f ( n+1 )
      end select
      
    end associate

  end subroutine poisson_Jacobi_iteration_1d

end module mod_poisson_1d_Jacobi
