module mod_poisson_1d_Jacobi
  use mod_defs
  use mod_poisson_1d
  implicit none

contains

  subroutine poisson_Jacobi_iteration_1d ( x )
    type ( poisson_1d ), intent ( inout ) :: x
    real ( fp ) :: u1, un
    integer :: n

    associate ( &
         u => x % u, &
         f => x % f )

      n = size ( u ) - 1

      select case ( x % bc )
      case ( BC_DD )
         u ( 2 : n ) = 0.5_fp * ( f ( 2 : n ) &
              + u ( 1 : n - 1 ) + u ( 3 : n + 1 ) )
      case ( BC_PP )
         u1 = 0.5_fp * ( f ( 1 ) + u ( n ) + u ( 2 ) )
         u ( 2 : n ) = 0.5_fp * ( f ( 2 : n ) &
              + u ( 1 : n - 1 ) + u ( 3 : n + 1 ) )
         u ( 1     ) = u1
         u ( n + 1 ) = u1
         u = u - u1
      case ( BC_NN )
         u1 = u ( 2 ) - x % bc_ax
         un = u ( n ) + x % bc_bx
         u ( 2 : n ) = 0.5_fp * ( f ( 2 : n ) &
              + u ( 1 : n - 1 ) + u ( 3 : n + 1 ) )
         u ( n + 1 ) = un
         u = u - u1
      end select
    end associate

  end subroutine poisson_Jacobi_iteration_1d

end module mod_poisson_1d_Jacobi
