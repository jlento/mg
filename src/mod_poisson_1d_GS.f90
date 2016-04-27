module mod_poisson_1d_GS
  use mod_defs
  use mod_poisson_1d
  implicit none

contains

  subroutine poisson_GS_iteration_1d ( x )
    type ( poisson_1d ), intent ( inout ) :: x

    integer :: i, n

    associate ( &
         u => x % u, &
         f => x % f )

      ! Natural ordering Gauss-Seidel
      n = size ( x % u ) - 1
      select case ( x % bc )
      case ( BC_DD )
         do i = 2, n
            u ( i ) = 0.5_fp * ( f ( i ) + u ( i - 1 ) + u ( i + 1 ) )
         end do
      case ( BC_PP )
         u ( 1 ) = 0.5_fp * ( f ( 1 ) + u ( n ) + u ( 2 ) )
         do i = 2, n
            u ( i ) = 0.5_fp * ( f ( i ) + u ( i - 1 ) + u ( i + 1 ) )
         end do
         u ( n + 1 ) = u ( 1 )
         u = u - u ( 1 )
      case ( BC_NN )
         u ( 1 ) = u ( 2 ) - x % bc_ax
         do i = 2, n
            u ( i ) = 0.5_fp * ( f ( i ) + u ( i - 1 ) + u ( i + 1 ) )
         end do
         u ( n + 1 ) = u ( n ) + x % bc_bx
         u = u - u ( 1 )
      end select

      ! Red-Black Gauss-Seidel
!      n = size ( x % u )
!      u ( 2 : n - 1 : 2 ) = 0.5_fp * ( f ( 2 : n - 1 : 2 ) &
!           + u ( 1 : n - 2  : 2 ) + u ( 3 : n : 2 ) )
!      if ( size ( u ) > 3 ) then
!         u ( 3 : n - 1 : 2 ) = 0.5_fp * ( f ( 3 : n - 1 : 2 ) &
!              + u ( 2 : n - 2 : 2 ) + u ( 4 : n : 2 ) )
!      end if
    end associate

  end subroutine poisson_GS_iteration_1d

end module mod_poisson_1d_GS
