module mod_poisson_1d_GS
  use mod_defs
  use mod_poisson_1d
  implicit none

contains

  subroutine poisson_GS_iteration_1d ( x )
    type ( poisson_1d ), intent ( inout ) :: x

    integer :: i, n

    associate ( &
         h => x % h, &
         u => x % u, &
         v => x % f )

      call x % force_boundaries
      ! Natural ordering Gauss-Seidel
      do i = 2, size ( u ) - 1
         u ( i ) = 0.5_fp * ( h ** 2 * v ( i ) + u ( i - 1 ) + u ( i + 1 ) )
      end do

      ! Red-Black Gauss-Seidel
!      n = size ( x % u )
!      u ( 2 : n - 1 : 2 ) = 0.5_fp * ( h ** 2 * v ( 2 : n - 1 : 2 ) &
!           + u ( 1 : n - 2  : 2 ) + u ( 3 : n : 2 ) )
!      if ( size ( u ) > 3 ) then
!         u ( 3 : n - 1 : 2 ) = 0.5_fp * ( h ** 2 * v ( 3 : n - 1 : 2 ) &
!              + u ( 2 : n - 2 : 2 ) + u ( 4 : n : 2 ) )
!      end if
    end associate

  end subroutine poisson_GS_iteration_1d

end module mod_poisson_1d_GS
