module mod_poisson_1d_GS
  use mod_defs
  use mod_poisson_1d
  implicit none

contains

  subroutine poisson_GS_iteration_1d ( x )
    type ( poisson_1d ), intent ( inout ) :: x

    integer :: i

    associate ( &
         h => x % h, &
         u => x % u, &
         v => x % f )

      call x % force_boundaries
      do i = 2, size ( u ) - 1
         u ( i ) = 0.5_fp * ( h ** 2 * v ( i ) + u ( i - 1 ) + u ( i + 1 ) )
      end do

  end associate

  end subroutine poisson_GS_iteration_1d

end module mod_poisson_1d_GS
