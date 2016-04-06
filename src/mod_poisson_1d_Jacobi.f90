module mod_poisson_1d_Jacobi
  use mod_defs
  use mod_poisson_1d
  implicit none

contains

  subroutine poisson_Jacobi_iteration_1d ( x )
    type ( poisson_1d ), intent ( inout ) :: x

    real ( fp ) :: D_inv, R
    integer :: n

    call x % force_boundaries
    associate ( &
         h => x % h, &
         u => x % u, &
         f => x % f )

      n = size ( u ) - 1
      D_inv = 0.5_fp * h ** 2
      R = -1.0_fp / h ** 2

      u ( 2 : n ) = D_inv * ( f ( 2 : n ) &
         - R * ( u ( 1 : n - 1 ) + u ( 3 : n + 1 ) ) )

    end associate

  end subroutine poisson_Jacobi_iteration_1d

end module mod_poisson_1d_Jacobi
