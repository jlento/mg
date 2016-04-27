module mod_poisson_1d_Jacobi
  use mod_defs
  use mod_poisson_1d
  implicit none

contains

  subroutine poisson_Jacobi_iteration_1d ( x )
    type ( poisson_1d ), intent ( inout ) :: x

    integer :: n

    associate ( &
         u => x % u, &
         f => x % f )

      n = size ( u ) - 1

      u ( 2 : n ) = 0.5_fp * ( f ( 2 : n ) &
           + u ( 1 : n - 1 ) + u ( 3 : n + 1 ) )

    end associate

  end subroutine poisson_Jacobi_iteration_1d

end module mod_poisson_1d_Jacobi
