module mod_poisson_1d_identity_test
  use iso_fortran_env, only : eu => error_unit
  use mod_defs
  use mod_poisson_1d
  use mod_poisson_1d_solver
  implicit none

contains

  function test_solution ( nx, bc, bc_ax, bc_bx ) result ( u )
    integer, intent ( in ) :: nx, bc
    real ( fp ), intent ( in ) :: bc_ax, bc_bx
    real ( fp ), dimension ( nx + 1 ) :: u

    integer, parameter :: n = 3
    integer :: i, x_i ( n ), f ( n )
    real (fp) :: c

    x_i = ( [ 1, 2, 3 ] * ( nx + 1 ) ) / 4
    f   = [ -2, -2, 4 ]

    ! A simple solution
    u = piecewise_linear ( nx, x_i, f )

    ! Enforce boundary conditions
    select case ( bc )
    case ( BC_DD )
       c = ( u(nx+1) + bc_bx - bc_ax ) / real ( nx, fp )
    case ( BC_DP )
       c = u(nx+1) / nx
    case ( BC_DN )
       c = ( bc_bx - sum ( f ) ) / nx
    case default
       write ( eu, '(a)' ) 'Unrecocnized boundary condition'
       stop -1
    end select
    u = u + bc_ax - c * [ ( i, i = 0, nx ) ]

    ! Scale so that it looks nice on x = [0,1] interval
    u = u / real ( nx, fp ) * 4.0_fp
    
  end function test_solution

  function piecewise_linear ( nx, x_i, f ) result ( u )
    integer, intent ( in )                :: nx, x_i ( : ), f ( : )
    real ( fp ) , dimension ( nx + 1 )    :: u
    integer :: i, j, n

    ! u       -  discretized piecewise linear curve, for the first line
    !            segment y = 0
    ! nx      -  number of discretizion intervals
    ! x_i     -  vertex indices of slope changes
    ! f       -  slope changes at x_i's

    ! grid point spacing h = 1
    
    n = size ( x_i )
    
    u = 0.0_fp
    do i = 1, n - 1
       u ( x_i(i)+1: ) = u ( x_i(i)+1: ) &
            + f ( i ) * [ ( j, j = 1, nx+1-x_i(i) ) ]
    end do
    u ( x_i(n)+1: ) = u ( x_i(n)+1: ) &
         + f ( n ) * [ ( i, i = 1, nx+1-x_i(n) ) ]

  end function piecewise_linear

end module mod_poisson_1d_identity_test
