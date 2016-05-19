module mod_poisson_1d_identity_test
  use iso_fortran_env, only : eu => error_unit
  use mod_defs
  use mod_poisson_1d
  use mod_poisson_1d_solver
  implicit none

contains

  function test_solution ( nx, h, bc, bc_ax, bc_bx ) result ( u )
    integer, intent ( in ) :: nx, bc
    real ( fp ), intent ( in ) :: h, bc_ax, bc_bx
    real ( fp ), dimension ( nx + 1 ) :: u
    integer, parameter :: n = 3
    integer :: x ( n )
    real ( fp ) :: f ( n ), a0, u0
    
    ! Locations of the delta sources (derivate changes)
    x = ( [ 1, 2, 3 ] * ( nx + 1 ) ) / 4

    ! Magnitudes of the delta sources 
    f = real ( [ -2, -2, 4 ], fp )

    ! Set initial value and slope at ( x = 0 )
    select case ( bc )
    case ( BC_DD )
       u0 = bc_ax
       a0 = sum ( ( real ( x - 1, fp ) - 1.0_fp ) * f ) + bc_bx - bc_ax
    case ( BC_PP )
       u0 = 0
       a0 = sum ( ( real ( x - 1, fp ) - 1.0_fp ) * f )
    case ( BC_NN )
       u0 = 0
       a0 = bc_ax
    case default
       write ( eu, '(a)' ) 'Unrecocnized boundary condition'
       stop -1
    end select

    u = piecewise_linear ( nx, h, u0, a0, x, f )

    ! Verify (x = 1) boundary condition
    select case ( bc )
    case ( BC_DD )
       if ( abs ( u ( nx + 1 ) - bc_bx ) &
            .gt. epsilon ( u ) / h ) then
          write ( eu, * ) BC_DD, u ( nx + 1 ), bc_bx, epsilon ( u )
          stop -1
       end if
    case ( BC_PP )
       if ( abs ( u ( nx + 1 ) - u ( 1 ) ) &
            .gt. 10.0_fp * epsilon ( u ) / h ) stop -1
    case ( BC_NN )
       if ( abs ( ( u ( nx + 1 ) - u ( nx ) ) * h - bc_bx ) &
            .gt. 10.0_fp * epsilon ( a0 ) / h) stop -1
    end select

  end function test_solution

  function piecewise_linear ( nx, h, u0, a0, x, f ) result ( u )
    integer, intent ( in ) :: nx, x ( : )
    real ( fp ), intent ( in ) :: h, u0, a0, f ( : )
    real ( fp ) , dimension ( nx + 1 ) :: u
    integer, dimension ( : ), allocatable :: l
    integer :: i, ii
    real ( fp ) :: a
    
    ! Initial slope and value
    a = a0; u ( 1 ) = u0

    ! End ponts of the line segments
    l = [ 1, x,  nx + 1 ]

    do ii = 1, size ( l ) - 2
       do i = l ( ii ) + 1, l ( ii + 1 )
          u ( i ) = u ( i - 1 ) + a * h
       end do
       a = a + f ( ii ) / h
    end do
    do i = l ( size ( l ) - 1 ) + 1, l ( size ( l ) )
       u ( i ) = u ( i - 1 ) + a * h
    end do
  end function piecewise_linear

end module mod_poisson_1d_identity_test
