module mod_poisson_1d_MG
  use mod_defs
  use mod_poisson_1d
  use mod_poisson_1d_GS, only: smooth => poisson_GS_iteration_1d
  implicit none

contains

  recursive subroutine poisson_MG_W_cycle_1d  ( fine )
    type ( poisson_1d ), intent ( inout ) :: fine
    type ( poisson_1d ) :: coarse
    integer :: i

    do i = 1, 2
       call smooth ( fine )
       call smooth ( fine )
       if ( size ( fine % u ) > 3 ) then
          call restrict ( fine, coarse )
          call poisson_MG_W_cycle_1d  ( coarse )
          call interpolate ( coarse, fine )
       end if
       call smooth ( fine )
       call smooth ( fine )
    end do
  end subroutine poisson_MG_W_cycle_1d

  subroutine restrict ( x, x_c )
    type ( poisson_1d ), intent ( in )  :: x
    type ( poisson_1d ), intent ( out ) :: x_c

    integer     :: n_c
    real ( fp ) :: h
    real ( fp ), dimension ( : ), allocatable :: f_c
    integer :: n

    n = size ( x % u ) - 1
    n_c = 2 ** ( int ( log ( real ( n, fp ) ) / log ( 2.0_fp ) ) )
    if ( n_c .eq. n ) n_c = n / 2
    h = real ( n_c, fp ) / real ( n, fp )
    f_c = linear_interpolation_1d ( residual ( x ), n_c )
    x_c = poisson_1d ( f_c / h ** 2, x % bc, x % bc_ax, x % bc_bx, &
         x % level + 1 )
  end subroutine restrict

  subroutine interpolate ( x_c, x )
    type ( poisson_1d ), intent ( in )    :: x_c
    type ( poisson_1d ), intent ( inout ) :: x
    integer :: n, n_c
    real ( fp ) :: h
    n     = size ( x % u ) - 1
    n_c   = size ( x_c % u ) - 1
    h     = real ( n_c, fp ) / real ( n, fp )
    x % u = x % u - linear_interpolation_1d ( x_c % u, n )
  end subroutine interpolate

  function linear_interpolation_1d ( f, n_c ) result ( f_c )
    real ( fp ), dimension ( : ), intent ( in )  :: f
    integer, intent ( in )                       :: n_c
    real ( fp ), dimension ( : ), allocatable    :: f_c
    real ( fp ) :: h, h_c
    integer :: i, j, n
    n   = size ( f ) - 1
    h   = 1.0_fp / real ( n, fp )
    h_c = 1.0_fp / real ( n_c, fp )
    allocate ( f_c ( n_c + 1 ) )
    do i = 2, n_c
       j = int ( real ( i - 1, fp ) * h_c / h ) + 1
       f_c ( i ) = f ( j ) + ( ( i - 1 ) * h_c - ( j - 1 ) * h ) &
            * ( f ( j + 1 ) - f ( j ) ) / h
    end do
    f_c ( 1 )       = f ( 1 )
    f_c ( n_c + 1 ) = f ( n + 1 )
  end function linear_interpolation_1d

end module mod_poisson_1d_MG
