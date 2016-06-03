module mod_poisson_1d
  use mod_defs
  implicit none

  integer, parameter :: BC_DD = 1, BC_DP = 2, BC_DN = 3

  type poisson_1d
     real ( fp ), dimension ( : ), allocatable :: f, u
     integer :: bc
     real ( fp ) :: bc_ax, bc_bx
     integer :: it, level
  end type poisson_1d

  interface poisson_1d
     procedure :: new_poisson_1d
  end interface poisson_1d

contains

  function new_poisson_1d ( f, bc, bc_ax, bc_bx, level ) result ( self )
    real ( fp ), dimension ( : ), intent ( in ) :: f
    real ( fp ), intent ( in ) :: bc_ax, bc_bx
    integer, intent ( in ) :: bc, level
    type ( poisson_1d ) :: self

    self % f = f
    allocate ( self % u ( size ( f ) ) )
    self % u = 0.0_fp
    self % bc = bc
    self % bc_ax = bc_ax
    self % bc_bx = bc_bx
    self % it    = 0
    self % level = level

  end function new_poisson_1d

  function bc_type ( s )
    character ( * ), intent (in ) :: s
    integer :: bc_type
    select case ( s )
    case ( 'DD' )
       bc_type = BC_DD
    case ( 'DP' )
       bc_type = BC_DP
    case ( 'DN' )
       bc_type = BC_DN
    case default
       print *, 'ERROR: Unknown or not implemented boundary condition'
       stop
    end select
  end function bc_type

  
  function laplace ( u, bc, bc_ax, bc_bx ) result ( f )
    real ( fp ), dimension ( : ), intent ( in ) :: u
    integer, intent ( in ) :: bc
    real ( fp ), intent ( in ) :: bc_ax, bc_bx
    real ( fp ), dimension ( : ), allocatable :: f
    integer :: i, n

    n = size ( u )
    
    allocate ( f ( n ) )
    do i = 2, n - 1
       f ( i ) = u ( i + 1 ) - 2.0 * u ( i ) + u ( i - 1 )
    end do

  end function laplace

  
  function residual ( s )
    class ( poisson_1d ) :: s
    real ( fp ), dimension ( : ), allocatable :: residual
    integer :: i, n

    n = size ( s % u )
    allocate ( residual ( n ) )

    associate ( &
         v => s % u, &
         w => s % f )

      residual ( 1 ) = 0.0_fp
      residual ( n ) = 0.0_fp
      do i = 2, n - 1
         residual ( i ) = 2.0 * v ( i ) - v ( i + 1 ) - v ( i - 1 ) &
              - w ( i )
      end do
    end associate

  end function residual

end module mod_poisson_1d
