module mod_poisson_1d
  use mod_defs
  implicit none

  integer, parameter :: BC_DD = 1, BC_NN = 2, BC_PP = 3

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
    case ( 'NN' )
       bc_type = BC_NN
    case ( 'PP' )
       bc_type = BC_PP
    case default
       print *, 'ERROR: Unknown boundary condition'
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
    
    select case ( bc )
    case ( BC_DD )
       f ( 1 ) = 0.0_fp
       f ( n ) = 0.0_fp
    case ( BC_PP )
       f ( 1 ) = 2.0 * u ( 1 ) - u ( 2 ) - u ( n - 1 )
       f ( n ) = f ( 1 )
    case ( BC_NN )
       f ( 1 ) = u ( 2 ) - bc_ax
       f ( n ) = u ( n - 1 ) + bc_bx
    end select

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
