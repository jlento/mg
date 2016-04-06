module mod_poisson_1d
  use mod_defs
  implicit none

  integer, parameter :: BC_DD = 1, BC_NN = 2, BC_PP = 3

  type poisson_1d
     real ( fp ), dimension ( : ), allocatable :: f, u
     real ( fp ) :: h
     integer :: bc
     real ( fp ) :: bc_ax, bc_bx
     integer :: it, level
   contains
     procedure :: x
     procedure :: residual
     procedure :: force_boundaries
  end type poisson_1d

  interface poisson_1d
     procedure :: new_poisson_1d
  end interface poisson_1d

contains

  function new_poisson_1d ( f, h, bc, bc_ax, bc_bx, level ) result ( self )
    real ( fp ), dimension ( : ), intent ( in ) :: f
    real ( fp ), intent ( in ) :: h, bc_ax, bc_bx
    integer, intent ( in ) :: bc, level
    type ( poisson_1d ) :: self

    self % f = f
    allocate ( self % u ( size ( f ) ) )
    self % u = 0.0_fp
    self % h = h
    self % bc = bc
    self % bc_ax = bc_ax
    self % bc_bx = bc_bx
    self % it    = 0
    self % level = level

  end function new_poisson_1d

  function x ( self )
    class ( poisson_1d ), intent ( in ) :: self
    real ( fp ), dimension ( : ), allocatable :: x
    integer :: i
    x = [ (self % h * i , i = 0, size ( self % f ) - 1) ]
  end function x
  
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

  subroutine force_boundaries ( self )
    class ( poisson_1d ) :: self
    integer :: n

    n = size ( self % u )

    select case ( self % bc )
    case ( BC_DD )
       self % u ( 1 ) = self % bc_ax
       self % u ( n ) = self % bc_bx
    case ( BC_NN )
       self % u ( 1 ) = self % u ( 2 ) - self % h * self % bc_ax
       self % u ( n ) = self % u ( n - 1 ) + self % h * self % bc_bx
    case ( BC_PP )
       self % u ( 1 ) = -0.5 * ( self % h ** 2 * self % f( 1 ) &
                            + self % u ( n - 1 ) + self % u ( 2 ) )
       self % u ( n ) = self % u ( 1 )
    end select

  end subroutine force_boundaries

  function residual ( self )
    class ( poisson_1d ) :: self
    real ( fp ), dimension ( : ), allocatable :: residual
    integer :: i, n

    n = size ( self % u )
    allocate ( residual ( n ) )

    associate ( &
         h => self % h, &
         v => self % u, &
         w => self % f )

      residual ( 1 ) = 0.0_fp
      residual ( n ) = 0.0_fp
      do i = 2, n - 1
         residual ( i ) = &
              1.0 / h ** 2 * ( 2.0 * v ( i ) - v ( i + 1 ) - v ( i - 1 ) ) &
              - w ( i )
      end do

    end associate

  end function residual

end module mod_poisson_1d
