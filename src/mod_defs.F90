module mod_defs
  use iso_fortran_env
  implicit none

  integer, parameter :: fp    = REAL32

  interface num2str
     procedure :: real_to_str
     procedure :: integer_to_str
  end interface num2str

contains

  function rms ( x )
    real ( fp ), dimension ( : ), intent ( in ) :: x
    real ( fp )                                 :: rms
    rms = sqrt ( sum ( x * x ) / size ( x ) )
  end function rms

  function digits10 ( r )
    type ( real ), intent ( in ) :: r
    integer :: digits10
    digits10 = int ( log10 ( real ( 2 ** digits ( r ), fp ) ) )
  end function digits10

  function real_to_str ( r, f )
    real ( fp ), intent ( in ) :: r
    character ( * ), optional :: f
    character ( : ), allocatable :: real_to_str
    character ( 100 ) :: buf
    if ( present ( f ) ) then
       write ( buf, f ) r
    else
       write ( buf, '(g0)' ) r
    end if
    real_to_str = trim ( buf )
  end function real_to_str

  function integer_to_str ( r, f )
    integer ( fp ), intent ( in ) :: r
    character ( * ), optional :: f
    character ( : ), allocatable :: integer_to_str
    character ( 100 ) :: buf
    if ( present ( f ) ) then
       write ( buf, f ) r
    else
       write ( buf, '(i0)' ) r
    end if
    integer_to_str = trim ( buf )
  end function integer_to_str

end module mod_defs
