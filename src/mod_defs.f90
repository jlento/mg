module mod_defs
  use iso_fortran_env
  implicit none

  integer, parameter :: fp = REAL32

contains

  function rms ( x )
    real ( fp ), dimension ( : ), intent ( in ) :: x
    real ( fp )                                 :: rms
    rms = sqrt ( sum ( x * x ) / ( size ( x ) + 1 ) )
  end function rms

end module mod_defs
