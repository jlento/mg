module mod_gnuplot
  use, intrinsic :: iso_fortran_env
  use,intrinsic :: iso_c_binding
  use mod_defs
  implicit none

  type(c_ptr) :: h = c_null_ptr

  private :: h

  interface

     function popen(command, mode) bind(C,name='popen')
       import :: c_char, c_ptr
       character(kind=c_char),dimension(*) :: command, mode
       type(c_ptr) :: popen
     end function popen

     function fputs(s, stream) bind(C,name='fputs')
       import :: c_char, c_ptr, c_int
       integer(c_int) :: fputs
       character(kind=c_char),dimension(*) :: s
       type(c_ptr),value :: stream
     end function fputs

     function fflush(handle) bind(C, name='fflush')
       use, intrinsic :: ISO_C_BINDING
       integer(c_int) :: fflush
       type (c_ptr), value :: handle
     end function fflush

     function pclose(stream) bind(C,name='pclose')
       import :: c_ptr, c_int
       integer(c_int) :: pclose
       type(c_ptr),value :: stream
     end function pclose

  end interface

contains

  subroutine gnuplot_open
    h = c_null_ptr
    h = popen('gnuplot --persist'//c_null_char,'w'//c_null_char)
    if (.not.c_associated(h)) then
       print *, 'Could not open gnuplot!?!'
       stop
    end if
  end subroutine gnuplot_open

  subroutine gnuplot_command ( s )
    character ( kind=c_char, len=* ) :: s
    integer(c_int) :: istat
    istat = fputs(s//c_new_line//c_null_char,h)
    istat = fflush(h)
  end subroutine gnuplot_command

  subroutine gnuplot_close
    integer(c_int) :: istat
    istat = pclose(h)
  end subroutine gnuplot_close

  subroutine gnuplot_write_data ( data, ncol, unit )
    real ( fp ), dimension ( : ), intent ( in )  :: data
    integer, optional :: ncol, unit
    integer :: nrow, ncol_, u
    character ( 20 ) :: fmt
    if ( present ( unit ) ) then
       u = unit
    else
       u = output_unit
    end if
    if ( present ( ncol ) ) then
       ncol_ = ncol
    else
       ncol_ = 2
    end if
    nrow = size ( data ) / ncol_
    write ( fmt, '("(",i0,"f16.5)")' ) ncol_
    write ( u, fmt ) reshape ( data, [ ncol_, nrow ], order = [ 2, 1 ] )
  end subroutine gnuplot_write_data

end module mod_gnuplot
