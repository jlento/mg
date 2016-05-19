module mod_gnuplot
  use, intrinsic :: iso_fortran_env
  use,intrinsic :: iso_c_binding
  use mod_defs
  implicit none

  type(c_ptr) :: handle = c_null_ptr
  private :: handle

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
    handle = c_null_ptr
    handle = popen('gnuplot --persist'//c_null_char,'w'//c_null_char)
    if ( .not. c_associated ( handle ) ) then
       print *, 'Could not open gnuplot!?!'
       stop
    end if
  end subroutine gnuplot_open

  subroutine gnuplot_command ( s )
    character ( kind = c_char, len = * ) :: s
    integer ( c_int ) :: istat
    istat = fputs ( s // c_new_line // c_null_char, handle )
    istat = fflush ( handle )
  end subroutine gnuplot_command

  subroutine gnuplot_close
    integer ( c_int ) :: istat
    istat = pclose ( handle )
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

  
  ! subroutine plot_iteration ( system )
  !   type ( poisson_1d ), intent ( in ) :: system
  !   character ( * ), parameter :: fname = 'iterations.dat'
  !   integer, save :: unit
  !   logical :: opened
  !   real ( fp ) :: h
  !   integer :: nx, i
  !   real ( fp ), dimension ( : ), allocatable :: x
  !   nx = size ( system % f ) - 1
  !   h = 1.0_fp / real ( nx, fp )
  !   x = [ ( i * h, i = 0, nx ) ]
  !   inquire ( file = fname, opened = opened )
  !   if ( .not. opened ) then
  !      open ( newunit = unit, file = fname )
  !      write ( unit, '("#",2a16)') '"x"', '"Source"'
  !      call gnuplot_write_data ( &
  !           data   = [ x, system % f / h ], &
  !           unit   = unit )
  !      write ( unit, '(a)' )
  !      write ( unit, '(a)' )
  !      write ( unit, '("#",2a16)') '"x"', '"Exact solution * 4"'
  !      call gnuplot_write_data ( &
  !           data   = [ x, poisson_analytic_solution_1d ( system ) * 4.0_fp ], &
  !           unit   = unit )
  !      write ( unit, '(a)' )
  !      write ( unit, '(a)' )
  !      write ( unit, '("#",2a16)') '"x"', '"Solution * 4"'
  !      call gnuplot_write_data ( &
  !           data   = [ x, system % u * 4.0_fp ], &
  !           unit   = unit )
  !      write ( unit, '(a)' )
  !      flush ( unit )
  !      call gnuplot_command ( 'set xzeroaxis' )
  !      call gnuplot_command ( "plot '" // fname &
  !           // "' i 2 w l t 'Solution * 4'," &
  !           // "'' i 1 w lp t 'Exact solution * 4'," &
  !           // "'' i 0 w impulses t 'Source'" )
  !   else
  !      call gnuplot_write_data ( &
  !           data = [ x, system % u * 4.0_fp ], &
  !           unit = unit )
  !      flush ( unit )
  !      call gnuplot_command ( "replot" )
  !   end if
  ! end subroutine plot_iteration

end module mod_gnuplot
