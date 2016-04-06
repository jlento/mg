program test_1d
  use iso_fortran_env, only : ou => output_unit
  use mod_defs
  use mod_poisson_1d
  use mod_poisson_1d_Jacobi
  use mod_poisson_1d_GS
  use mod_poisson_1d_MG
  use mod_gnuplot
  implicit none

  ! Usage : time ./test_1d 20 DD J | tee >(gnuplot --persist)

  ! Command line arguments
  integer         :: nx
  character ( 2 ) :: bc
  character ( 2 ) :: method

  type ( poisson_1d ) :: system

  real ( fp )  :: h, rms_error
  real ( fp )  :: bc_ax, bc_bx

  call read_command_line_args()
  h     = 1.0_fp / real ( nx, fp )
  bc_ax = 0.0_fp
  bc_bx = 0.0_fp

  write ( ou, '(a)')         '# Poisson equation in 1D'
  write ( ou, '(a)')         '# ----------------------'
  write ( ou, '(a,i10)')     '#      Number of cells: ', nx
  write ( ou, '(a,a10)')     '#  Boundary conditions: ', bc
  write ( ou, '(a,a10)')     '#               Solver: ', trim(method)
  write ( ou, '(a,g14.4)')   '#         Grid spacing: ', h
  select case ( bc )
  case ( 'DD' )
     write ( ou, '(a,g14.5)')   '#               u(x=0): ', bc_ax
     write ( ou, '(a,g14.5)')   '#               u(x=1): ', bc_bx
  case ( 'NN' )
     write ( ou, '(a,g14.5)')   "#              u'(x=0): ", bc_ax
     write ( ou, '(a,g14.5)')   "#              u'(x=1): ", bc_bx
  end select

  system = poisson_1d ( source_field ( nx + 1, h ), h, &
                        bc_type ( bc ), bc_ax, bc_bx, 0 )

  do
     select case ( method )
     case ('J')
        call poisson_Jacobi_iteration_1d ( system )
     case ('GS')
        call poisson_GS_iteration_1d ( system )
     case ('MG')
        call poisson_MG_W_cycle_1d ( system )
     case default
        stop
     end select

     rms_error = rms ( system % residual () )
     call plot_iterations ( rms_error, system )
     if ( converged ( rms_error ) ) exit
     system % it = system % it + 1

  end do

  write ( ou, '(a)' ) "plot '-' w lp t 'f*h/8', '' w lp t 'u'"
  call gnuplot_write_data ( &
       data = [ system % x (), system % f * h / 8.0_fp ], &
       unit = ou )
  write ( ou, '("e")' )
  call gnuplot_write_data ( &
       data = [ system % x (), system % u ], &
       unit = ou )
  write ( ou, '("e")' )

  write ( ou, '(a,i10)' )  '# Number of iterations: ', system % it
  write ( ou, '(a,g14.5)') '# Normalized RMS error: ', &
       rms_error / rms ( system % f )

contains

  subroutine read_command_line_args ()
    character ( 100 ) :: arg
    call get_command_argument ( 1, arg )
    read ( arg, * ) nx
    call get_command_argument ( 2, arg )
    read ( arg, * ) bc
    call get_command_argument ( 3, arg )
    read ( arg, * ) method
  end subroutine read_command_line_args

  function source_field ( n, h ) result ( b )
    integer, intent ( in ) :: n
    real ( fp ), intent ( in ) :: h
    real ( fp ), dimension ( : ), allocatable :: b
    allocate ( b ( n ) )
    b = 0.0_fp
    b ( n/2 - 5 ) =  -2.0_fp
    b ( n/2 + 1 ) =  -2.0_fp
    b ( n/2 + 7 ) =   4.0_fp
    b = b / h
  end function source_field

  function converged ( rms )
    real ( fp ), intent ( in ) :: rms
    logical :: converged
    real ( fp ), save :: rms_prev = huge ( rms )
    if ( rms + epsilon ( rms_error ) > rms_prev ) then
       converged = .true.
    else
       converged = .false.
    end if
    rms_prev = rms
  end function converged

  subroutine plot_iterations ( rms_error, system )
    real ( fp ), intent ( in ) :: rms_error
    type ( poisson_1d ), intent ( in ) :: system
    character ( * ), parameter :: fname = 'iterations.dat'
    integer, save :: unit
    logical :: opened
    real ( fp ), save :: rms_plot = huge ( rms_error )
    inquire ( file = fname, opened = opened )
    if ( .not. opened ) then
       open ( newunit = unit, file = fname )
       write ( unit, '("#",2a16)') '"x"', '"Source"'
       call gnuplot_write_data ( &
            data   = [ system % x (), system % f * system % h / 8.0_fp ], &
            unit   = unit )
       write ( unit, '(a)' )
       write ( unit, '(a)' )
       write ( unit, '("#",2a16)') '"x"', '"Solution"'
       call gnuplot_write_data ( &
            data   = [ system % x (), system % u ], &
            unit   = unit )
       write ( unit, '(a)' )
       flush ( unit )
       call gnuplot_open
       call gnuplot_command ( "plot '" // fname // "' i 0 w lp t 'Source'," &
            // "'' i 1 w lp t 'Solution'" )
    else
       if ( rms_plot / rms_error > 2.0_fp ) then
          call gnuplot_write_data ( &
               data = [ system % x (), system % u ], &
               unit = unit )
          flush ( unit )
          call gnuplot_command ( "replot" )
          rms_plot = rms_error
       end if
    end if
  end subroutine plot_iterations

end program test_1d
