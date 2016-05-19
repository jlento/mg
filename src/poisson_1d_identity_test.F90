program poisson_identity_test_1d
  use iso_fortran_env, only : INT64, ou => output_unit
  use mod_defs
  use mod_poisson_1d_identity_test
  implicit none

  ! Command line arguments
  integer         :: nx
  character ( 2 ) :: bc
  
  type ( poisson_1d ), dimension ( nsolvers ) :: system

  real ( fp )  :: h, bc_ax, bc_bx
  real ( fp ), dimension ( : ), allocatable :: x, f, u
  integer :: i
  integer ( INT64 ) :: tstart ( nsolvers ), tstop ( nsolvers ), rate

  call read_command_line_args()

  ! Domain is [ 0, 1 ]
  h     = 1.0_fp / real ( nx, fp )
  x     = [ ( i * h, i = 0, nx ) ]

  ! Boundary values
  bc_ax = 0.0_fp
  bc_bx = 0.0_fp

  call write_header

  ! Reference system
  u     = 4.0_fp * h &
          * test_solution ( nx, h, bc_type ( bc ), bc_ax, bc_bx )
  f     = -1.0_fp * laplace ( u / h / 4.0_fp, bc_type ( bc ), bc_ax, bc_bx )

  ! Solvers
  do i = 1, nsolvers
     system ( i ) = poisson_1d ( f * h * 4.0_fp, &
          bc_type ( bc ), bc_ax, bc_bx, level = 0 )
     call system_clock ( tstart ( i ), rate )
     call solve ( system ( i ), i )
     call system_clock ( tstop ( i ) )
  end do

  call write_footer

contains

  subroutine usage
    write ( ou, '(a)') &
         'poisson_identity_test_1d tests the implemented 1D-Poisson', &
         'equation solvers.', &
         '', &
         'Usage :', &
         '    poisson_identity_test_1d NX BC', &
         '', &
         '    where NX is the number of intervals in the discretization,', &
         '    and the boundary condition BC is either "DD", "PP", or "NN",', &
         '    for Dirichlet, periodic, or Neumann, respectively.', &
         '', &
         '    The output can be piped directly to gnuplot 5.0+.', &
         '', &
         'Example:', &
         '    time ./test_1d 20 PP | tee >(gnuplot -e "set term wxt" -p -)'
  end subroutine usage

  subroutine read_command_line_args ()
    character ( 100 ) :: arg
    call get_command_argument ( 1, arg )
    read ( unit = arg, fmt = *, end = 100, err = 100 ) nx
    call get_command_argument ( 2, arg )
    read ( unit = arg, fmt = *, end = 100, err = 100 ) bc
    return
100 call usage
    error stop
  end subroutine read_command_line_args

  subroutine write_header
    write ( ou, '(5(a,/),a,a10,/,2(a,f10.5,/),a,i10,/)') &
         '#!/usr/bin/env gnuplot', &
         'if ( GPVAL_VERSION < 5.0 ) print "Requires gnuplot 5.0+"; exit', &
         '', &
         '# Poisson equation in 1D', &
         '# ----------------------', &
         '#  Boundary conditions: ', bc, &
         '#               u(x=0): ', 0.0_fp, &
         '#               u(x=1): ', 0.0_fp, &
         '#      Number of cells: ', nx
  end subroutine write_header
  
  subroutine write_footer
    integer :: i, j

    ! Gnuplot inline data block
    write ( ou, '(/,a)') '$db << EOD'
    write ( ou, '(*(a15))' ) '"x"', '"f"', '"Ref"', &
         ( '"' // trim ( method ( i ) ) // '"', i = 1, nsolvers)
    do j = 1, nx + 1
       write ( ou, '(*(e15.5))' ) &
            x ( j ), &
            f ( j ), &
            u ( j ), &
            ( system ( i ) % u ( j ), i = 1, nsolvers )
    end do
    write ( ou, '(a)') 'EOD'
    
    write ( ou, '(/,*(a,:,/))') &
         'set xzeroaxis', &
         'set xrange [0:1]', &
         'set key outside autotitle columnhead &
         &title "' // bc // ' boundary conditions"', &
         'set multiplot layout 2, 1 title "Poisson equation in 1D"'

    write ( ou, '(/,a)') 'set title "Solutions"'
    select case ( bc_type ( bc ) )
    case ( BC_DD, BC_PP )
       write ( ou, '(a)') 'set yrange [-4:4]'
    case ( BC_NN )
       write ( ou, '(a)') 'set yrange [-8:4]'
    end select
    write ( ou, '("plot",2(T6,a,/),*(T6,a,i0,a,i0,:,", \",/))' ) &
         '"$db" u 1:2 w impulses lc 1, \', &
         '"$db" u 1:3 w lp lc 2, \', &
         ( &
         '"$db" u 1:', i + 3, ' w l lc ', i + 2, &
         i = 1, nsolvers )

    write ( ou, '(/,a)') 'set title "Errors"'
    write ( ou, '(a)') 'unset yrange'
    write ( ou, '("plot",*(T6,a,i0,a,i0,:,", \",/))' ) &
         ( &
         '"$db" u 1:($', i + 3, '-$3) w l lc ', i + 2, &
         i = 1, nsolvers )

    
    write ( ou, '(/,"#",a25,3a15)' ) 'Method: ', &
         ( trim ( method (i) ), i = 1, nsolvers )
    write ( ou, '("#",a25,3i15,/,5("#",a25,3f15.5,/))' )  &
         'Number of iterations: ', &
         ( system ( i ) % it, i = 1, nsolvers ), &
         'Wall clock time (s): ', &
         real ( tstop - tstart, fp ) / real ( rate, fp ), &
         'Residual RMS: ', &
         ( rms ( residual ( system ( i ) ) ) , i = 1, nsolvers ), &
         'Residual maximum: ', &
         ( sqrt ( maxval ( residual ( system ( i ) ) ** 2 ) ) , &
         i = 1, nsolvers ), &
         'Error RMS: ', &
         ( rms ( system ( i ) % u - u ) , i = 1, nsolvers ), &
         'Error maximum: ', &
         ( sqrt ( maxval ( ( system ( i ) % u - u ) ** 2 ) ) , &
         i = 1, nsolvers )
    
  end subroutine write_footer

end program poisson_identity_test_1d
