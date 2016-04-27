module mod_poisson_solver_1d
  use iso_fortran_env, only : eu => error_unit
  use mod_poisson_1d
  use mod_poisson_1d_Jacobi
  use mod_poisson_1d_GS
  use mod_poisson_1d_MG
  implicit none

contains

  subroutine solve ( system, method )
    class ( poisson_1d ) , intent ( inout ) :: system
    character ( * ), intent ( in ) :: method
    real ( fp ) :: e, e_prev, e_plot
    e_prev = huge ( e_prev )
    e_plot = huge ( e_prev )
    do
       select case ( method ( 1 : 1 ) )
       case ('J')
          call poisson_Jacobi_iteration_1d ( system )
       case ('G')
          call poisson_GS_iteration_1d ( system )
       case ('M')
          call poisson_MG_W_cycle_1d ( system )
       case default
          write ( eu, '(a, """",a,""".")' ) &
               'Unknown solver ', method
          error stop
       end select
       system % it = system % it + 1
       e = rms ( residual ( system ) )
       if (  abs ( e - e_prev ) .lt. epsilon ( e ) ) exit
       e_prev = e
    end do
  end subroutine solve
  
end module mod_poisson_solver_1d
