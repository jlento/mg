Multigrid example in 1D
=======================

This example solves 1D Poisson equation using Jacobi, Gauss-Seidel,
and Multigrid methods.

This example demonstrates that

- Jacobi and Gauss-Seidel methods converge very slowly for any
  larger system sizes

- multigrid converges well independent of system size (number of
  discretization points)

- change in the iterative solution (or change in the residual) is in
  general not a reliable convergence criteria (absolute residual is
  better)

The output of the demo can be directly piped to gnuplot5.

If PETSc is available, it's multigrid preconditioner is additionally
used to solve the equation.

Also, the demo contains sources that can be used to visualize the
development of solution at runtime with gnuplot.


Development environment
-----------------------

Let's use Ubuntu 16.04 LTS virtual machine as a reference environment.

1. Install git, VirtualBox and Vagrant (1.8.2+) to your OS.

2. Clone this repository

        git clone https://github.com/jlento/mg.git

3. Go to the cloned project root `mg` and boot up the VM

        cd mg
        vagrant up

4. Vagrant version 1.8.1 or earlier fails to provision the VM, but that
   can be done manually (this step is unnecessary for Vagrant 1.8.2+)

        vagrant ssh -c "sudo bash /vagrant/bootstrap.sh"
        vagrant reload

5. You may need to fix the keyboard settings in the VM to what you
   actually have in the front of you, for me for example with

        gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'fi+mac')]"


Build
-----

    cd build
    make -f ../src/makefile


Play
----

    time ./poisson_1d_identity_test 20 DD | tee >(gnuplot --persist)

One can vary the size of the system and the boundary conditions and
the solver with the 1st and 2nd command line argument,
respectively. Valid values for boundary conditions are DD, DN and DP,
for Dirichlet, Neumann, and periodic boundary conditions on the right
boundary (x=1), respectively. Left boundary (x=0) has Dirichlet
boundary conditions in all cases (to fix the solution).
