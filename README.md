Multigrid demo in 1D
====================

This demo shows how iterative Jacobi, Gauss-Seidel and Multigrid
solutions converge in case of Poisson equation in 1D for a simple,
fixed source. The iteration is stopped when the change in the residual
between subsequent iterations is small.

The output of the demo can be directly piped to gnuplot5.

Also, the demo contains sources that can be used to visualize the
development of solution at runtime with gnuplot.


Development environment
-----------------------

1. Install git, VirtualBox and Vagrant (1.8.2+).
2. Clone this repository

    git clone https://github.com/jlento/mg.git

3. Run commands
    cd mg
    vagrant up

A fresh Ubuntu 16.04 virtual machine with all necessary packages should
eventually boot. The project root `.../mg/` on the host machine is
synchronized to directory `/vagrant` in the guest VM.

You may need to fix the keyboard settings in the VM to what you actully have
in the front of you, for me for example with

```
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'fi+mac')]"
```


Build
-----

```
cd build
make -f ../src/makefile
```


Play
----

```
time ./poisson_identity_test_1d 20 DD | tee >(gnuplot --persist)
```

One can vary the size of the system and the boundary conditions and
the solver with the 1st and 2nd command line argument,
respectively. Valid values for boundary conditions are DD, NN and
PP, for Dirichlet, Neumann, and periodic boundary conditions,
respectively.
