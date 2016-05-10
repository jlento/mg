#!/usr/bin/env bash

# Ubuntu 16.04 LTS

####################
# Desktop settings #
####################

# Keyboard
dbus-launch gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'fi+mac')]"

# Language
update-locale LANG=en_US.UTF-8

############
# Packages #
############

apt-get update
apt-get install -y synaptic git emacs gfortran makedepf90 gnuplot5 libpetsc3.6
 