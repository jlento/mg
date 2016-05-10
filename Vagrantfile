# -*- mode: ruby -*-
# vi: set ft=ruby :

ENV['LC_ALL']="en_US.UTF-8"

Vagrant.configure(2) do |config|

  config.ssh.username = "vagrant"
  config.vm.box = "ubuntu/xenial64"
  config.vm.provider "virtualbox" do |vb|
    vb.gui = true
    vb.memory = "2048"
  end
  config.vm.provision :shell, path: "bootstrap.sh"

end
