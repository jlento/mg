# -*- mode: ruby -*-
# vi: set ft=ruby :

ENV['LC_ALL'] = "en_US.UTF-8"

Vagrant.configure(2) do |config|
  config.vm.box = "box-cutter/ubuntu1604-desktop"
  config.vm.provider "virtualbox" do |vb|
    vb.gui = true
    vb.memory = "2048"
  end
  config.vm.synced_folder ".", "/vagrant", disabled: false
  config.vm.provision :shell, path: "bootstrap.sh"
end
