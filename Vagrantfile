# -*- mode: ruby -*-
# vi: set ft=ruby :

ENV['LC_ALL']   = "en_US.UTF-8"

Vagrant.configure(2) do |config|

  config.vm.box = "box-cutter/ubuntu1604-desktop"
  config.vm.provider "virtualbox" do |vb|
    vb.gui = true
    vb.memory = "2048"
  end
 config.vm.synced_folder ".", "/vagrant", disabled: false
#  config.vm.synced_folder ".", "/home/vagrant/mg"
#  config.vm.provision :shell, path: "bootstrap.sh"
  config.vm.provision "shell", inline: "update-locale LANG=en_US.UTF-8"

#  config.vm.provision "ansible_local" do |ansible|
#    ansible.playbook = "playbook.yml"
#  end

end
