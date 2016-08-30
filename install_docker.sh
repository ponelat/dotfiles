UBUNTU_CODENAME=`cat /etc/os-release | grep UBUNTU_CODENAME | cut -f2 -d=`

if ! dpkg-query -W docker-engine ; then
  sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
  echo "deb https://apt.dockerproject.org/repo ubuntu-$UBUNTU_CODENAME main" | sudo tee /etc/apt/sources.list.d/docker.list
  sudo apt-get purge lxc-docker
  sudo apt-get update
  sudo apt-get install -y docker-engine
  sudo apt-cache policy docker-engine
fi
