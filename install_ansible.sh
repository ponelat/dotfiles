#sudo apt-add-repository -y ppa:ansible/ansible
#sudo apt-get update
#sudo apt-get install -y ansible
#sudo mv /etc/ansible/hosts /etc/ansible/hosts.orig
echo -e "localhost ansible_connection=local" | sudo tee /etc/ansible/hosts

