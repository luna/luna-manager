cp -r /home /usr/home
parted /dev/vdb mklabel gpt
parted /dev/vdb mkpart primary ext4 0% 100%
mkfs.ext4 /dev/vdb1
bash -c "echo '/dev/vdb1       /home ext4 defaults   0       0' >> /etc/fstab"
mount -a
cp -r /usr/home /
chown -R vagrant:vagrant /home/vagrant
