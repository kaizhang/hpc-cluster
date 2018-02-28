# Mount all XFS partitions
for device in `blkid | grep xfs | grep /dev/sd | cut -f2 -d' ' | cut -f2 -d'=' | sed -e 's/"//g'`
do
    mkdir -p /export/$device
    mount --uuid $device /export/$device
    echo "/export/$device *(rw,no_subtree_check,fsid=10,no_root_squash)" >> /etc/exports
done

exportfs -a
systemctl restart nfs
