# Mount all XFS partitions
for device in `blkid | grep xfs | grep /dev/sd | cut -f2 -d' ' | cut -f2 -d'=' | sed -e 's/"//g'`
do
    mkdir -p /export/$device
    echo "UUID=$device /export/$device xfs defaults,uquota,pquota 1 2" >> /etc/fstab
    echo "/export/$device *(rw,no_subtree_check,fsid=$device,no_root_squash)" >> /etc/exports
done

mount -a
exportfs -a
systemctl restart nfs
rpc.rquotad
