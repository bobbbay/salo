# This section automatically creates the default directories for SEMC OS
mkdir -pv ${DESTDIR}/{bin,boot{,/grub},dev,{etc/,}opt,home,lib/{firmware,modules},lib64,mnt}
mkdir -pv ${DESTDIR}/{proc,media/{floppy,cdrom},sbin,srv,sys}
mkdir -pv ${DESTDIR}/var/{lock,log,mail,run,spool}
mkdir -pv ${DESTDIR}/var/{opt,cache,lib/{misc,locate},local}
install -dv -m 0750 ${DESTDIR}/root
install -dv -m 1777 ${DESTDIR}{/var,}/tmp
install -dv ${DESTDIR}/etc/init.d
mkdir -pv ${DESTDIR}/usr/{,local/}{bin,include,lib{,64},sbin,src}
mkdir -pv ${DESTDIR}/usr/{,local/}share/{doc,info,locale,man}
mkdir -pv ${DESTDIR}/usr/{,local/}share/{misc,terminfo,zoneinfo}
mkdir -pv ${DESTDIR}/usr/{,local/}share/man/man{1,2,3,4,5,6,7,8}
for dir in ${DESTDIR}/usr{,/local}; do
    ln -sv share/{man,doc,info} ${dir}
done

# This section installs /etc/ - it needs touchups, maybe
# even use it with the package manager?
git clone https://github.com/semcos-packages/etc/
cd etc/
cp ./0.0.4a/* ${DESTDIR}/etc/

# Intialize /var here

# Initialize /boot here

# Install busybox here

# Compile the kernel here

# Install zlib here

# Install more things (like gcc) here

# Final touchups here
