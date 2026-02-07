# Run the fedora-post-install.sh script
# Troubleshooting
## group id may have changed. Use the command below to find the group
Below command filters on word devel
```
dnf group list --hidden \*devel\*
```
# If using the Gnome Desktop
## Use gnome-tweaks to enable the minimize and maximize buttons on windows
    See the 'Windows -> Titlebar Buttons' section to enable the Maximize and Minimize buttons.
## Screen Lock and Power Settings
    In the Gnome Settings dialog, go to the 'Power' tab
    Change the Screen Blank and Automatic Suspend as needed
## Enable 'Night Light'
    In the Gnome Settings dialog, got to the 'Displays' tab
    Toggle on 'Night Light' and adjust settings as needed
## Auto delete Trash
    Go to Settings -> Privacy -> File History & Trash and toggle the Automatic Delete Trash Content option as required
## Set Power Profile
    Power profiles are accessible from the Settings page and through the top panel (or the system tray)
## Use unrestricted flathub apps
    flatpak remote-modify --enable flathub
## Extensions
   A list of extensions I have found useful:
   - Blur my Shell by aunetx
   - Clipboard Indicator by tudmotu
   - Color Picker by tuberry (groot)
   - Dash to Dock by micxgx (michele_g)
   - Dash to Panel  by jderose9
   - Vitals by CoreCoding
   - Wallpaper Slideshow by azwallpaper (andrew_z)d
# nvm
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/master/install.sh | bash
	nvm install --lts
	nvm install stable
	nvm ls
	nvm current
	nvm use 18.16.0
## nvm links
https://www.linuxcapable.com/how-to-install-node-js-on-fedora-linux/

## Installing Fonts
https://docs.fedoraproject.org/en-US/quick-docs/fonts/
### Fonts
https://www.nerdfonts.com/font-downloads
- AnonymousPro
- CascadiaMono
- FiraCode
- Go-Mono
- Iosevka
- JetBrainsMono
# Links
https://medium.com/@KarolDanisz/things-to-do-after-installing-fedora-39-workstation-cc8eb4090dd1
https://www.hackingthehike.com/fedora39-guide/

# ssh
Add the following lines to /etc/ssh/sshd_config, replacing <USER> with user name to allow

```
PasswordAuthentication no
PermitRootLogin no
AllowUsers <USER>
```

# Install nVidia Drivers
## Links
https://www.tecmint.com/install-nvidia-drivers-in-linux/
https://rpmfusion.org/Howto/Secure%20Boot
## Check if system can detect nVidia GPU
lspci | grep -Ei 'VGA|3D'
If not detected then must investigate and get it detected before proceeding with these instructions.
## Update the system
sudo dnf update --refresh
## Install Kernel Headers and Development Tools
sudo dnf install kernel-devel kernel-headers gcc make dkms acpid libglvnd-glx libglvnd-opengl libglvnd-devel pkgconfig

## Install RPM Fusion Repositories in Fedora
sudo dnf install https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
sudo dnf install https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
sudo dnf makecache

## Install NVIDIA Driver and CUDA Support in Fedora
sudo dnf install akmod-nvidia xorg-x11-drv-nvidia-cuda
reboot

## Secure Boot
If secure boot is enable, the nVidia driver most likely was not loaded.  Check by running the following on the command line:
nvidia-smi
The output should indicate if the GPU was found
### Install needed tools
sudo dnf install kmodtool akmods mokutil openssl

### Generate key with default values
sudo kmodgenca -a

### enroll the public key in MOK
sudo mokutil --import /etc/pki/akmods/certs/public_key.der
You will be asked to create a password.  Remember it for later.
### Reboot into MOK
systemctl reboot

### MOK management
After reboot MOK management is launched
Select prompts to "Enroll MOK"
"Yes" to cofirm enrollment
You will be prompted for the password created above
Reboot afterwords and nVidia driver should be loaded.
