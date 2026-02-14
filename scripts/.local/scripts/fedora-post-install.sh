#!/usr/bin/bash

if (( $EUID != 0 )); then
	echo "Script must be run as root or with sudo"
	exit 1
fi

##################
# Helper Functions
##################
cur_date_time () {
	echo "[$(date +'%Y-%m-%d %H:%M:%S')]:"
}

notice_msg () {
	echo ">>>> $*"
}

status_msg () {
	echo "---------------------------------------------------------------------"
	echo "$(cur_date_time) $*"
	echo "---------------------------------------------------------------------"
}

warn_msg () {
	echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
	echo "$(cur_date_time) WARNING: $*"
	echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
}

error_msg () {
	echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	echo "$(cur_date_time) ERROR: $*"
	echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
}

error_check () {
	error_no=$1
	error_str=$2

	if [[ $error_no -ne 0 ]]; then
		error_msg "$error_str"
		exit $error_no
	fi
}

enable_faster_dnf () {
	status_msg "Configuring DNF for faster download of packages"
	echo "max_parallel_downloads=10" >> /etc/dnf/dnf.conf
	# echo "fastestmirror=true" >> /etc/dnf/dnf.conf
}

enable_rpm_fusion_repo () {
	status_msg "Enabling RPM Fusion Repository"
	dnf -y install https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
	error_check $? "Failed on install of rpmfusion-free-release"
	dnf -y install https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
	error_check $? "Failed on install of rpmfusion-nonfree-release"
}

install_media_codecs () {
	status_msg "Installing media codecs"

	dnf -y install gstreamer1-plugins-{bad-\*,good-\*,base} gstreamer1-plugin-openh264 gstreamer1-libav --exclude=gstreamer1-plugins-bad-free-devel
	error_check $? "Failed on install of gstreamer codecs"

	dnf -y install lame\* --exclude=lame-devel
	error_check $? "Failed on install of lame codecs"

	# dnf group upgrade --with-optional Multimedia
	dnf group upgrade --skip-unavailable Multimedia
	error_check $? "Failed on group upgrade Multimedia"
}

change_hostname () {
	status_msg "Changing hostname"
	hostnamectl set-hostname "hera"
}

install_gnome_tweaks_extensions () {
	status_msg "Installing gnome-tweaks and gnome-extensions-app"
	dnf -y install gnome-tweaks
	error_check $? "Failed to install gnome-tweaks"
	flatpak install flathub com.mattjakeman.ExtensionManager
	error_check $? "Failed to install ExtensionManager"
}

install_c_cpp_dev_tools () {
	status_msg "Installing C/C++ Development Tools and Libraries"
	dnf group install c-development
	dnf group install development-tools
	error_check $? "Failed to install group C Dev Tools and Libs"
	dnf -y install cmake
	error_check $? "Failed to install cmake"
	dnf -y install conan
	error_check $? "Failed to install conan"
    dnf -y install gcc-c++
	error_check $? "Failed to install gcc-c++"
    dnf -y install clang
	error_check $? "Failed to install clang"
    dnf -y install libasan
	error_check $? "Failed to install libasan"
}

install_golang () {
	status_msg "Installing golang. See https://developer.fedoraproject.org/tech/languages/go/go-installation.html"
	dnf -y install golang
	error_check $? "Failed to install golang"
	notice_msg "Create $HOME/go and export GOPATH=$HOME/go in your .bashrc"
}

install_brave () {
	status_msg "Installing Brave browser"
	dnf -y install dnf-plugins-core
	error_check $? "Failed to install brave browser"

	dnf config-manager addrepo --from-repofile=https://brave-browser-rpm-release.s3.brave.com/brave-browser.repo
	error_check $? "Failed to install brave browser"

	dnf -y install brave-browser
	error_check $? "Failed to install brave browser"
}

install_vscode () {
	status_msg "Installing VSCode"
	rpm --import https://packages.microsoft.com/keys/microsoft.asc
	error_check $? "Failed to import microsoft key"
	sh -c 'echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo'
	error_check $? "Failed to add microsoft VSCode repo"
	dnf update
	dnf -y install code
	error_check $? "Failed to install VSCode"
}

install_command_line_editors () {
	status_msg "Installing command line editors"
	declare -a EditorsArray=( "vim" "neovim" "emacs" )
	for item in ${EditorsArray[@]}; do
		dnf -y install $item
		error_check $? "Failed to install ${item}"
	done
}

confirm_and_execute () {
	msg=$1
	func=$2

	read -p "$msg (Yy/N): " confirm
	if [[ "$confirm" == [yY] || "$confirm" == [yY][eE][sS] ]]; then
		$func
	fi
}


confirm_and_execute "Enable faster dnf?" enable_faster_dnf
# confirm_and_execute "Enable RPM fusion repositories?" enable_rpm_fusion_repo
confirm_and_execute "Install media codecs?" install_media_codecs
confirm_and_execute "Change hostname?" change_hostname
confirm_and_execute "Install gnome-tweaks and gnome-extensions?" install_gnome_tweaks_extensions
confirm_and_execute "Install C/C++ Dev Tools and Libs?" install_c_cpp_dev_tools
confirm_and_execute "Install golang?" install_golang
confirm_and_execute "Install Brave Web Browser?" install_brave
confirm_and_execute "Install VSCode?" install_vscode
confirm_and_execute "Install command line editors?" install_command_line_editors

# alacritty
#
