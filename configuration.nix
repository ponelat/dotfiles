# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

let
  unstable = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
      config.allowUnfree = true;
      overlays = [
        (import (builtins.fetchTarball {
          url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        }))
      ];
    };

in {
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/cachix.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.blacklistedKernelModules = [
    "rtl8xxxu"
  ];
  boot.kernelModules = [ "rtl8192eu" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.rtl8192eu ];
  networking.hostName = "office-desktop"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Africa/Johannesburg";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlp0s20u5.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  fonts.fonts = with pkgs; [
    noto-fonts
  ];


  # Map capslock => control key
  services.xserver.xkbOptions = "ctrl:nocaps";
  console.useXkbConfig = true;

  # Enable the GNOME 3 Desktop Environment.


  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.josh = {
    isNormalUser = true;
    home = "/home/josh";
    extraGroups = [ "wheel" "docker" ]; # 'wheel' enables ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
	# "google-chrome"
  # ];

  # Docker
  virtualisation.docker.enable = true;

  # Enable nix eval --expr
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command
   '';

  environment.systemPackages = with pkgs; [
    curl wget vim git fasd jq sqlite unzip ripgrep xsel

    openvpn

    binutils gcc libgccjit
    unstable.emacsGcc

    noto-fonts

    unstable.gnomeExtensions.material-shell

    python3 gnumake
    nodejs-14_x unstable.yarn

    firefox google-chrome inkscape slack dropbox-cli zoom-us
  ];

  # Dropbox
  networking.firewall = {
    allowedTCPPorts = [ 17500 ];
    allowedUDPPorts = [ 17500 ];
  };
  systemd.user.services.dropbox = {
    description = "Dropbox";
    after = [ "xembedsniproxy.service" ];
    wants = [ "xembedsniproxy.service" ];
    wantedBy = [ "graphical-session.target" ];
    environment = {
      QT_PLUGIN_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtPluginPrefix;
      QML2_IMPORT_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtQmlPrefix;
    };
    serviceConfig = {
      ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
      ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
      KillMode = "control-group"; # upstream recommends process
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };

# Needed for i3
# environment.pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw
services.xserver = {
  # Enable the X11 windowing system.
  enable = true;
  desktopManager = {
    xterm.enable = false;
    gnome3.enable = true;
  };
  displayManager = {
    # defaultSession = "none+i3";
    gdm.enable = true;
  };

  # i3 Window Manager
  # windowManager.i3 = {
  #   enable = true;
  #   package = pkgs.i3-gaps;
  #   extraPackages = with pkgs; [
  #     dmenu #application launcher most people use
  #     i3status # gives you the default i3 status bar
  #     i3lock #default i3 screen locker
  #     i3blocks #if you are planning on using i3blocks over i3status
  #  ];
  # };
};

# Some programs need SUID wrappers, can be configured further or are
# started in user sessions.
# programs.mtr.enable = true;
# programs.gnupg.agent = {
#   enable = true;
#   enableSSHSupport = true;
# };

# List services that you want to enable:

# Enable the OpenSSH daemon.
# services.openssh.enable = true;

# Open ports in the firewall.
# networking.firewall.allowedTCPPorts = [ ... ];
# networking.firewall.allowedUDPPorts = [ ... ];
# Or disable the firewall altogether.
# networking.firewall.enable = false;

# This value determines the NixOS release from which the default
# settings for stateful data, like file locations and database versions
# on your system were taken. It‘s perfectly fine and recommended to leave
# this value at the release version of the first install of this system.
# Before changing this value read the documentation for this option
# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
system.stateVersion = "20.09"; # Did you read the comment?

}
