# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

let

  stableWithOverlays = import <nixpkgs-stable> {
    overlays = [
      # Need to test the "fix" below. As the upstream builder takes around an hour to build, if we run an update in that period it'll cause a cache miss and we'll build it ourselves!
      # `sudo nix-channel --update` needed as well.
      (import (builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/master@{2%20hours%20ago}.tar.gz";
      }))

    ];

  };


  unstable = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
      config.allowUnfree = true;
    };

in {
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/cachix.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub = {
        configurationLimit = 3;
        default = "saved";
      };
    };
    blacklistedKernelModules = [
      "rtl8xxxu"
    ];
    kernelModules = [ "rtl8192eu" ];
    extraModulePackages = [
      config.boot.kernelPackages.rtl8192eu
    ];

  };

  networking.hostName = "office-desktop"; # Define your hostname.

  # Custom wifi drivers. For the little TP-Link dongle
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Africa/Johannesburg";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  # If you leave this on and don't have wifi plugged in, it'll take 1:30min to search for the device.
  # networking.interfaces.wlp0s20u5.useDHCP = true;

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
    open-sans
    corefonts
  ];

  # Map capslock => control key
  services.xserver.xkbOptions = "ctrl:nocaps";
  console.useXkbConfig = true;

  # For Flutter/Android
  # programs.adb.enable = true;

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.

  # Going with pipewire
  sound.enable = false;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire  = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    media-session.config.bluez-monitor.rules = [
      {
        # Matches all cards
        matches = [ { "device.name" = "~bluez_card.*"; } ];
        actions = {
          "update-props" = {
            # "bluez5.reconnect-profiles" = [ "hsp_hs" "hs_ag" "hfp_hf" ];
            "bluez5.headset-roles" = ["hsp_hs" "hsp_ag" "hfp_hf"];
            # mSBC is not expected to work on all headset + adapter combinations.
            "bluez5.msbc-support" = true;
            # SBC-XQ is not expected to work on all headset + adapter combinations.
            "bluez5.sbc-xq-support" = true;
          };
        };
      }
      {
        matches = [
          # Matches all sources
          { "node.name" = "~bluez_input.*"; }
          # Matches all outputs
          { "node.name" = "~bluez_output.*"; }
        ];
        actions = {
          "node.pause-on-idle" = false;
        };
      }
    ];
  };


  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  programs.fish = {
    # Needs direnv installed
    shellAliases = {
      "j" = "fasd_cd -d";
      "cs" = "git status || ls";
      "em" = "emacsclient -c -a ''";
    };
   enable = true;
   shellInit = ''
     fish_vi_key_bindings

     function fasd_cd -d "fasd builtin cd"
       if test (count $argv) -le 1
         command fasd "$argv"
       else
         fasd -e 'printf %s' $argv | read -l ret
         test -z "$ret"; and return
         test -d "$ret"; and cd "$ret"; or printf "%s\n" $ret
       end
     end

    direnv hook fish | source
'';
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.

  users.users.josh = {
    isNormalUser = true;
    shell = pkgs.fish;
    home = "/home/josh";

    extraGroups = [ "wheel" "docker" "adbusers" ]; # 'wheel' enables ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "obsidian"
    "google-chrome"
    "slack"
    "skypeforlinux"
    "teams"
    "dropbox"
    "dropbox-cli"
    "corefonts"
    "zoom-us"
    "zoom"
    # WTF firefox??
    "firefox-bin"
    "firefox-release-bin-unwrapped"
  ];

  # Services
  virtualisation.docker.enable = true;

  # Lorri is a nix-shell replacement built on direnv.
  services.lorri.enable = true;

  # Emacs (probably need to add in the packages here at some point)
  services.emacs = {
   enable = true;
   package = stableWithOverlays.emacsPgtkGcc;
  };

  # Enable nix eval --expr
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command
   '';

  environment.systemPackages = with pkgs; [
    # Dev stuff
    curl wget vim git fasd jq sqlite unzip ripgrep xsel fd visidata bind zip ispell tldr gitAndTools.gh direnv fzf bat

    openvpn

    binutils gcc libgccjit

    noto-fonts open-sans corefonts

    # exfat exfatprogs nfs-utils
    ntfs3g # I think we only need ntfs3g to access USB drives with > 4gb files.

    # Audio
    rnnoise-plugin

    python3 gnumake pandoc ledger
    #pdflatex
    nodejs-14_x 

    firefox google-chrome inkscape qgis slack dropbox-cli zoom-us skypeforlinux teams obsidian
    blender
    deluge
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
    gnome.enable = true; # Gnome 4 in 21.05
  };
  displayManager = {
    # defaultSession = "none+i3";
    # Autologin seems to break GDM??
    # autoLogin = {
    #   enable = true;
    #   user = "josh";
    # };
    gdm = {
      enable = true;
      wayland = true;
    };
  };

  # i3 Window Manager
  windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    extraPackages = with pkgs; [
      dmenu #application launcher most people use
      # i3status # gives you the default i3 status bar
   ];
  };
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
