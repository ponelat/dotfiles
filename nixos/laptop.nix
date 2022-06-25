# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# For Home Manager, we have the following
# See: https://nix-community.github.io/home-manager/index.html#sec-install-nixos-module


{ config, lib, pkgs, ... }:

let

pinned = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/6c4b9f1a2fd761e2d384ef86cff0d208ca27fdca.tar.gz") {
      overlays = [
        (import (builtins.fetchTarball {
          url =
            "https://github.com/nix-community/emacs-overlay/archive/ccf704241a96879f117b49490de1ba617defac25.tar.gz";
        }))
      ];
    };

  # stableWithOverlays = import <nixos> {
  #   overlays = [
  #     # Need to test the "fix" below. As the upstream builder takes around an hour to build, if we run an update in that period it'll cause a cache miss and we'll build it ourselves!
  #     # `sudo nix-channel --update` needed as well.
  #     (import (builtins.fetchTarball {
  #       url = "https://github.com/nix-community/emacs-overlay/archive/master@{2%20hours%20ago}.tar.gz";
  #     }))


  #   ];

  # };


  # unstable = import (fetchTarball
  #   "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
  #     overlays = [
  #       (self: super:
  #         {
  #           zoomUsFixed = unstable.zoom-us.overrideAttrs (old: {
  #             postFixup = old.postFixup + ''
  #             wrapProgram $out/bin/zoom-us --unset XDG_SESSION_TYPE
      
  #           '';});
  #           zoom = unstable.zoom-us.overrideAttrs (old: {
  #             postFixup = old.postFixup + ''
  #             wrapProgram $out/bin/zoom --unset XDG_SESSION_TYPE
  #           '';});
  #         }
  #       )
  #     ];
  #     config.allowUnfree = true;
  #   };

in {
  imports = [
      # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/cachix.nix

      # Home Manager
      <home-manager/nixos>  
   ];



  networking.hostName = "laptop-x1eg2"; # Define your hostname.
  networking.extraHosts =
    ''
    127.0.0.1 kafka.local
  '';

  # Custom wifi drivers. For the little TP-Link dongle
  
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Africa/Johannesburg";
  # time.timeZone = "Europe/Dublin";


  boot = {
    loader = {
      systemd-boot.enable = true;
      # Use the systemd-boot EFI boot loader.
      efi.canTouchEfiVariables = true;
      grub = {
        configurationLimit = 3;
        default = "saved";
      };
    };
    # blacklistedKernelModules = [
    #   "rtl8xxxu"
    # ];
    # kernelModules = [ "rtl8192eu" ];
    # extraModulePackages = [
    #   config.boot.kernelPackages.rtl8192eu
    # ];

    
    # This is the equivelant of mkinitcpi.conf, for adding a module to the initrd.
    initrd.kernelModules = [
      "battery" # Battery issue in x1eg2, see: https://wiki.archlinux.org/title/Lenovo_ThinkPad_X1_Extreme_(Gen_2)#Power_management
    ];

  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  # networking.useDHCP = false;
  # networking.interfaces.eno1.useDHCP = true;
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

  fonts.fonts = [
    pkgs.noto-fonts
    pkgs.open-sans
    pkgs.corefonts
    pkgs.font-awesome
  ];
  
  # Map capslock => control key
  # services.xserver.xkbOptions = "ctrl:nocaps";
  # console.useXkbConfig = true;
  services.interception-tools = {
    enable = true;
    udevmonConfig = ''
    - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
      DEVICE:
        NAME: "AT Translated Set 2 keyboard"
        EVENTS:
          EV_KEY: [KEY_CAPSLOCK]
    '';
    
  };

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
  hardware.bluetooth.enable = true;
  security.rtkit.enable = true;
  services.blueman.enable = true;
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
            "bluez5.headset-roles" = ["sbc-xq" "hsp_hs" "hsp_ag" "hfp_hf"];
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


  # Define a user account. Don't forget to set a password with ‘passwd’.

  users.users.josh = {
    isNormalUser = true;
    shell = pkgs.fish;
    home = "/home/josh";
    extraGroups = [ "wheel" "docker" "adbusers" ]; # 'wheel' enables ‘sudo’ for the user.
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  home-manager.users.josh = { pkgs, ... }: {

    home.packages = with pkgs; [
      rnix-lsp
      node2nix
    ];

 # xdg.configFile."i3blocks/config".source = ./i3blocks.conf;
 #
 #  home.file.".gdbinit".text = ''
 #      set auto-load safe-path /nix/store
 #  '';

    home.file = { 
      # ".josh/test.2" = {
      #   source = ./inkscape;
      #   recursive = true;
      # };
    };

    xdg.configFile = {

      # "inkscape" = {
      #   source = ./inkscape;
      #   recursive = true;
      # };

      # "test/josh.txt" = {
      #   text = ''
      #       Hello!
      #       This is cool.
      #     '';
      # };

      "mako/config" = {
        text = ''
        default-timeout=5000
        '';
        
      };

    };

        # epkgs.emacs28Packages.pdf-tools

    # Emacs (probably need to add in the packages here at some point)
    programs.emacs = {
      enable = true;
      package = pinned.emacsPgtkGcc;
      extraPackages = epkgs: [
        epkgs.pdf-tools
      ];
    };

    services.emacs = {
      enable = true;
      package = pinned.emacsPgtkGcc;
      defaultEditor = true;
    };

    # programs.emacs = {
    #   enable = true;
    #   package = pinned.emacsPgtkGcc;
    # };


    # systemd.user.services.emacs.serviceConfig.TimeoutStartSec = "20min";


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
  };
    

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # nixpkgs.config.allowUnfree = true;
  nixpkgs = {
    # overlays = [];
    overlays = [
      (self: super:
        {
          zoomUsFixed = pkgs.zoom-us.overrideAttrs (old: {
            postFixup = old.postFixup + ''
              wrapProgram $out/bin/zoom-us --unset XDG_SESSION_TYPE
              wrapProgram $out/bin/zoom --unset XDG_SESSION_TYPE
            '';});
          # zoom = pkgs.zoom-us.overrideAttrs (old: {
          #   postFixup = old.postFixup + ''
          #     wrapProgram $out/bin/zoom --unset XDG_SESSION_TYPE
          #   '';});
        }
      )
    ];

    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "obsidian"
      "google-chrome"
      "slack"
      "skypeforlinux"
      "teams"
      "dropbox"
      "dropbox-cli"
      "corefonts"
      "zoom"
      "zoom-us"
      "zoomUsFixed"
      # WTF firefox??
      "firefox-bin"
      "firefox-release-bin-unwrapped"
    ];
  };

  # Services
  virtualisation.docker.enable = true;

  # Lorri is a nix-shell replacement built on direnv.
  services.lorri.enable = true;

  # Enable nix eval --expr
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command
   '';

  environment.systemPackages = [
    # Dev stuff
    pkgs.curl pkgs.wget pkgs.vim pkgs.git pkgs.fasd pkgs.jq pkgs.sqlite pkgs.unzip pkgs.ripgrep pkgs.xsel pkgs.fd pkgs.visidata pkgs.bind pkgs.zip pkgs.ispell pkgs.tldr pkgs.gitAndTools.gh pkgs.direnv pkgs.fzf pkgs.bat pkgs.file pkgs.gnupg pkgs.tmux pkgs.killall

    pkgs.psmisc # for 'fuser -k 3000/tcp'

    pkgs.leiningen # For emacs

    pkgs.openvpn

    pkgs.binutils pkgs.gcc pkgs.libgccjit

    pkgs.noto-fonts pkgs.open-sans pkgs.corefonts

    pkgs.exfat pkgs.exfatprogs pkgs.nfs-utils pkgs.ntfs3g # I think we only need ntfs3g to access USB drives with > 4gb files.

    # Audio
    pkgs.rnnoise-plugin

    pkgs.python3 pkgs.gnumake pkgs.pandoc pkgs.ledger
    #pdflatex
    pkgs.nodejs-16_x 
    pkgs.nodePackages.typescript-language-server
    pkgs.nodePackages.typescript
    pkgs.jdk
    pkgs.jdt-language-server

    pkgs.pulseaudio

    pkgs.firefox pkgs.google-chrome pkgs.inkscape pkgs.slack pkgs.dropbox-cli pkgs.skypeforlinux pkgs.teams pkgs.obsidian
    pkgs.deluge
    pkgs.obs-studio
    pkgs.vlc

    # Unstable 
    # unstable.gnomeExtensions.pop-shell
    pkgs.zoomUsFixed
    # pkgs.zoom
  ];

  
# Needed for i3
# environment.pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw
services.xserver = {
#   # Enable the X11 windowing system.

  # Enable touchpad support (enabled default in most desktopManager).
  # libinput.enable = true;

  enable = true;
  desktopManager = {
    xterm.enable = false;
    gnome.enable = true; # Gnome 4 in 21.05
  };
  displayManager = {
    defaultSession = "sway";
    gdm.enable = true;
    # sddm.enable = true;
    gdm.wayland = true;
  };

#   # i3 Window Manager
#   # windowManager.i3 = {
#   #   enable = false;
#   #   package = pkgs.i3-gaps;
#   #   extraPackages = [
#   #     pkgs.dmenu #application launcher most people use
#   #     # i3status # gives you the default i3 status bar
#   #  ];
#   # };

};

# pathsToLink is needed by polkit_gnome
environment.pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw
# programs.gnome.enable = true;
xdg.portal = {
  enable = true;
  gtkUsePortal = true; 
};

programs.sway = {
  enable = true;
  wrapperFeatures.gtk = true; # so that gtk works properly
  extraPackages = [

    pkgs.swaylock
    pkgs.swayidle

    # Bar
    pkgs.waybar

    # SSH
    pkgs.polkit_gnome
    pkgs.openssh 

    pkgs.wl-clipboard
    pkgs.mako # notification daemon
    pkgs.alacritty # Alacritty is the default terminal in the config
    # pkgs.dmenu # Dmenu is the default in the config but i recommend wofi since its wayland native
    pkgs.wofi

    # Sound / Audio
    pkgs.pavucontrol
    pkgs.pamixer
    pkgs.brightnessctl

    # Screenshots
    pkgs.slurp
    pkgs.grim

    # GTK Themeing
    pkgs.gtk-engine-murrine
    pkgs.gtk_engines
    pkgs.gsettings-desktop-schemas
    pkgs.lxappearance
  ];
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

programs.ssh = {
  # askPassword = true;
  startAgent = true;
};

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
system.stateVersion = "21.11"; # Did you read the comment?

}
