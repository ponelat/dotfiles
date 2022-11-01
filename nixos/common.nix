# Common configuration across the different machines.
# Most of the stuff should be in here,
# except for hardware specific.

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

  unstable = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
      overlays = [
        (self: super:
          {
            zoomUsFixed = unstable.zoom-us.overrideAttrs (old: {
              postFixup = old.postFixup + ''
              wrapProgram $out/bin/zoom-us --unset XDG_SESSION_TYPE

            '';});
            zoom = unstable.zoom-us.overrideAttrs (old: {
              postFixup = old.postFixup + ''
              wrapProgram $out/bin/zoom --unset XDG_SESSION_TYPE
            '';});
          }
        )
      ];
      config.allowUnfree = true;
    };

  # From: https://nixos.wiki/wiki/Sway
  # bash script to let dbus know about important env variables and
  # propogate them to relevent services run at the end of sway config
  # see
  # https://github.com/emersion/xdg-desktop-portal-wlr/wiki/"It-doesn't-work"-Troubleshooting-Checklist
  # note: this is pretty much the same as  /etc/sway/config.d/nixos.conf but also restarts
  # some user services to make sure they have the correct environment variables

  custom.dbus-sway-environment = pkgs.writeTextFile {
    name = "dbus-sway-environment";
    destination = "/bin/dbus-sway-environment";
    executable = true;

    text = ''
  dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway
  systemctl --user stop pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
  systemctl --user start pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
      '';
  };

  # From: https://nixos.wiki/wiki/Sway
  # currently, there is some friction between sway and gtk:
  # https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland
  # the suggested way to set gtk settings is with gsettings
  # for gsettings to work, we need to tell it where the schemas are
  # using the XDG_DATA_DIR environment variable
  # run at the end of sway config
  custom.configure-gtk = pkgs.writeTextFile {
      name = "configure-gtk";
      destination = "/bin/configure-gtk";
      executable = true;
      text = let
        schema = pkgs.gsettings-desktop-schemas;
        datadir = "${schema}/share/gsettings-schemas/${schema.name}";
      in ''
        export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
        gnome_schema=org.gnome.desktop.interface
        gsettings set $gnome_schema gtk-theme 'Dracula'
        '';
  };



in {
  imports = [
      # Include the results of the hardware scan.
      # /etc/nixos/hardware-configuration.nix
      /etc/nixos/cachix.nix

      # Home Manager
      <home-manager/nixos>
   ];


  # Set your time zone. This is best in hardware specific. For when I want servers
  # time.timeZone = "Africa/Johannesburg";
  # time.timeZone = "Europe/Dublin";
  # time.timeZone = "America/Los_Angeles";

  # See: https://nixos.wiki/wiki/Flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  fonts.fonts = [
    pkgs.noto-fonts
    pkgs.open-sans
    pkgs.corefonts
    pkgs.source-sans-pro
    pkgs.font-awesome
  ];

  # Map capslock => control key
  # services.xserver.xkbOptions = "ctrl:nocaps";
  # console.useXkbConfig = true;
      # NAME: "AT Translated Set 2 keyboard"
  services.interception-tools = {
    enable = true;
    # The "-m 1" is for 'minimal' mode, see: https://gitlab.com/interception/linux/plugins/caps2esc
    udevmonConfig = ''
    - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc -m 1 | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
      DEVICE:
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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # This is assumed to be in the calling nix configuration
  # users.users.josh = {
  #   isNormalUser = true;
  #   shell = pkgs.fish;
  #   home = "/home/josh";
  #   extraGroups = [ "wheel" "docker" "adbusers" ]; # 'wheel' enables ‘sudo’ for the user.
  # };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  home-manager.users.josh = { pkgs, ... }: {

    home.packages = with pkgs; [
      rnix-lsp
      node2nix
      playerctl
      spotify
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

      "alacritty/alacritty.yml" = {
        text =''
        colors:
            primary:
                background: '0x1e2030'
                foreground: '0x7f85a3'

            cursor:
                text:   '0x7f85a3'
                cursor: '0x808080'

            normal:
                black:   '0x444a73'
                red:     '0xff5370'
                green:   '0x4fd6be'
                yellow:  '0xffc777'
                blue:    '0x3e68d7'
                magenta: '0xfc7b7b'
                cyan:    '0x86e1fc'
                white:   '0xd0d0d0'

            bright:
                black:   '0x828bb8'
                red:     '0xff98a4'
                green:   '0xc3e88d'
                yellow:  '0xffc777'
                blue:    '0x82aaff'
                magenta: '0xff966c'
                cyan:    '0xb4f9f8'
                white:   '0x5f8787'
        '';
      };

      "sway" = {
        # recursive = true;
        source = "/home/josh/projects/dotfiles/dots/config/sway";
      };

    };

    programs.git = {
      enable = true;
      userName = "Josh Ponelat";
      userEmail = "jponelat@gmail.com";
      extraConfig = {
        push.autoSetupRemote = true; # git config --global --add --bool push.autoSetupRemote true
      };
      aliases = {
        aa = "add";
        cm = "commit -m";
      };
    };

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
        ",," = "nix-shell -p";
      };
      enable = true;

      interactiveShellInit = ''
        # See:  https://github.com/haslersn/any-nix-shell#fish-1
        # It's for using the same shell when invoking `nix-shell` or `nix-run`
        any-nix-shell fish --info-right | source
      '';

      shellInit = ''

        # Some clipboard stuff. Grabbed from https://github.com/fish-shell/fish-shell/issues/3299
        function fish_user_key_bindings
            bind yy fish_clipboard_copy
            bind Y fish_clipboard_copy
            bind p fish_clipboard_paste
            # bind -k nul accept-autosuggestion
            # bind -M default \$ end-of-line accept-autosuggestion
        end
        fish_vi_key_bindings
        set -g fish_escape_delay_ms 10

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

  # Darn it, allow all unfree!
  config.allowUnfree = true;
  #   config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
  #     "obsidian"
  #     "spotify"
  #     "google-chrome"
  #     "slack"
  #     "skypeforlinux"
  #     "teams"
  #     "dropbox"
  #     "dropbox-cli"
  #     "corefonts"
  #     "zoom"
  #     "zoom-us"
  #     "zoomUsFixed"
  #     # WTF firefox??
  #     "firefox-bin"
  #     "firefox-release-bin-unwrapped"

  #     # Nvidia
  #     "nvidia-x11"
  #     "nvidia-settings"
  #   ];

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

  # For Wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";


  environment.systemPackages = [
    # Dev stuff
    pkgs.curl pkgs.wget pkgs.vim pkgs.git pkgs.fasd pkgs.jq pkgs.sqlite pkgs.unzip pkgs.ripgrep pkgs.xsel pkgs.fd pkgs.visidata pkgs.bind pkgs.zip pkgs.ispell pkgs.tldr pkgs.gitAndTools.gh pkgs.direnv pkgs.fzf pkgs.bat pkgs.file pkgs.gnupg pkgs.tmux pkgs.killall

    pkgs.psmisc # for 'fuser -k 3000/tcp'

    pkgs.comma pkgs.any-nix-shell # For `, some-command`

    pkgs.leiningen # For emacs


    pkgs.shared-mime-info # For copy/pasting from fish shell

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
    pkgs.nodePackages.js-beautify
    pkgs.jdk
    pkgs.maven
    pkgs.jdt-language-server

    # Image and document tools
    pkgs.imagemagick pkgs.qpdf

    pkgs.pulseaudio

    pkgs.firefox pkgs.google-chrome pkgs.inkscape pkgs.slack pkgs.dropbox-cli pkgs.skypeforlinux pkgs.teams
    unstable.obsidian
    pkgs.deluge
    pkgs.obs-studio
    pkgs.vlc

    # Unstable
    # unstable.gnomeExtensions.pop-shell
    # unstable.zoomUsFixed
    pkgs.zoomUsFixed
    # pkgs.zoom


  ];


# Needed for i3
# environment.pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw



services.xserver = {
  enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # libinput.enable = true;

  # Sway crashes with nvidia drivers. Try fix when possible.
  # videoDrivers = [ "nvidia" ];

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

# To help with Sway
services.dbus.enable = true;

xdg = {
  portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr
      # xdg-desktop-portal-gtk
    ];
    gtkUsePortal = true;
  };
};

programs.sway = {
  enable = true;
  wrapperFeatures.gtk = true; # so that gtk works properly

  extraPackages = [

    # Custom
    custom.dbus-sway-environment # See 'let' above. This is custom. https://nixos.wiki/wiki/Sway
    custom.configure-gtk # See 'let' above. This is custom. https://nixos.wiki/wiki/Sway

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
    pkgs.ksnip

    # GTK Themeing
    pkgs.gtk-engine-murrine
    pkgs.gtk_engines
    pkgs.gsettings-desktop-schemas
    pkgs.lxappearance
    pkgs.dracula-theme
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

# This is assumed to exist in the calling nixos configuration file
# system.stateVersion = "21.11"; # Did you read the comment?

}
