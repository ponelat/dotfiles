# Common configuration across the different machines.
# Most of the stuff should be in here,
# except for hardware specific.

# For Home Manager, we have the following
# See: https://nix-community.github.io/home-manager/index.html#sec-install-nixos-module


{ config, lib, pkgs, ... }:

let

  # pinned = import (fetchTarball
  #   "https://github.com/NixOS/nixpkgs/archive/24.05.tar.gz") {
  #     overlays = [
  #       (import (builtins.fetchTarball {
  #         url =
  #           "https://github.com/nix-community/emacs-overlay/archive/ccf704241a96879f117b49490de1ba617defac25.tar.gz";
  #       }))
  #     ];
  #   };

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


in {
  imports = [
    # Adding cache for nix-community (emacs)
    /etc/nixos/cachix.nix

    # Home Manager
    <home-manager/nixos>

    # Sway is complex enough to move out here. Ignoring for now.
    # /home/josh/projects/dotfiles/nixos/sway.nix


    # For flashing the Ergodox-EZ
    /home/josh/projects/dotfiles/nixos/zsa-keyboard-flashing.nix
  ];


  # Set your time zone. This is best in hardware specific. For when I want servers
  # time.timeZone = "Africa/Johannesburg";

  # This is to set nixos rtc to local time to help with dual booting windows and linux
  # Curtesy of Papa
  time.hardwareClockInLocalTime = true;


  # See: https://nixos.wiki/wiki/Flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  fonts.packages = [
    pkgs.noto-fonts
    pkgs.open-sans
    pkgs.corefonts
    pkgs.source-sans-pro
    pkgs.font-awesome
    pkgs.emacs-all-the-icons-fonts
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

  systemd.services.ulauncher = {
    # WantedBy=graphical-session.target
    wantedBy = [ "graphical-session.target" ];
    description = "Start the ulauncher";
    unitConfig = {
      Documentation = "https://ulauncher.io";
    };
    serviceConfig = {
    # BusName=io.ulauncher.Ulauncher
    # Type=dbus
      Type = "dbus";
      BusName="io.ulauncher.Ulauncher";
    # Restart=always
      Restart = "always";
      # RestartSec=1;
      RestartSec=1;
      # ExecStart=/usr/bin/ulauncher --hide-window
      ExecStart = ''${pkgs.ulauncher}/bin/ulauncher --hide-window'';
    };
  };

  # For Flutter/Android
  programs.adb.enable = true;

  # For running some script
  programs.java.enable = true;

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
    # stateVersion = "21.11";

    home.stateVersion = "21.11";
    home.packages = with pkgs; [
      # rnix-lsp
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


    # Dropbox (home-manager). Per https://nixos.wiki/wiki/Dropbox
    systemd.user.services.dropbox = {
      Unit = {
        Description = "Dropbox service";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = "${pkgs.dropbox}/bin/dropbox";
        Restart = "on-failure";
      };
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

      "fish/functions/fish_prompt.fish" = {
        source = "/home/josh/projects/dotfiles/dots/config/fish/functions/fish_prompt.fish";
      };

      "fish/conf.d/github-copilot-cli.fish" = {
        source = "/home/josh/projects/dotfiles/dots/config/fish/conf.d/github-copilot-cli.fish";
      };


      "mako/config" = {
        text = ''
        default-timeout=5000
        '';

      };

      "alacritty/alacritty.toml" = {
        text =''
            [colors.bright]
            black = "0x828bb8"
            blue = "0x82aaff"
            cyan = "0xb4f9f8"
            green = "0xc3e88d"
            magenta = "0xff966c"
            red = "0xff98a4"
            white = "0x5f8787"
            yellow = "0xffc777"

            [colors.cursor]
            cursor = "0x808080"
            text = "0x7f85a3"

            [colors.normal]
            black = "0x444a73"
            blue = "0x3e68d7"
            cyan = "0x86e1fc"
            green = "0x4fd6be"
            magenta = "0xfc7b7b"
            red = "0xff5370"
            white = "0xd0d0d0"
            yellow = "0xffc777"

            [colors.primary]
            background = "0x1e2030"
            foreground = "0x7f85a3"

            [shell]
            args = ["tmux", "new-session", "-A", "-s", "general"]
            program = "/usr/bin/env"
        '';
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
        co = "checkout";
        di = "diff";
      };
    };

    # Emacs (probably need to add in the packages here at some point)
    programs.emacs = {
      enable = true;
      package = with pkgs; (
        (emacsPackagesFor emacs29).emacsWithPackages (
          epkgs: [
            epkgs.vterm
            epkgs.pdf-tools
            epkgs.treesit-grammars.with-all-grammars
            # Consider: https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
            # pkgs.emacs-lsp-booster


            # Used in LSP mode
            # pkgs.nodePackages.typescript-language-server
            pkgs.nodePackages.typescript
            pkgs.elixir
            pkgs.elixir-ls
            pkgs.rust-analyzer
            pkgs.python311Packages.python-lsp-server
            # pkgs.rustup
            pkgs.nodePackages.bash-language-server
          ]
        )
      );
    };

    # services.emacs = {
    #   enable = true;
    #   package = pinned.emacsPgtkGcc;
    #   defaultEditor = true;
    # };

    programs.fish = {
      enable = true;

      # Needs direnv installed
      shellAliases = {
            # "j" = "fasd_cd -d";
        "cs" = "git status || ls";
        "em" = "emacsclient -c -a ''";
        # Moved the following into shellInit as a function
        # ",," = "nix-shell -p";
      };

      interactiveShellInit = ''
        # See:  https://github.com/haslersn/any-nix-shell#fish-1
        # It's for using the same shell when invoking `nix-shell` or `nix-run`
        any-nix-shell fish --info-right | source
      '';

      shellInit = ''
        fish_vi_key_bindings
        set -g fish_escape_delay_ms 10

        # \v = ctrl-k and forward-char will drive foward, completing the suggestion.
        bind -M insert \v forward-char

        # Some clipboard stuff. Grabbed from https://github.com/fish-shell/fish-shell/issues/3299
        function fish_user_key_bindings
            bind yy fish_clipboard_copy
            bind Y fish_clipboard_copy
            bind p fish_clipboard_paste
            bind -M default k up-or-search 
            bind -M default j down-or-search 
            # bind -k nul accept-autosuggestion
            # bind -M default \$ end-of-line accept-autosuggestion
        end


        function ,,
            if test (count $argv) -gt 0
                nix-shell -p $argv
            else
                nix-shell 
            end
        end

        function ,,,
            nix-shell -I nixpkgs=channel:nixpkgs-unstable -p $argv
        end

        # function fasd_cd -d "fasd builtin cd"
        # if test (count $argv) -le 1
            # command fasd "$argv"
        # else
            # fasd -e 'printf %s' $argv | read -l ret
            # test -z "$ret"; and return
            # test -d "$ret"; and cd "$ret"; or printf "%s\n" $ret
        # end
        # end

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
    # Required by (older versions of)obsidian
    # config.permittedInsecurePackages = [
    #   "electron-25.9.0"
    # ];

    #   config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    #     "obsidian"
    #     "spotify"
    # "google-chrome"
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

  virtualisation.virtualbox = {
    host = {
      enable = true; 
      # enableExtensionPack = true;
    };
    # guest.enable = true;
    # guest.x11 = true;
  };

  users.extraGroups.vboxusers.members = [ "josh" ];

  # Lorri is a nix-shell replacement built on direnv.
  # services.lorri.enable = true;

  # Enable nix eval --expr
  # nix.package = pkgs.nix;
  nix.extraOptions = ''
      experimental-features = nix-command
   '';

  # For Wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # Used for openvpnforticlient. See: https://github.com/adrienverge/openfortivpn/issues/1076#issuecomment-1777003887
  environment.etc."ppp/options".text = "ipcp-accept-remote";

  environment.systemPackages = [
    # Dev stuff
    pkgs.curl pkgs.wget pkgs.vim pkgs.git pkgs.fasd pkgs.zoxide pkgs.jq pkgs.sqlite pkgs.unzip pkgs.ripgrep pkgs.xsel pkgs.fd pkgs.visidata pkgs.bind pkgs.zip pkgs.ispell pkgs.tldr pkgs.gitAndTools.gh pkgs.direnv pkgs.fzf pkgs.bat pkgs.file pkgs.gnupg pkgs.tmux pkgs.killall

    # JSON view in cli, like 'less'
    pkgs.jless

    # XF86 window system
    pkgs.xclip

    # Screenshot. I just can't use it, so frustrating. 
    # pkgs.ksnip

    pkgs.psmisc # for 'fuser -k 3000/tcp'

    pkgs.any-nix-shell # For `, some-command`

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
    pkgs.nodejs-18_x
    pkgs.cargo
    unstable.bun
    unstable.deno
    unstable.github-copilot-cli

    # Why not..?
    pkgs.elixir

    # typescript-language-server doesn't play nicely with emacsWithPackages
    pkgs.nodePackages.typescript-language-server

    # pkgs.nodePackages.typescript
    pkgs.nodePackages.js-beautify
    pkgs.jdk
    pkgs.clojure
    pkgs.maven
    pkgs.jdt-language-server

    pkgs.babashka

    # Image and document tools
    pkgs.imagemagick pkgs.qpdf

    pkgs.pulseaudio

    # Termina

    pkgs.alacritty

    # pkgs.firefox
    pkgs.inkscape pkgs.slack
    # pkgs.dropbox-cli
    pkgs.onlyoffice-bin
    # pkgs.skypeforlinux
    # pkgs.teams
    unstable.obsidian
    unstable.google-chrome
    pkgs.deluge
    pkgs.obs-studio
    pkgs.vlc
    pkgs.ulauncher
    pkgs.vscode

    pkgs.steam-run # Great for running binaries that aren't NixOS friendly.

    # # Gnome Extensions
    # pkgs.gnomeExtensions.forge
    pkgs.gnome.gnome-tweaks

    # Unstable
    # unstable.gnomeExtensions.pop-shell
    # unstable.zoomUsFixed
    # pkgs.zoomUsFixed
    # unstable.zoom-us
    pkgs.zoom-us

  ];

  services.xserver = {
    enable = true;

    desktopManager = {
      xterm.enable = false;
      gnome.enable = true; # Gnome 4 in 21.05
    };

    displayManager = {
      gdm.enable = true;
      # sddm.enable = true;
      gdm.wayland = false;
    };

  };

  # pathsToLink is needed by polkit_gnome
  environment.pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw

  xdg = {
   # portal = {
    #   enable = true;
    #   extraPortals = with pkgs; [
    #     xdg-desktop-portal-wlr
    #     # xdg-desktop-portal-gtk
    #   ];

    #   # trace: warning: The option `xdg.portal.gtkUsePortal' defined in `/home/josh/projects/dotfiles/nixos/common.nix' has been deprecated. Setting the variable globally with `environment.sessionVariables' NixOS option can have unforeseen side-effects.
    #   # gtkUsePortal = true;
    # };

    mime = {
      enable = true;

      addedAssociations = {
        "application/x-mimearchive" = ["google-chrome.desktop"];
      };

      defaultApplications = {
        "application/x-mimearchive" = ["google-chrome.desktop"];
      };
    };

  };

  programs.ssh = {
    # askPassword = true;
    startAgent = true;
  };

}
