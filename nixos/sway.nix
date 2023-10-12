

{ config, lib, pkgs, ... }:

let

  unstable = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
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



in  {

  services.xserver = {
    # Sway crashes with nvidia drivers. Try fix when possible.
    # videoDrivers = [ "nvidia" ];
    displayManager.defaultSession = "sway";
  };

  home-manager.users.josh = { pkgs, ... }: {
    xdg.configFile."sway" = {
      source = "/home/josh/projects/dotfiles/dots/config/sway";
    };
  };


  # To help with Sway
  services.dbus.enable = true;


  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true; # so that gtk works properly
    package = unstable.sway; 

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
  
}
