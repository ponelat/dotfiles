# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# For Home Manager, we have the following
# See: https://nix-community.github.io/home-manager/index.html#sec-install-nixos-module


{ config, lib, pkgs, ... }:

let

in {


  # For mDNS (discoverying .local domains on the LAN)
  services.avahi = {
    enable = true;
    nssmdns4 = true;       # enables `.local` lookup
    publish.enable = true;
    publish.userServices = true;
    ipv6 = false;
    hostName = "leno";
    allowInterfaces = [ "wlp82s0" ]; # Avoid connecting the docker (and other) interfaces. 
  };

 # 1. Enable the service and the firewall
  # services.tailscale.enable = true;

  # networking  = {
  #   hostName = "leno";
  #   # Modern 
  #   nftables.enable = true;

  #   firewall = {
  #     enable = true;
  #     # Always allow traffic from your Tailscale network
  #     trustedInterfaces = [ "tailscale0" ];
  #     # Allow the Tailscale UDP port through the firewall
  #     allowedUDPPorts = [
  #       config.services.tailscale.port
  #       5353 # for mDNS multicast
  #     ];
  #   };

  # };

  # Force tailscaled to use nftables (Critical for clean nftables-only systems)
  # This avoids the "iptables-compat" translation layer issues.
  # systemd.services.tailscaled.serviceConfig.Environment = [ 
  #   "TS_DEBUG_FIREWALL_MODE=nftables" 
  # ];

  # 3. Optimization: Prevent systemd from waiting for network online 
  # (Optional but recommended for faster boot with VPNs)
  systemd.network.wait-online.enable = false; 
  boot.initrd.systemd.network.wait-online.enable = false;

  networking.extraHosts =
    ''
    127.0.0.1 kafka.local
    127.0.0.1 leno.local
  '';

  imports = [
      # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      /home/josh/projects/dotfiles/nixos/common.nix
   ];

  # Custom wifi drivers. For the little TP-Link dongle
  
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  # services.automatic-timezoned.enable = true;
  # time.timeZone = "Africa/Johannesburg";
  # time.timeZone = "Canada/Atlantic";
  # time.timeZone = "Canada/Pacific";
  # time.timeZone = "Europe/Dublin";
  # time.timeZone = "America/Los_Angeles";


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


  # Going with pipewire
  # sound.enable = false;
  services.pulseaudio.enable = false;
  hardware.bluetooth.enable = true;
  security.rtkit.enable = true;

  # The rootCA.pem needs to be created with `JAVA_HOME= mkcert -install`
  # Chrome needs a .crt file, so convert with:
  # $ openssl x509 -in /home/josh/.local/share/mkcert/rootCA.pem -out /home/josh/.local/share/mkcert/rootCA.crt
  # You should circulate the rootCA.pem to those devices you want to access your stuff.
  # To stamp out a cert, run `mkcert leno.local localhost 127.0.0.1 ::1`.
  # ... then use those files from your server (both key and pem)
  security.pki.certificateFiles = let
    caFiles = [
      # /home/josh/.local/share/mkcert/rootCA.pem # Note, literal files, not strings here.
      /home/josh/.local/share/mkcert/rootCA.crt
    ]; in
    builtins.filter builtins.pathExists caFiles;

  services.blueman.enable = true;
  services.pipewire  = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;

    # media-session.config.bluez-monitor.rules = [
    #   {
    #     # Matches all cards
    #     matches = [ { "device.name" = "~bluez_card.*"; } ];
    #     actions = {
    #       "update-props" = {
    #         # "bluez5.reconnect-profiles" = [ "hsp_hs" "hs_ag" "hfp_hf" ];
    #         "bluez5.headset-roles" = ["sbc-xq" "hsp_hs" "hsp_ag" "hfp_hf"];
    #         # mSBC is not expected to work on all headset + adapter combinations.
    #         "bluez5.msbc-support" = true;
    #         # SBC-XQ is not expected to work on all headset + adapter combinations.
    #         "bluez5.sbc-xq-support" = true;
    #       };
    #     };
    #   }
    #   {
    #     matches = [
    #       # Matches all sources
    #       { "node.name" = "~bluez_input.*"; }
    #       # Matches all outputs
    #       { "node.name" = "~bluez_output.*"; }
    #     ];
    #     actions = {
    #       "node.pause-on-idle" = false;
    #     };
    #   }
    # ];

  };

  # For NVIDIA
  # hardware.opengl.enable = true;
  # hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
  # hardware.nvidia.modesetting.enable = true;


  programs.fish.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.josh = {
    isNormalUser = true;
    shell = pkgs.fish;
    home = "/home/josh";
    extraGroups = [ "wheel" "docker" "adbusers" ]; # 'wheel' enables ‘sudo’ for the user.
  };




# Open ports in the firewall.
  # 5173 is the default vite port. I needed to expose my local app for my phone
# networking.firewall.allowedTCPPorts = [ 5173 ];
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
