{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    terms.security.acme.acceptTerms = true;
  }
}:
with obelisk;
let proj = project ./. ({ pkgs, hackGet, ... }@args: {
  overrides = pkgs.lib.composeExtensions
    (pkgs.callPackage (hackGet ./dep/rhyolite) args).haskellOverrides
    (self: super: with pkgs.haskell.lib; {
      lens-aeson = self.callHackage "lens-aeson" "1.2.2" {};
      reflex-dom-core = super.reflex-dom-core.overrideAttrs (attrs: {src = (hackGet ./dep/reflex-dom) + "/reflex-dom-core";});
      gogol = (self.callCabal2nix "gogol" ((hackGet ./dep/gogol) + "/lib/gogol") {});
      gogol-core = (self.callCabal2nix "gogol-core" ((hackGet ./dep/gogol) + "/lib/gogol-core") {});
      gogol-drive = (self.callCabal2nix "gogol-drive" ((hackGet ./dep/gogol) + "/lib/services/gogol-drive") {});
      gogol-sheets = (self.callCabal2nix "gogol-sheets" ((hackGet ./dep/gogol) + "/lib/services/gogol-sheets") {});
      # crypton = self.callHackageDirect { pkg = "crypton"; ver="0.34"; sha256 = pkgs.lib.fakeSha256;} { };
      crypton = self.callHackageDirect { pkg = "crypton"; ver="0.34"; sha256 = "sha256-dHvzmwq5l1dPZsp0sYFe9l8mXF/Ya5aFbkDg0ljEEKY="; } { };
      crypton-x509 = self.callHackageDirect { pkg = "crypton-x509"; ver="1.7.7"; sha256 = "sha256-1i9T0Z77lfTSycpKdt74RIfv0Ug4EK+dkutBSrlMbzs="; } { };
      crypton-x509-store = self.callHackageDirect { pkg = "crypton-x509-store"; ver="1.6.9"; sha256 = "sha256-npb60VvCLzh5LszJgBZyGt/IJw/dZL4arium2YhHnog="; } { };
      servant = self.callHackageDirect { pkg = "servant"; ver="0.20.2"; sha256 = "sha256-kKsQEfMfO7tGupW+CcyToBit+bmEa+TuK1NzZbd4AXA="; } { };
      base64 = doJailbreak (self.callHackageDirect { pkg = "base64"; ver="1.0"; sha256 = "sha256-YLQqQWZ5B7w0tNJGEmPNumONwX5vQPHelC12iVBGMFU"; } { });
      
      # beam-automigrate = doJailbreak (self.callCabal2nix "beam-automigrate" (hackGet ./dep/beam-automigrate) {});
      # websockets = self.callHackage "websockets" "0.12.6.0" {};
    });
  packages = {
    hunttools = hackGet ./dep/hunttools;
    hunttools-utils = (hackGet ./dep/hunttools) + "/hunttools-utils";
    hunttools-dicts-if = hackGet ./dep/hunttools-dicts-if;
    packed-dawg-big = hackGet ./dep/packed-dawg-big;
  };
  tools = pkgs: [
    proj.ghc.hunttools-utils
  #  proj.ghc.backend
  ];
  staticFiles = import ./static {inherit pkgs; };
  android.applicationId = "systems.obsidian.obelisk.examples.rhyolite";
  android.displayName = "Rhyolite Example App";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.rhyolite";
  ios.bundleName = "Rhyolite Example App";
  # __closureCompilerOptimizationLevel = null; # Set this to `null` to skip the closure-compiler step
});
in proj // {
  hunttoolsEnv = proj.ghc.ghcWithPackages (pkgs: with pkgs; [hunttools hunttools-dicts-if packed-dawg-big]);
}
