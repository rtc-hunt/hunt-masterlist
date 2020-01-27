{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    config = { allowBroken = true; };
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }@args: {
  overrides = pkgs.lib.composeExtensions (import (hackGet ./dep/rhyolite) (args//{obelisk=obelisk;})).haskellOverrides
    (self: super: with pkgs.haskell.lib; {
      clay = dontCheck super.clay;
      # Your custom overrides go here.
    });
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
