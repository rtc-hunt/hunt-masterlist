{ pkgs ? import <nixpkgs> {} }:
let
    new-pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable";
    # Commit hash for nixos-unstable as of 2018-09-12
    url = "https://github.com/NixOS/nixpkgs/archive/b67e752c29f18a0ca5534a07661366d6a2c2e649.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1n47f7r8cm9pcsz7vl4nxjfvs0fgzvcmjda5h0inz3yx9vghp5xm";
    }) {};

    nodejs = new-pkgs.nodejs-12_x;

    nodePackages = pkgs.nodePackages.override {
      inherit nodejs;
    };
in new-pkgs.mkShell {
  buildInputs = [
    nodejs
    nodePackages.node2nix
  ];
}
