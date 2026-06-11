{
  description = "CLASP Common Lisp CI environment";

  inputs = {
    # Pinned to a stable NixOS channel rather than nixpkgs-unstable.
    # To update: nix flake update (regenerates flake.lock)
    # flake.lock should always be committed so CI is fully reproducible.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          # n.b., clasp-common-lisp is present in nixpkgs but has had
          # intermittent build issues historically. If nix develop fails
          # on this package, check https://github.com/NixOS/nixpkgs/issues
          # and consider temporarily pinning nixpkgs to a known-good commit.
          pkgs.clasp-common-lisp

          # SBCL and OpenSSL are required to build qlot from its tar.gz.
          # Qlot does not ship pre-built binaries — it compiles itself via
          # scripts/setup.sh using SBCL + OpenSSL. The workflow step below
          # handles the download (with hash verification) and build.
          pkgs.sbcl
          pkgs.openssl

          pkgs.gnumake
          pkgs.git
          pkgs.curl
          pkgs.cacert
        ];

        # Explicitly expose OpenSSL shared libraries so CFFI can find libssl.so
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.openssl ];
      };
    };
}
