{
  description = "Dev shell for myBookShelf (Lazarus/FPC)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05"; # feel free to bump to unstable if you prefer
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShells = {
          # Safe default: GTK2 widgetset
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.fpc
              pkgs.fpcSrc
              pkgs.lazarus
              pkgs.gtk2                 # widget toolkit for LCL
              pkgs.poppler_utils        # pdftoppm for PDF covers
              pkgs.openssl
              pkgs.cacert               # SSL certs for HTTPS
              pkgs.gdb                  # Lazarus debugger
              pkgs.pkg-config
            ];
            shellHook = ''
              export LCLWidgetType=gtk2
              export FPCSRC=${pkgs.fpcSrc}
              # Nix usually sets this, but make it explicit:
              export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
              export NIX_SSL_CERT_FILE=$SSL_CERT_FILE

              echo
              echo "myBookShelf dev shell (GTK2) ready!"
              echo "  • Widgetset:      $LCLWidgetType"
              echo "  • FPC source dir: $FPCSRC"
              echo "Commands:"
              echo "  lazarus &     # launch IDE (first run: point FPC sources to \$FPCSRC if asked)"
              echo "  lazbuild --ws=$LCLWidgetType path/to/project.lpi"
              echo
            '';
          };

          # Optional: Qt5 widgetset
          qt5 = pkgs.mkShell {
            buildInputs = [
              pkgs.fpc
              pkgs.fpcSrc
              pkgs.lazarus
              pkgs.qt5.qtbase
              pkgs.qt5.qttools
              pkgs.poppler_utils
              pkgs.openssl
              pkgs.cacert
              pkgs.gdb
              pkgs.pkg-config
            ];
            shellHook = ''
              export LCLWidgetType=qt5
              export FPCSRC=${pkgs.fpcSrc}
              export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
              export NIX_SSL_CERT_FILE=$SSL_CERT_FILE

              echo
              echo "myBookShelf dev shell (Qt5) ready!"
              echo "  • Widgetset:      $LCLWidgetType"
              echo "  • FPC source dir: $FPCSRC"
              echo "Commands:"
              echo "  lazarus &     # launch IDE"
              echo "  lazbuild --ws=$LCLWidgetType path/to/project.lpi"
              echo
            '';
          };
        };
      });
}
