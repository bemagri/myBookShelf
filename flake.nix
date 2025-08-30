{
  description = "Dev shell for myBookShelf (Lazarus/FPC)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib  = pkgs.lib;

        lazGtk =
          if pkgs ? lazarus-gtk2 then pkgs.lazarus-gtk2
          else if pkgs ? lazarus then pkgs.lazarus
          else pkgs.lazarus-qt5;

        lazQt5 =
          if pkgs ? lazarus-qt5 then pkgs.lazarus-qt5
          else if pkgs ? lazarus then pkgs.lazarus
          else lazGtk;

        fpcSrcOpt = if pkgs ? fpcSrc then pkgs.fpcSrc else null;

        gtkLibPath = lib.makeLibraryPath [
          pkgs.gtk2 pkgs.glib pkgs.pango pkgs.cairo pkgs.gdk-pixbuf pkgs.atk
          pkgs.xorg.libX11 pkgs.xorg.libXext pkgs.xorg.libXrender pkgs.xorg.libXrandr
          pkgs.xorg.libXinerama pkgs.xorg.libXcursor pkgs.xorg.libXi pkgs.xorg.libXfixes
        ];
        qtLibPath  = lib.makeLibraryPath [ pkgs.qt5.qtbase pkgs.qt5.qtsvg pkgs.qt5.qtx11extras ];
        qtPlugins  = "${pkgs.qt5.qtbase}/lib/qt-5/plugins";

        commonInputs = [
          pkgs.fpc
          lazGtk
          pkgs.poppler_utils
          pkgs.openssl
          pkgs.cacert
          pkgs.gdb
          pkgs.pkg-config
          pkgs.gtk2
        ] ++ lib.optionals (fpcSrcOpt != null) [ fpcSrcOpt ];

        commonHook = ''
          : "${LCLWidgetType:=gtk2}"
          export LCLWidgetType
          export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          export NIX_SSL_CERT_FILE=$SSL_CERT_FILE

          # Find Lazarus IDE binary and expose a 'lazarus' helper
          LAZ_BIN=""
          for b in lazarus lazarus-ide startlazarus; do
            if command -v "$b" >/dev/null 2>&1; then LAZ_BIN="$b"; break; fi
          done
          lazarus () { "$LAZ_BIN" "$@"; }
          export -f lazarus

          # Prefer pkgs.fpcSrc if available; else probe inside pkgs.fpc for sources
          if [ -d "${if fpcSrcOpt != null then fpcSrcOpt else ""}" ]; then
            export FPCSRC="${if fpcSrcOpt != null then fpcSrcOpt else ""}"
          else
            FPC_OUT=${pkgs.fpc}
            for cand in "$FPC_OUT/share/fpcsrc" "$FPC_OUT/lib/fpc"/*/source "$FPC_OUT/share/fpc/source"; do
              [ -d "$cand" ] && export FPCSRC="$cand" && break
            done
          fi

          echo
          echo "myBookShelf dev shell"
          echo "  • Widgetset:      $LCLWidgetType"
          [ -n "$LAZ_BIN" ] && echo "  • Lazarus:        $LAZ_BIN   (helper: 'lazarus')"
          [ -n "$FPCSRC" ] && echo "  • FPC source dir: $FPCSRC" || echo "  • FPC sources:    not found (OK for building)"
          echo "Commands:"
          echo "  lazbuild --ws=\$LCLWidgetType src/myBookShelf.lpi"
          echo "  lazarus &"
          echo
        '';
      in {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = commonInputs;
            shellHook = ''
              # GTK2 runtime libs so the app runs *inside* the shell
              export LD_LIBRARY_PATH="${gtkLibPath}:${pkgs.openssl.out}/lib:$LD_LIBRARY_PATH"
${commonHook}
            '';
          };

          qt5 = pkgs.mkShell {
            buildInputs = commonInputs ++ [ lazQt5 pkgs.qt5.qtbase pkgs.qt5.qttools pkgs.libqt5pas ];
            shellHook = ''
              export LCLWidgetType=qt5
              # Qt runtime libs + plugin search path
              export FPCOPT="$FPCOPT -Fl${pkgs.libqt5pas}/lib"
              export LD_LIBRARY_PATH="${qtLibPath}:${pkgs.libqt5pas}/lib:${pkgs.openssl.out}/lib:$LD_LIBRARY_PATH"
              export QT_PLUGIN_PATH="${qtPlugins}"

${commonHook}
            '';
          };
        };
      });
}
