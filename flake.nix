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
        # Build the application using lazbuild and wrap for runtime libs
        myBookShelfPkg = pkgs.stdenv.mkDerivation rec {
          pname = "myBookShelf";
          version = "0.1.0";
          src = self;

          nativeBuildInputs = [
            lazGtk
            pkgs.fpc
            pkgs.pkg-config
            pkgs.makeWrapper
          ];

          # C/GTK libraries needed at link time (provides .pc files and libs)
          buildInputs = [
            pkgs.gtk2
            pkgs.glib
            pkgs.pango
            pkgs.cairo
            pkgs.gdk-pixbuf
            pkgs.atk
            pkgs.xorg.libX11
            pkgs.xorg.libXext
            pkgs.xorg.libXrender
            pkgs.xorg.libXrandr
            pkgs.xorg.libXinerama
            pkgs.xorg.libXcursor
            pkgs.xorg.libXi
            pkgs.xorg.libXfixes
          ];

          # Lazarus puts the binary next to the .lpr by default
          buildPhase = ''
            runHook preBuild
            export LAZARUS_DIR=${lazGtk}/share/lazarus
            export HOME=$TMPDIR
            mkdir -p "$HOME/.lazarus"
            echo "Using Lazarus at $LAZARUS_DIR"
            # Help the linker find GTK libs at build time
            export FPCOPT="$FPCOPT -k-L${gtkLibPath}"
            export PKG_CONFIG_PATH
            lazbuild --lazarusdir="$LAZARUS_DIR" --ws=gtk2 --build-all src/myBookShelf.lpi
          runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin
            # Try the usual locations for the produced binary
            if [ -x src/myBookShelf ]; then
              cp -v src/myBookShelf $out/bin/myBookShelf
            elif [ -x src/mybookshelf ]; then
              cp -v src/mybookshelf $out/bin/myBookShelf
            elif [ -x src/lib/*/myBookShelf ]; then
              cp -v src/lib/*/myBookShelf $out/bin/myBookShelf
            else
              echo "error: built binary not found" >&2
              ls -R src || true
              exit 1
            fi
            # Wrap with GTK2 runtime libraries and CA bundle for TLS
            wrapProgram $out/bin/myBookShelf \
              --set LCLWidgetType gtk2 \
              --set LD_LIBRARY_PATH "${gtkLibPath}:${pkgs.openssl.out}/lib" \
              --set SSL_CERT_FILE ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt \
              --set NIX_SSL_CERT_FILE ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
            runHook postInstall
          '';

          meta = with lib; {
            description = "Personal bookshelf manager (Lazarus/FPC)";
            license = licenses.mit;
            platforms = platforms.linux;
          };
        };
      in {
        packages.default = myBookShelfPkg;

        apps.default = {
          type = "app";
          program = "${myBookShelfPkg}/bin/myBookShelf";
        };

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
