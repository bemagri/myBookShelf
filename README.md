# myBookShelf
This is a Free Pascal/Lazarus project to keep all your ebooks (and pdfs) organized in an iBook like graphical interface.

![Screenshot](/screenshot/mybookshelf_screenshot2.png?raw=true "GTK Screenshot")

![Screenshot](/screenshot/mybookshelf_screenshot3.png?raw=true "GTK Screenshot")

## Dependencies

- Poppler utils: provides `pdftoppm` and `pdfinfo` used to extract PDF covers and metadata
- unzip: used to read EPUB metadata from the OPF file

On Nix, these are included via the flake. Use:

- `nix develop` to enter the dev shell (PATH includes poppler_utils and unzip)
- `nix run` to run the packaged app (wrapper sets PATH to include these tools)

On Debian/Ubuntu:

- `sudo apt-get install poppler-utils unzip`

On Fedora:

- `sudo dnf install poppler-utils unzip`

On Arch:

- `sudo pacman -S poppler unzip`
