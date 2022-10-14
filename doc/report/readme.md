## Paper

You can generate the PDF using:

```shell
latexmk -pdfxe -xelatex="xelatex --shell-escape %O %S" report
```

or, if you have `cargo`:

```shell
cargo build
```

We assume a full `texlive` distribution, `latexmk` and the `pygments` python library, so on Ubuntu the dependencies can be installed using:

```shell
sudo apt install texlive-full latexmk python3-pygments
```
