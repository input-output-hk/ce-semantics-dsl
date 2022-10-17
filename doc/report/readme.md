## Paper

You can generate the PDF using:

```shell
latexmk -pdfxe -xelatex="xelatex --shell-escape %O %S" report
```

or, if you have [`cargo`](https://doc.rust-lang.org/cargo/):

```shell
cargo build
```

or, if you have [`just`](https://github.com/casey/just):

```shell
just
```

Note that the `just` recipe assumes `cargo`.

We assume a full `texlive` distribution, `latexmk` and the `pygments` python library, so on Ubuntu the dependencies can be installed using:

```shell
sudo apt install texlive-full latexmk python3-pygments
```
