use std::process::exit;
use std::process::Command;

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=report.tex");
  println!("cargo:rerun-if-changed=report.pdf");
  println!("cargo:rerun-if-changed=report.bib");
  println!("cargo:rerun-if-changed=rules.pdf");

  let status = Command::new("latexmk")
    .arg("-pdfxe")
    .arg("-xelatex='xelatex --shell-escape %O %S'")
    .arg("report")
    .status()
    .expect("error compiling report");

  exit(status.code().unwrap_or(1))
}
