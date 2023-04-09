use std::path::PathBuf;
use fs_extra;

fn main() {
    let mut out_dir = PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
    out_dir.pop();
    out_dir.pop();
    out_dir.pop();
    println!("{:?}", &out_dir);
    fs_extra::copy_items(&vec![PathBuf::from("lib")], out_dir, &fs_extra::dir::CopyOptions::new().overwrite(true)).unwrap();
    println!("cargo:rerun-if-changed=lib");
}