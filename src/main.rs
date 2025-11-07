#![cfg_attr(coverage_nightly, feature(coverage_attribute))]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

mod hilcode;

use std::fs::File;
use std::fs::Metadata;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use clap::Parser;
use memmap3::Mmap;
use xxhash_rust::xxh3::xxh3_64;

use crate::hilcode::config::app_config::AppConfig;
use crate::hilcode::config::cli::Cli;
use crate::hilcode::io::file_set::FileSet;
use crate::hilcode::io::file_set_glob::FileSetGlob;

fn main()
{
	let arguments: Vec<String> = std::env::args().collect();
	let cli: Cli = Cli::parse_from(arguments);
	let root_directory: PathBuf = PathBuf::new().join(".");
	let app_config: Rc<AppConfig> = AppConfig::new(&root_directory, &cli);
	println!("{:?}", app_config);
	let base_directory: PathBuf = Path::new("../teng").to_path_buf();
	let file_set_glob: FileSetGlob =
		FileSetGlob::new(&base_directory, vec!["**/*.rs".into(), "!.git/**".into(), "!target/**".into()]);
	let files: FileSet = file_set_glob.search("rust-sources").unwrap();
	for (index, file) in files.iter().enumerate()
	{
		let metadata: Metadata = file.metadata().unwrap();
		let file: File = File::open(file).unwrap();
		let mmap: Mmap = unsafe { Mmap::map(&file).unwrap() };
		let hash: u64 = xxh3_64(&mmap);
		println!("{}: {:?} / {:?} / {:?}", index, file, &metadata, hash);
	}
	println!("Okay");
}
