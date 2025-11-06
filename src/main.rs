#![cfg_attr(coverage_nightly, feature(coverage_attribute))]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

mod hilcode;

use std::path::PathBuf;
use std::rc::Rc;

use clap::Parser;

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
	let file_set_glob: FileSetGlob =
		FileSetGlob::new("../teng".into(), vec!["**/*.rs".into(), "!.git/**".into(), "!target/**".into()]);
	let files: FileSet = file_set_glob.search().unwrap();
	println!("{:?}", &files);
	println!("Okay");
}
