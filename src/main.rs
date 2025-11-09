#![cfg_attr(coverage_nightly, feature(coverage_attribute))]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

mod hilcode;

use std::fs::File;
use std::fs::Metadata;
use std::io::BufReader;
use std::io::Error;
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::time::SystemTime;

use clap::Parser;
use xxhash_rust::xxh3::Xxh3;

use crate::hilcode::config::app_config::AppConfig;
use crate::hilcode::config::cli::Cli;
use crate::hilcode::error::hepheastus_error::HepheastusError;
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
	for (index, path_buf) in files.iter().enumerate()
	{
		let metadata: Metadata = path_buf.metadata().unwrap();
		let modified: SystemTime = metadata.modified().unwrap();
		let file_size: u64 = metadata.size();
		let file_mode: u32 = metadata.mode();
		let file: File = File::open(path_buf).unwrap();
		let mut buf_reader: BufReader<File> = BufReader::new(file);
		let mut hasher: Xxh3 = Xxh3::new();
		std::io::copy(&mut buf_reader, &mut hasher).unwrap();
		let hash: u64 = hasher.digest();
		println!("{}: {:?} / {:?} {:#o} {:?} / {:?}", index, path_buf, &modified, file_mode, file_size, hash);
	}
	println!("Okay");
}

pub struct Hash(u64);

pub struct FileSize(u64);

pub struct FileMode(u32);

pub struct FileStat
{
	modified: SystemTime,
	file_size: FileSize,
	file_mode: FileMode,
	hash: Hash,
}

impl FileStat
{
	pub fn get(path_buf: &PathBuf) -> Result<FileStat, HepheastusError>
	{
		let metadata: Metadata = path_buf.metadata()?;
		let modified: SystemTime = metadata.modified()?;
		let file_size: FileSize = FileSize(metadata.size());
		let file_mode: FileMode = FileMode(metadata.mode());
		let hash: Hash = FileStat::get_hash(path_buf)?;
		let file_stat: FileStat = FileStat {
			modified,
			file_size,
			file_mode,
			hash,
		};
		Result::Ok(file_stat)
	}

	fn get_hash(path_buf: &PathBuf) -> Result<Hash, Error>
	{
		let file: File = File::open(path_buf)?;
		let mut buf_reader: BufReader<File> = BufReader::new(file);
		let mut hasher: Xxh3 = Xxh3::new();
		std::io::copy(&mut buf_reader, &mut hasher)?;
		Result::Ok(Hash(hasher.digest()))
	}
}
