#![cfg_attr(coverage_nightly, feature(coverage_attribute))]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

mod hilcode;

use std::fmt::Debug;
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
		let file_stat: Result<FileStat, HepheastusError> = FileStat::get(path_buf);
		println!("{}: {:?} / {:?}", index, path_buf, &file_stat);
	}
	println!("Okay");
}

pub struct Hash(u64);

impl Debug for Hash
{
	fn fmt(
		&self,
		formatter: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result
	{
		formatter.write_fmt(format_args!("{:17x}", self.0))
	}
}

pub struct FileSize(u64);

impl Debug for FileSize
{
	fn fmt(
		&self,
		formatter: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result
	{
		formatter.write_fmt(format_args!("{}B", self.0))
	}
}

pub struct FileMode(u32);

impl Debug for FileMode
{
	fn fmt(
		&self,
		formatter: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result
	{
		let permissions: [u8; 9] = [
			if self.0 & 0o100 == 0 { b'-' } else { b'r' },
			if self.0 & 0o010 == 0 { b'-' } else { b'w' },
			if self.0 & 0o001 == 0 { b'-' } else { b'x' },
			if self.0 & 0o000100 == 0 { b'-' } else { b'r' },
			if self.0 & 0o000010 == 0 { b'-' } else { b'w' },
			if self.0 & 0o000001 == 0 { b'-' } else { b'x' },
			if self.0 & 0o000000100 == 0 { b'-' } else { b'r' },
			if self.0 & 0o000000010 == 0 { b'-' } else { b'w' },
			if self.0 & 0o000000001 == 0 { b'-' } else { b'x' },
		];
		let permissions: &str = unsafe { std::str::from_utf8_unchecked(&permissions) };
		formatter.write_fmt(format_args!("{}", permissions))
	}
}

#[derive(Debug)]
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
