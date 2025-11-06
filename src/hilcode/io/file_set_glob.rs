use std::path::Path;
use std::path::PathBuf;

use globwalker::DirEntry;
use globwalker::GlobWalker;
use globwalker::GlobWalkerBuilder;
use globwalker::WalkError;

use crate::hilcode::error::hepheastus_error::HepheastusError;
use crate::hilcode::io::file_set::FileSet;

pub struct FileSetGlob
{
	base_directory: PathBuf,
	globs: Vec<String>,
}

impl FileSetGlob
{
	pub fn new(
		base_directory: &Path,
		globs: Vec<String>,
	) -> FileSetGlob
	{
		let base_directory: PathBuf = base_directory.to_path_buf();
		FileSetGlob { base_directory, globs }
	}

	pub fn search(&self) -> Result<FileSet, HepheastusError>
	{
		let globe_walker: GlobWalker = GlobWalkerBuilder::from_patterns(&self.base_directory, &self.globs).build()?;
		globe_walker
			.into_iter()
			.map(|dir_entry: Result<DirEntry, WalkError>| -> Result<PathBuf, WalkError> {
				dir_entry.map(|dir_entry: DirEntry| -> PathBuf { dir_entry.into_path() })
			})
			.collect::<Result<Vec<PathBuf>, WalkError>>()
			.map_err(HepheastusError::DirectoryWalkerError)
			.map(FileSet::new)
	}
}
