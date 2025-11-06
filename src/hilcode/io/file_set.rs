use std::path::PathBuf;

#[derive(Debug)]
pub struct FileSet(Vec<PathBuf>);

impl FileSet
{
	pub fn new(paths: Vec<PathBuf>) -> FileSet
	{
		FileSet(paths)
	}
}
