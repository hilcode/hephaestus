use std::path::PathBuf;

#[derive(Debug)]
pub struct FileSet
{
	name: String,
	files: Vec<PathBuf>,
}

impl FileSet
{
	pub fn new(
		name: impl Into<String>,
		files: impl Into<Vec<PathBuf>>,
	) -> FileSet
	{
		let name: String = name.into();
		let files: Vec<PathBuf> = files.into();
		FileSet { name, files }
	}

	pub fn iter(&self) -> impl Iterator<Item = &PathBuf>
	{
		self.files.iter()
	}
}
