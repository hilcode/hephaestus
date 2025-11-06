use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use tempfile::TempDir;
use tempfile::tempdir;

use crate::hilcode::config::app_config::AppConfig;
use crate::hilcode::log::level::LogLevel;

pub struct TestEnv
{
	app_config: Rc<AppConfig>,
	temp_dir: TempDir,
	root_directory: PathBuf,
}

impl Default for TestEnv
{
	fn default() -> Self
	{
		let temp_dir: TempDir = tempdir().unwrap();
		let root_directory: PathBuf = temp_dir.path().to_path_buf();
		TestEnv::new(temp_dir, &root_directory)
	}
}

#[cfg_attr(coverage_nightly, coverage(off))]
impl TestEnv
{
	pub fn with_root_directory(root_directory: &Path) -> TestEnv
	{
		let temp_dir: TempDir = tempdir().unwrap();
		TestEnv::new(temp_dir, root_directory)
	}

	fn new(
		temp_dir: TempDir,
		root_directory: &Path,
	) -> TestEnv
	{
		let root_directory: PathBuf = root_directory.to_path_buf();
		let app_config: Rc<AppConfig> = AppConfig::builder().with_log_level(LogLevel::Trace).build();
		TestEnv {
			app_config,
			temp_dir,
			root_directory,
		}
	}

	pub fn root_directory(&self) -> &Path
	{
		&self.root_directory
	}
}
