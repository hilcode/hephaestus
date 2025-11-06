use std::cmp::min;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use time::format_description::BorrowedFormatItem;
use time::macros::format_description;

use crate::hilcode::config::cli::Cli;
use crate::hilcode::log::level::LogLevel;

#[derive(Clone, Debug, PartialEq)]
pub struct AppConfig
{
	root_directory: PathBuf,
	log_level: LogLevel,
	time_format: &'static [BorrowedFormatItem<'static>],
}

impl AppConfig
{
	pub fn root_directory(&self) -> &Path
	{
		&self.root_directory
	}

	pub fn log_level(&self) -> LogLevel
	{
		self.log_level
	}

	pub fn time_format(&self) -> &'static [BorrowedFormatItem<'static>]
	{
		self.time_format
	}
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
impl AppConfig
{
	pub fn builder() -> AppConfigBuilder
	{
		AppConfigBuilder::new()
	}
}

impl Default for AppConfig
{
	fn default() -> Self
	{
		let root_directory: PathBuf = PathBuf::new().join(".");
		AppConfig {
			root_directory,
			log_level: LogLevel::Info,
			time_format: make_time_format(),
		}
	}
}

impl AppConfig
{
	pub fn new(
		root_directory: &Path,
		cli: &Cli,
	) -> Rc<Self>
	{
		let root_directory: PathBuf = root_directory.to_path_buf();
		let app_config: Self = Self {
			root_directory,
			log_level: make_log_level(cli),
			time_format: make_time_format(),
		};
		Rc::new(app_config)
	}
}

fn make_log_level(cli: &Cli) -> LogLevel
{
	let mut log_level: LogLevel = LogLevel::Info;
	let mut quiet: u8 = cli.quiet;
	let mut verbose: u8 = cli.verbose;
	while quiet > 0 || verbose > 0
	{
		if quiet > 0 && verbose > 0
		{
			quiet -= min(cli.quiet, cli.verbose);
			verbose -= min(cli.quiet, cli.verbose);
		}
		else if quiet > 0
		{
			quiet -= 1;
			log_level = log_level.decrease();
		}
		else
		{
			verbose -= 1;
			log_level = log_level.increase();
		}
	}
	log_level
}

fn make_time_format() -> &'static [BorrowedFormatItem<'static>]
{
	format_description!("[year]-[month]-[day]T[hour]:[minute]:[second].[subsecond digits:3] ")
}

#[cfg(test)]
pub struct AppConfigBuilder
{
	root_directory: PathBuf,
	log_level: LogLevel,
	time_format: &'static [BorrowedFormatItem<'static>],
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
impl AppConfigBuilder
{
	pub fn new() -> Self
	{
		Self {
			root_directory: PathBuf::new().join("."),
			log_level: LogLevel::Trace,
			time_format: format_description!(""),
		}
	}

	pub fn with_root_directory(
		self,
		root_directory: &Path,
	) -> Self
	{
		let root_directory: PathBuf = root_directory.to_path_buf();
		Self { root_directory, ..self }
	}

	pub fn without_logging(self) -> Self
	{
		Self {
			log_level: LogLevel::Off,
			..self
		}
	}

	pub fn with_log_level(
		self,
		log_level: LogLevel,
	) -> Self
	{
		Self { log_level, ..self }
	}

	pub fn with_time_format(
		self,
		time_format: &'static [BorrowedFormatItem<'static>],
	) -> Self
	{
		Self { time_format, ..self }
	}

	pub fn build(self) -> Rc<AppConfig>
	{
		let app_config: AppConfig = AppConfig {
			root_directory: self.root_directory,
			log_level: self.log_level,
			time_format: self.time_format,
		};
		Rc::new(app_config)
	}
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests
{

	use std::path::Path;
	use std::path::PathBuf;
	use std::rc::Rc;

	use clap::Parser;
	use time::format_description::BorrowedFormatItem;

	use crate::hilcode::config::app_config::AppConfig;
	use crate::hilcode::config::app_config::make_time_format;
	use crate::hilcode::config::cli::Cli;
	use crate::hilcode::log::level::LogLevel;
	use crate::hilcode::test::env::TestEnv;

	#[test]
	fn default()
	{
		let arguments: Vec<String> = to_arguments(vec!["/usr/bin/heph"]);
		let cli: Cli = Cli::parse_from(&arguments);
		let root_directory: PathBuf = PathBuf::new().join(".");
		let app_config: Rc<AppConfig> = AppConfig::new(&root_directory, &cli);
		assert_eq!(app_config, Rc::new(AppConfig::default()));
	}

	#[test]
	fn log_level()
	{
		let log_level: LogLevel = LogLevel::Error;
		let app_config: Rc<AppConfig> = AppConfig::builder().with_log_level(log_level).build();
		let actual: LogLevel = app_config.log_level();
		let expected: LogLevel = log_level;
		assert_eq!(expected, actual);
	}

	#[test]
	fn time_format()
	{
		let app_config: AppConfig = AppConfig::default();
		let actual: &'static [BorrowedFormatItem<'static>] = app_config.time_format();
		let expected: &'static [BorrowedFormatItem<'static>] = make_time_format();
		assert_eq!(expected, actual);
	}

	#[test]
	fn with_verbose()
	{
		let test_env: TestEnv = TestEnv::default();
		let root_directory: &Path = test_env.root_directory();
		{
			let arguments: Vec<String> = to_arguments(vec!["/usr/bin/teng", "--verbose"]);
			let cli: Cli = Cli::parse_from(arguments);
			let app_config: Rc<AppConfig> = AppConfig::new(root_directory, &cli);
			assert_eq!(app_config.log_level, LogLevel::Debug);
		}
		{
			let arguments: Vec<String> = to_arguments(vec!["/usr/bin/teng", "-v"]);
			let cli: Cli = Cli::parse_from(arguments);
			let app_config: Rc<AppConfig> = AppConfig::new(root_directory, &cli);
			assert_eq!(app_config.log_level, LogLevel::Debug);
		}
		{
			let arguments: Vec<String> = to_arguments(vec!["/usr/bin/teng", "-vv"]);
			let cli: Cli = Cli::parse_from(arguments);
			let app_config: Rc<AppConfig> = AppConfig::new(root_directory, &cli);
			assert_eq!(app_config.log_level, LogLevel::Trace);
		}
	}

	#[test]
	fn with_quiet_and_verbose()
	{
		let test_env: TestEnv = TestEnv::default();
		let root_directory: &Path = test_env.root_directory();
		{
			let arguments: Vec<String> = to_arguments(vec!["/usr/bin/teng", "-vqq"]);
			let cli: Cli = Cli::parse_from(arguments);
			let app_config: Rc<AppConfig> = AppConfig::new(root_directory, &cli);
			assert_eq!(app_config.log_level, LogLevel::Warn);
		}
		{
			let arguments: Vec<String> = to_arguments(vec!["/usr/bin/teng", "-vqv"]);
			let cli: Cli = Cli::parse_from(arguments);
			let app_config: Rc<AppConfig> = AppConfig::new(root_directory, &cli);
			assert_eq!(app_config.log_level, LogLevel::Debug);
		}
	}

	#[test]
	fn with_quiet()
	{
		let test_env: TestEnv = TestEnv::default();
		let root_directory: &Path = test_env.root_directory();
		{
			let arguments: Vec<String> = to_arguments(vec!["/usr/bin/teng", "--quiet"]);
			let cli: Cli = Cli::parse_from(arguments);
			let app_config: Rc<AppConfig> = AppConfig::new(root_directory, &cli);
			assert_eq!(app_config.log_level, LogLevel::Warn);
		}
		{
			let arguments: Vec<String> = to_arguments(vec!["/usr/bin/teng", "-q"]);
			let cli: Cli = Cli::parse_from(arguments);
			let app_config: Rc<AppConfig> = AppConfig::new(root_directory, &cli);
			assert_eq!(app_config.log_level, LogLevel::Warn);
		}
		{
			let arguments: Vec<String> = to_arguments(vec!["/usr/bin/teng", "-qq"]);
			let cli: Cli = Cli::parse_from(arguments);
			let app_config: Rc<AppConfig> = AppConfig::new(root_directory, &cli);
			assert_eq!(app_config.log_level, LogLevel::Error);
		}
	}

	fn to_arguments(arguments: Vec<&'static str>) -> Vec<String>
	{
		arguments.iter().map(|string| string.to_string()).collect()
	}
}
