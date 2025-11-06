use std::cell::RefCell;
use std::rc::Rc;

use colored::ColoredString;
use colored::Colorize;
use time::OffsetDateTime;
use time::format_description::BorrowedFormatItem;

use crate::hilcode::config::app_config::AppConfig;
use crate::hilcode::log::level::LogLevel;
#[cfg(test)]
use crate::hilcode::test::utils::clear_escape_codes;

pub trait Logger: std::io::Write
{
	#[cfg(test)]
	fn log_messages(&self) -> Vec<String>;
}

pub trait HasLogger
{
	fn is_enabled(
		&self,
		log_level: LogLevel,
	) -> bool;

	fn stdout(
		&self,
		message: &str,
	);

	fn trace(
		&self,
		message: &str,
	);

	fn debug(
		&self,
		message: &str,
	);

	fn info(
		&self,
		message: &str,
	);

	fn warn(
		&self,
		message: &str,
	);

	fn error(
		&self,
		message: &str,
	);
}

fn get_local_time(time_format: &[BorrowedFormatItem<'_>]) -> ColoredString
{
	OffsetDateTime::now_local().unwrap().format(time_format).unwrap().cyan()
}

pub struct AppLogger(Rc<AppConfig>, Rc<RefCell<dyn Logger>>);

pub struct StdOut(std::io::Stdout);

#[cfg_attr(coverage_nightly, coverage(off))]
impl std::io::Write for StdOut
{
	fn write(
		&mut self,
		buf: &[u8],
	) -> std::io::Result<usize>
	{
		self.0.write(buf)
	}

	fn flush(&mut self) -> std::io::Result<()>
	{
		self.0.flush()
	}
}

impl Logger for StdOut
{
	#[cfg(test)]
	#[cfg_attr(coverage_nightly, coverage(off))]
	fn log_messages(&self) -> Vec<String>
	{
		Vec::new()
	}
}

impl AppLogger
{
	#[cfg(not(test))]
	pub fn new(app_config: Rc<AppConfig>) -> Rc<AppLogger>
	{
		Rc::new(AppLogger(app_config, Rc::new(RefCell::new(StdOut(std::io::stdout())))))
	}

	#[cfg(test)]
	pub fn new(app_config: Rc<AppConfig>) -> Rc<AppLogger>
	{
		let log_messages: Rc<RefCell<LogMessages>> = LogMessages::new();
		AppLogger::test_logger(app_config, log_messages)
	}
}

impl HasLogger for AppLogger
{
	fn is_enabled(
		&self,
		log_level: LogLevel,
	) -> bool
	{
		log_level.is_enabled(self.0.log_level())
	}

	fn stdout(
		&self,
		message: &str,
	)
	{
		write!(self.1.borrow_mut(), "{}", message).unwrap();
	}

	fn trace(
		&self,
		message: &str,
	)
	{
		let local_time: ColoredString = get_local_time(self.0.time_format());
		let message: ColoredString = message.white();
		let text: String = format!("{}{} {}\n", &local_time, "TRACE".normal(), message);
		write!(self.1.borrow_mut(), "{}", text).unwrap();
	}

	fn debug(
		&self,
		message: &str,
	)
	{
		let local_time: ColoredString = get_local_time(self.0.time_format());
		let message: ColoredString = message.white();
		let text: String = format!("{}{} {}\n", &local_time, "DEBUG".purple(), message);
		write!(self.1.borrow_mut(), "{}", text).unwrap();
	}

	fn info(
		&self,
		message: &str,
	)
	{
		let local_time: ColoredString = get_local_time(self.0.time_format());
		let message: ColoredString = message.white();
		let text: String = format!("{}{} {}\n", &local_time, "INFO ".cyan(), message);
		write!(self.1.borrow_mut(), "{}", text).unwrap();
	}

	fn warn(
		&self,
		message: &str,
	)
	{
		let local_time: ColoredString = get_local_time(self.0.time_format());
		let message: ColoredString = message.white();
		let text: String = format!("{}{} {}\n", &local_time, "WARN ".yellow(), message);
		write!(self.1.borrow_mut(), "{}", text).unwrap();
	}

	fn error(
		&self,
		message: &str,
	)
	{
		let local_time: ColoredString = get_local_time(self.0.time_format());
		let message: ColoredString = message.white();
		let text: String = format!("{}{} {}\n", &local_time, "ERROR".red(), message);
		write!(self.1.borrow_mut(), "{}", text).unwrap();
	}
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests
{

	use std::rc::Rc;

	use crate::hilcode::config::app_config::AppConfig;
	use crate::hilcode::log::has_logger::AppLogger;
	use crate::hilcode::log::has_logger::HasLogger;
	use crate::hilcode::log::level::LogLevel;

	#[test]
	fn stdout()
	{
		let app_config: Rc<AppConfig> = Rc::new(AppConfig::default());
		let app_logger: Rc<AppLogger> = AppLogger::new(app_config);
		app_logger.stdout("The message");
		let log_messages: Vec<String> = app_logger.log_messages();
		assert_eq!(1, log_messages.len());
		let expected: &str = "The message";
		let actual: &str = log_messages.get(0).unwrap();
		assert_eq!(expected, actual);
	}

	mod trace
	{

		use std::rc::Rc;

		use crate::hilcode::log::has_logger::AppLogger;
		use crate::hilcode::log::has_logger::tests::enable_logging;
		use crate::hilcode::log::has_logger::tests::no_logging;
		use crate::hilcode::log::level::LogLevel;
		use crate::hilcode::log::macros::log_trace;

		#[test]
		fn off()
		{
			let app_logger: Rc<AppLogger> = no_logging();
			log_trace!(app_logger, "the message");
			let log_messages: Vec<String> = app_logger.log_messages();
			assert_eq!(0, log_messages.len());
		}

		#[test]
		fn on()
		{
			let app_logger: Rc<AppLogger> = enable_logging(LogLevel::Trace);
			log_trace!(app_logger, "the message");
			let log_messages: Vec<String> = app_logger.log_messages();
			assert_eq!(1, log_messages.len());
			let expected: &str = "TRACE the message\n";
			let actual: &str = log_messages.get(0).unwrap();
			assert_eq!(expected, actual);
		}
	}

	mod debug
	{

		use std::rc::Rc;

		use crate::hilcode::log::has_logger::AppLogger;
		use crate::hilcode::log::has_logger::tests::enable_logging;
		use crate::hilcode::log::has_logger::tests::no_logging;
		use crate::hilcode::log::level::LogLevel;
		use crate::hilcode::log::macros::log_debug;

		#[test]
		fn off()
		{
			let app_logger: Rc<AppLogger> = no_logging();
			log_debug!(app_logger, "the message");
			let log_messages: Vec<String> = app_logger.log_messages();
			assert_eq!(0, log_messages.len());
		}

		#[test]
		fn on()
		{
			let app_logger: Rc<AppLogger> = enable_logging(LogLevel::Debug);
			log_debug!(app_logger, "the message");
			let log_messages: Vec<String> = app_logger.log_messages();
			assert_eq!(1, log_messages.len());
			let expected: &str = "DEBUG the message\n";
			let actual: &str = log_messages.get(0).unwrap();
			assert_eq!(expected, actual);
		}
	}

	mod info
	{

		use std::rc::Rc;

		use crate::hilcode::log::has_logger::AppLogger;
		use crate::hilcode::log::has_logger::tests::enable_logging;
		use crate::hilcode::log::has_logger::tests::no_logging;
		use crate::hilcode::log::level::LogLevel;
		use crate::hilcode::log::macros::log_info;

		#[test]
		fn off()
		{
			let app_logger: Rc<AppLogger> = no_logging();
			log_info!(app_logger, "the message");
			let log_messages: Vec<String> = app_logger.log_messages();
			assert_eq!(0, log_messages.len());
		}

		#[test]
		fn on()
		{
			let app_logger: Rc<AppLogger> = enable_logging(LogLevel::Info);
			log_info!(app_logger, "the message");
			let log_messages: Vec<String> = app_logger.log_messages();
			assert_eq!(1, log_messages.len());
			let expected: &str = "INFO  the message\n";
			let actual: &str = log_messages.get(0).unwrap();
			assert_eq!(expected, actual);
		}
	}

	mod warn
	{

		use std::rc::Rc;

		use crate::hilcode::log::has_logger::AppLogger;
		use crate::hilcode::log::has_logger::tests::enable_logging;
		use crate::hilcode::log::has_logger::tests::no_logging;
		use crate::hilcode::log::level::LogLevel;
		use crate::hilcode::log::macros::log_warn;

		#[test]
		fn off()
		{
			let app_logger: Rc<AppLogger> = no_logging();
			log_warn!(app_logger, "the message");
			let log_messages: Vec<String> = app_logger.log_messages();
			assert_eq!(0, log_messages.len());
		}

		#[test]
		fn on()
		{
			let app_logger: Rc<AppLogger> = enable_logging(LogLevel::Warn);
			log_warn!(app_logger, "the message");
			let log_messages: Vec<String> = app_logger.log_messages();
			assert_eq!(1, log_messages.len());
			let expected: &str = "WARN  the message\n";
			let actual: &str = log_messages.get(0).unwrap();
			assert_eq!(expected, actual);
		}
	}

	mod error
	{

		use std::rc::Rc;

		use crate::hilcode::log::has_logger::AppLogger;
		use crate::hilcode::log::has_logger::tests::enable_logging;
		use crate::hilcode::log::has_logger::tests::no_logging;
		use crate::hilcode::log::level::LogLevel;
		use crate::hilcode::log::macros::log_error;

		#[test]
		fn off()
		{
			let app_logger: Rc<AppLogger> = no_logging();
			log_error!(app_logger, "the message");
			let log_messages: Vec<String> = app_logger.log_messages();
			assert_eq!(0, log_messages.len());
		}

		#[test]
		fn on()
		{
			let app_logger: Rc<AppLogger> = enable_logging(LogLevel::Error);
			log_error!(app_logger, "the message");
			let log_messages: Vec<String> = app_logger.log_messages();
			assert_eq!(1, log_messages.len());
			let expected: &str = "ERROR the message\n";
			let actual: &str = log_messages.get(0).unwrap();
			assert_eq!(expected, actual);
		}
	}

	fn no_logging() -> Rc<AppLogger>
	{
		let app_config: Rc<AppConfig> = AppConfig::builder().without_logging().build();
		AppLogger::new(app_config)
	}

	fn enable_logging(log_level: LogLevel) -> Rc<AppLogger>
	{
		let app_config: Rc<AppConfig> = AppConfig::builder().with_log_level(log_level).build();
		AppLogger::new(app_config)
	}
}

#[cfg(test)]
#[derive(Debug)]
pub struct LogMessages(pub Vec<String>);

#[cfg(test)]
impl LogMessages
{
	pub fn new() -> Rc<RefCell<LogMessages>>
	{
		Rc::new(RefCell::new(LogMessages(Vec::new())))
	}
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
impl std::io::Write for LogMessages
{
	fn write(
		&mut self,
		buffer: &[u8],
	) -> std::io::Result<usize>
	{
		let new_buffer: Vec<u8> = clear_escape_codes(buffer);
		self.0.push(String::from_utf8_lossy(&new_buffer).into_owned());
		std::io::Result::Ok(buffer.len())
	}

	fn flush(&mut self) -> std::io::Result<()>
	{
		std::io::Result::Ok(())
	}
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
impl Logger for LogMessages
{
	fn log_messages(&self) -> Vec<String>
	{
		self.0.clone()
	}
}

#[cfg(test)]
impl AppLogger
{
	pub fn test_logger<'test>(
		app_config: Rc<AppConfig>,
		log_messages: Rc<RefCell<LogMessages>>,
	) -> Rc<AppLogger>
	{
		Rc::new(AppLogger(app_config, log_messages))
	}

	pub fn log_messages(&self) -> Vec<String>
	{
		self.1.borrow().log_messages()
	}
}
