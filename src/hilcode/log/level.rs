#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum LogLevel
{
	Off,
	Error,
	Warn,
	Info,
	Debug,
	Trace,
}

impl LogLevel
{
	pub fn is_enabled(
		&self,
		configured_log_level: LogLevel,
	) -> bool
	{
		*self <= configured_log_level
	}

	pub fn increase(self) -> LogLevel
	{
		match self
		{
			LogLevel::Off => LogLevel::Error,
			LogLevel::Error => LogLevel::Warn,
			LogLevel::Warn => LogLevel::Info,
			LogLevel::Info => LogLevel::Debug,
			LogLevel::Debug => LogLevel::Trace,
			LogLevel::Trace => LogLevel::Trace,
		}
	}

	pub fn decrease(self) -> LogLevel
	{
		match self
		{
			LogLevel::Off => LogLevel::Off,
			LogLevel::Error => LogLevel::Off,
			LogLevel::Warn => LogLevel::Error,
			LogLevel::Info => LogLevel::Warn,
			LogLevel::Debug => LogLevel::Info,
			LogLevel::Trace => LogLevel::Debug,
		}
	}
}

#[cfg(test)]
mod tests
{

	use crate::hilcode::log::level::LogLevel;

	mod is_enabled
	{

		use crate::hilcode::log::level::LogLevel;

		#[test]
		fn info_debug()
		{
			let configured_log_level: LogLevel = LogLevel::Info;
			assert_eq!(LogLevel::Debug.is_enabled(configured_log_level), false);
		}

		#[test]
		fn info_info()
		{
			let configured_log_level: LogLevel = LogLevel::Info;
			assert_eq!(LogLevel::Info.is_enabled(configured_log_level), true);
		}

		#[test]
		fn info_warn()
		{
			let configured_log_level: LogLevel = LogLevel::Info;
			assert_eq!(LogLevel::Warn.is_enabled(configured_log_level), true);
		}
	}

	#[test]
	fn increase()
	{
		assert_eq!(LogLevel::Off.increase(), LogLevel::Error);
		assert_eq!(LogLevel::Error.increase(), LogLevel::Warn);
		assert_eq!(LogLevel::Warn.increase(), LogLevel::Info);
		assert_eq!(LogLevel::Info.increase(), LogLevel::Debug);
		assert_eq!(LogLevel::Debug.increase(), LogLevel::Trace);
		assert_eq!(LogLevel::Trace.increase(), LogLevel::Trace);
	}

	#[test]
	fn decrease()
	{
		assert_eq!(LogLevel::Off.decrease(), LogLevel::Off);
		assert_eq!(LogLevel::Error.decrease(), LogLevel::Off);
		assert_eq!(LogLevel::Warn.decrease(), LogLevel::Error);
		assert_eq!(LogLevel::Info.decrease(), LogLevel::Warn);
		assert_eq!(LogLevel::Debug.decrease(), LogLevel::Info);
		assert_eq!(LogLevel::Trace.decrease(), LogLevel::Debug);
	}
}
