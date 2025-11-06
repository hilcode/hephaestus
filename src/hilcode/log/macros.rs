#[allow(unused_macros)]
macro_rules! log_trace {
	($self:expr, $arg:expr) => (
		{
			#[allow(unused_imports)]
			use crate::hilcode::log::has_logger::HasLogger;
			if $self.is_enabled($crate::hilcode::log::level::LogLevel::Trace) {
				$self.trace($arg);
			}
		}
	);

	($self:expr, $($arg:expr),+) => (
		{
			#[allow(unused_imports)]
			use crate::hilcode::log::has_logger::HasLogger;
			if $self.is_enabled($crate::hilcode::log::level::LogLevel::Trace) {
				$self.trace(&format!("{}", format_args!($($arg),+)));
			}
		}
	);
}
#[allow(unused_imports)]
pub(crate) use log_trace;

#[allow(unused_macros)]
macro_rules! log_debug {
	($self:expr, $arg:expr) => (
		{
			#[allow(unused_imports)]
			use crate::hilcode::log::has_logger::HasLogger;
			if $self.is_enabled($crate::hilcode::log::level::LogLevel::Debug) {
				$self.debug($arg);
			}
		}
	);

	($self:expr, $($arg:expr),+) => (
		{
			#[allow(unused_imports)]
			use crate::hilcode::log::has_logger::HasLogger;
			if $self.is_enabled($crate::hilcode::log::level::LogLevel::Debug) {
				$self.debug(&format!("{}", format_args!($($arg),+)));
			}
		}
	);
}
#[allow(unused_imports)]
pub(crate) use log_debug;

#[allow(unused_macros)]
macro_rules! log_info {
	($self:expr, $arg:expr) => (
		{
			#[allow(unused_imports)]
			use crate::hilcode::log::has_logger::HasLogger;
			if $self.is_enabled($crate::hilcode::log::level::LogLevel::Info) {
				$self.info($arg);
			}
		}
	);

	($self:expr, $($arg:expr),+) => (
		{
			#[allow(unused_imports)]
			use crate::hilcode::log::has_logger::HasLogger;
			if $self.is_enabled($crate::hilcode::log::level::LogLevel::Info) {
				$self.info(&format!("{}", format_args!($($arg),+)));
			}
		}
	);
}
#[allow(unused_imports)]
pub(crate) use log_info;

#[allow(unused_macros)]
macro_rules! log_warn {
	($self:expr, $arg:expr) => (
		{
			#[allow(unused_imports)]
			use crate::hilcode::log::has_logger::HasLogger;
			if $self.is_enabled($crate::hilcode::log::level::LogLevel::Warn) {
				$self.warn($arg);
			}
		}
	);

	($self:expr, $($arg:expr),+) => (
		{
			#[allow(unused_imports)]
			use crate::hilcode::log::has_logger::HasLogger;
			if $self.is_enabled($crate::hilcode::log::level::LogLevel::Warn) {
				$self.warn(&format!("{}", format_args!($($arg),+)));
			}
		}
	);
}
#[allow(unused_imports)]
pub(crate) use log_warn;

#[allow(unused_macros)]
macro_rules! log_error {
	($self:expr, $arg:expr) => (
		{
			#[allow(unused_imports)]
			use crate::hilcode::log::has_logger::HasLogger;
			if $self.is_enabled($crate::hilcode::log::level::LogLevel::Error) {
				$self.error($arg);
			}
		}
	);

	($self:expr, $($arg:expr),+) => (
		{
			#[allow(unused_imports)]
			use crate::hilcode::log::has_logger::HasLogger;
			if $self.is_enabled($crate::hilcode::log::level::LogLevel::Error) {
				$self.error(&format!("{}", format_args!($($arg),+)));
			}
		}
	);
}
#[allow(unused_imports)]
pub(crate) use log_error;
