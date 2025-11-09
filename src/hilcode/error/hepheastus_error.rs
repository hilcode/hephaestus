use std::fmt::Display;
use std::io::Error;

use globwalker::GlobError;
use globwalker::WalkError;

#[derive(Debug)]
pub enum HepheastusError
{
	InvalidGlob(GlobError),
	IoError(Error),
	DirectoryWalkerError(WalkError),
}

impl Display for HepheastusError
{
	fn fmt(
		&self,
		formatter: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result
	{
		match self
		{
			Self::DirectoryWalkerError(error) =>
			{
				formatter.write_fmt(format_args!("HephaestusError::DirectoryWalkerError({:?})", error))
			}

			Self::InvalidGlob(glob_error) =>
			{
				formatter.write_fmt(format_args!("HephaestusError::InvalidGlob({:?})", glob_error))
			}

			Self::IoError(error) => formatter.write_fmt(format_args!("HephaestusError::IoError({:?})", error)),
		}
	}
}

impl From<GlobError> for HepheastusError
{
	fn from(value: GlobError) -> Self
	{
		HepheastusError::InvalidGlob(value)
	}
}

impl From<Error> for HepheastusError
{
	fn from(value: Error) -> Self
	{
		HepheastusError::IoError(value)
	}
}

impl From<WalkError> for HepheastusError
{
	fn from(value: WalkError) -> Self
	{
		HepheastusError::DirectoryWalkerError(value)
	}
}
