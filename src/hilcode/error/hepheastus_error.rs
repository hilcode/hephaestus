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
