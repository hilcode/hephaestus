use clap::ArgAction;
use clap::Parser;
use const_format::concatcp;

pub static BANNER: &str = {
	static VERSION: &str = env!("CARGO_PKG_VERSION");
	static DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");
	static BANNER_PREFIX: &str = r#"
    __  __           __                    __
   / / / /__  ____  / /_  ___  ____ ______/ /___  _______
  / /_/ / _ \/ __ \/ __ \/ _ \/ __ `/ ___/ __/ / / / ___/
 / __  /  __/ /_/ / / / /  __/ /_/ (__  ) /_/ /_/ (__  )
/_/ /_/\___/ .___/_/ /_/\___/\__,_/____/\__/\__,_/____/
          /_/   Version "#;
	static BANNER_SUFFIX: &str = "

Copyright © 2025 Hilbrand C. Wijbenga. All rights reserved.

";
	concatcp!(BANNER_PREFIX, VERSION, BANNER_SUFFIX, DESCRIPTION)
};

#[derive(Debug, Parser)]
#[command(version, about, long_about(BANNER))]
pub struct Cli
{
	/// Be more quiet
	#[arg(short, long, action = ArgAction::Count)]
	pub quiet: u8,
	/// Be more verbose
	#[arg(short, long, action = ArgAction::Count)]
	pub verbose: u8,
}
