use crate::hilcode::config::app_config::AppConfig;

pub trait HasAppConfig
{
	fn app_config(&self) -> &AppConfig;
}
