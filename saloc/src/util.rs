use color_eyre::owo_colors::OwoColorize;
use color_eyre::Report;
use tracing::info;

crate type Result<T> = std::result::Result<T, Report>;

crate fn setup() -> Result<()> {
    if std::env::var("SALO_LOG").unwrap_or("0".to_string()) == "1" {
        if std::env::var("RUST_BACKTRACE").is_err() {
            std::env::set_var("RUST_BACKTRACE", "1")
        }
        color_eyre::install()?;

        if std::env::var("RUST_LOG").is_err() {
            std::env::set_var("RUST_LOG", "info")
        }
        tracing_subscriber::fmt::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .init();
    } else {
        // We're in a non-debug environment
        color_eyre::config::HookBuilder::default()
            .issue_url(concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new"))
            .add_issue_metadata("version", env!("CARGO_PKG_VERSION"))
            .panic_section(format!("{}", "This is a compiler error.".red()))
            .install()?;
    }

    info!("Setup complete");
    Ok(())
}

#[derive(Debug)]
crate struct Code<'a> {
    crate content: &'a str,
    crate filename: &'a str,
}

impl<'a> Code<'a> {
    crate fn new(code: &'a str, filename: &'a str) -> Self {
        Self {
            content: code,
            filename,
        }
    }
}
