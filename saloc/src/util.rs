use color_eyre::eyre::Result;
use tracing::info;

use crate::ast::Expr;

/// Enables/disables logging hooks and levels depending on the value of `SALO_LOG`.
///  "0" | anything else: don't log, errors provide an issue URL.
///  "1": Short logs
///  "2": Long logs
crate fn setup() -> Result<()> {
    let loglevel = std::env::var("SALO_LOG").unwrap_or("0".to_string());
    let hook = color_eyre::config::HookBuilder::blank();

    let hook = match &*loglevel {
        "1" => {
            std::env::set_var("RUST_BACKTRACE", "1");
            std::env::set_var("RUST_LOG", "info");

            tracing_subscriber::fmt::fmt()
                .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
                .init();

            hook
        }
        "2" => {
            std::env::set_var("RUST_BACKTRACE", "full");
            std::env::set_var("RUST_LOG", "full");
            std::env::set_var("COLORBT_SHOW_HIDDEN", "1");

            tracing_subscriber::fmt::fmt()
                .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
                .init();

            hook
        }
        _ => hook
            .issue_url(concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new"))
            .display_env_section(false),
    };

    hook.install()?;

    info!("Setup complete");
    Ok(())
}

/// Struct that contains code.
#[derive(Debug)]
crate struct Code<'life> {
    crate content: &'life str,
    crate filename: &'life str,

    crate ast: Option<Vec<Expr<'life>>>,
}

impl<'life> Code<'life> {
    crate fn new(code: &'life str, filename: &'life str) -> Self {
        Self {
            content: code,
            filename,
            ast: None,
        }
    }
}
