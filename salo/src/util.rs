use color_eyre::Report;

crate type Result<T> = std::result::Result<T, Report>;
