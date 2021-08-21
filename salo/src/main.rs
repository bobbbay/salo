use clap::{load_yaml, App};

fn main() {
    let yaml = load_yaml!("../cli.yaml");
    let _matches = App::from(yaml).get_matches();
}
