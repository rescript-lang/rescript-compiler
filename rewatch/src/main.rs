use clap::{Parser, ValueEnum};
use regex::Regex;

use rewatch::{build, cmd, lock, watcher};

#[derive(Debug, Clone, ValueEnum)]
enum Command {
    /// Build using Rewatch
    Build,
    /// Build, then start a watcher
    Watch,
    /// Clean the build artifacts
    Clean,
}

/// Rewatch is an alternative build system for the Rescript Compiler bsb (which uses Ninja internally). It strives
/// to deliver consistent and faster builds in monorepo setups with multiple packages, where the
/// default build system fails to pick up changed interfaces across multiple packages.
#[derive(Parser, Debug)]
#[command(version)]
struct Args {
    #[arg(value_enum)]
    command: Option<Command>,

    /// The relative path to where the main bsconfig.json resides. IE - the root of your project.
    folder: Option<String>,

    /// Filter allows for a regex to be supplied which will filter the files to be compiled. For
    /// instance, to filter out test files for compilation while doing feature work.
    #[arg(short, long)]
    filter: Option<String>,

    /// This allows one to pass an additional command to the watcher, which allows it to run when
    /// finished. For instance, to play a sound when done compiling, or to run a test suite.
    /// NOTE - You may need to add '--color=always' to your subcommand in case you want to output
    /// colour as well
    #[arg(short, long)]
    after_build: Option<String>,

    #[arg(short, long)]
    no_timing: Option<bool>,

    /// This creates a source_dirs.json file at the root of the monorepo, which is needed when you
    /// want to use Reanalyze
    #[arg(short, long)]
    create_sourcedirs: Option<bool>,

    #[arg(long)]
    compiler_args: Option<String>,

    #[arg(long)]
    rescript_version: Option<String>,

    #[arg(long)]
    bsc_path: Option<String>,
}

fn main() {
    env_logger::init();
    let args = Args::parse();

    let command = args.command.unwrap_or(Command::Build);
    let folder = args.folder.unwrap_or(".".to_string());
    let filter = args
        .filter
        .map(|filter| Regex::new(filter.as_ref()).expect("Could not parse regex"));

    match args.compiler_args {
        None => (),
        Some(path) => {
            println!(
                "{}",
                build::get_compiler_args(&path, args.rescript_version, args.bsc_path)
            );
            std::process::exit(0);
        }
    }

    match lock::get(&folder) {
        lock::Lock::Error(ref e) => {
            eprintln!("Error while trying to get lock: {e}");
            std::process::exit(1)
        }
        lock::Lock::Aquired(_) => match command {
            Command::Clean => build::clean::clean(&folder, args.bsc_path),
            Command::Build => {
                match build::build(
                    &filter,
                    &folder,
                    args.no_timing.unwrap_or(false),
                    args.create_sourcedirs.unwrap_or(false),
                    args.bsc_path,
                ) {
                    Err(e) => {
                        eprintln!("Error Building: {e}");
                        std::process::exit(1)
                    }
                    Ok(_) => {
                        if let Some(args_after_build) = args.after_build {
                            cmd::run(args_after_build)
                        }
                        std::process::exit(0)
                    }
                };
            }
            Command::Watch => {
                watcher::start(
                    &filter,
                    &folder,
                    args.after_build,
                    args.create_sourcedirs.unwrap_or(false),
                );
            }
        },
    }
}
