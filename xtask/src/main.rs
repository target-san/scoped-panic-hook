use anyhow::{Context, Result, anyhow};
use clap::Parser;
use std::ffi::OsStr;
use std::path::Path;
use std::process::{Command, exit};

fn main() {
    if let Err(err) = do_main() {
        eprintln!("{err:#}");
        exit(1)
    }
}

#[derive(clap::Parser, Debug)]
#[command(about, long_about = None)]
struct Xtask {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(clap::Subcommand, Debug)]
enum Cmd {
    /// Lint, format and other checks over project
    Tidy,
    /// Run checks for continuous integration
    CI,
    /// Run cleanup for all crates in project, incl. ones not in workspace
    Clean,
    /// Run all tests in project, incl. crates not in main workspace
    Test,
}

fn do_main() -> Result<()> {
    let crates = [None, Some("test-panic-abort"), Some("xtask")];

    let test_crates = [None, Some("test-panic-abort")];

    match Xtask::parse().cmd {
        Cmd::Tidy => {
            for crate_ in crates {
                // Normally, we run linter first to format code after all lints are fixed
                cargo_clippy(crate_).run()?;
                cargo_fmt(crate_, false).run()?;
            }
        }
        Cmd::CI => {
            for crate_ in crates {
                // On CI, we run format checks first because they're cheaper
                cargo_fmt(crate_, true).run()?;
                cargo_clippy(crate_).run()?;
            }
        }
        Cmd::Test => {
            for crate_ in test_crates {
                cargo_test(crate_).run()?;
            }
        }
        Cmd::Clean => {
            for crate_ in crates {
                cargo_clean(crate_).run()?;
            }
        }
    }

    Ok(())
}

fn cargo() -> Command {
    Command::new(env!("CARGO"))
}

fn cargo_clippy<P: AsRef<OsStr>>(manifest_dir: Option<P>) -> Command {
    let mut cargo = cargo();
    cargo.args(["clippy", "--all-features", "--all-targets", "--no-deps"]);

    if let Some(dir) = manifest_dir {
        cargo.manifest_dir(dir);
    }
    cargo.args(["--", "-Dwarnings"]);

    cargo
}

fn cargo_fmt<P: AsRef<OsStr>>(manifest_dir: Option<P>, check: bool) -> Command {
    let mut cargo = cargo();
    cargo.args(["fmt", "--all"]);

    if let Some(dir) = manifest_dir {
        cargo.manifest_dir(dir);
    }

    if check {
        cargo.arg("--check");
    }

    cargo
}

fn cargo_clean<P: AsRef<OsStr>>(manifest_dir: Option<P>) -> Command {
    let mut cargo = cargo();
    cargo.arg("clean");

    if let Some(dir) = manifest_dir {
        cargo.manifest_dir(dir);
    }

    cargo
}

fn cargo_test<P: AsRef<OsStr>>(manifest_dir: Option<P>) -> Command {
    let mut cargo = cargo();
    cargo.args(["test", "--all"]);

    if let Some(dir) = manifest_dir {
        cargo.manifest_dir(dir);
    }

    cargo
}

trait RunCommand {
    fn manifest_dir(&mut self, dir: impl AsRef<OsStr>) -> &mut Self;
    fn run(&mut self) -> Result<()>;
}

impl RunCommand for Command {
    fn manifest_dir(&mut self, dir: impl AsRef<OsStr>) -> &mut Self {
        self.arg("--manifest-path").arg(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("..")
                .join(Path::new(&dir))
                .join("Cargo.toml"),
        )
    }

    fn run(&mut self) -> Result<()> {
        (|| {
            let status = self.status()?;

            if status.success() {
                Ok(())
            } else {
                Err(anyhow!("Command failed with {status}"))
            }
        })()
        .with_context(|| command_context(self))
    }
}

fn command_context(cmd: &Command) -> String {
    use std::fmt::Write;

    let mut buf = "When executing".to_owned();
    write!(buf, " {:?}", cmd.get_program()).unwrap();
    for arg in cmd.get_args() {
        write!(buf, " {arg:?}").unwrap();
    }

    buf
}
