use anyhow::{Context, Result, anyhow};
use std::process::{Command, exit};

use clap::Parser;

fn main() {
    if let Err(err) = do_main() {
        eprintln!("{err:#}");
        exit(1)
    }
}

fn do_main() -> Result<()> {
    match Xtask::parse().cmd {
        Cmd::Check => {
            // Normally, we run linter first to format code after all lints are fixed
            cargo()?
                .args([
                    "clippy",
                    "--all-features",
                    "--all-targets",
                    "--no-deps",
                    "--",
                    "-Dwarnings",
                ])
                .run()?;
            cargo()?.args(["fmt", "--all"]).run()?;
        }
        Cmd::CI => {
            // On CI, we run format checks first because they're cheaper
            cargo()?.args(["fmt", "--all", "--check"]).run()?;
            cargo()?
                .args([
                    "clippy",
                    "--all-features",
                    "--all-targets",
                    "--no-deps",
                    "--",
                    "-Dwarnings",
                ])
                .run()?;
        }
    }

    Ok(())
}

fn cargo() -> Result<Command> {
    std::env::var_os("CARGO")
        .map(Command::new)
        .ok_or_else(|| anyhow!("Missing CARGO environment variable. Did you run `cargo xtask`?"))
}

fn command_context(cmd: &Command) -> String {
    use std::fmt::Write;

    let mut buf = "When executing".to_owned();
    write!(buf, " {:?}", cmd.get_program()).unwrap();
    for arg in cmd.get_args() {
        write!(buf, " {:?}", arg).unwrap();
    }

    buf
}

trait RunCommand {
    fn run(&mut self) -> Result<()>;
}

impl RunCommand for Command {
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

#[derive(clap::Parser, Debug)]
#[command(about, long_about = None)]
struct Xtask {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(clap::Subcommand, Debug)]
enum Cmd {
    /// Lint, format and other checks over project
    Check,
    /// Run checks for continuous integration
    CI,
}
