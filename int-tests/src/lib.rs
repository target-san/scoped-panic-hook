use std::env::var_os;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};

use tempfile::tempfile;

#[macro_export]
macro_rules! bin_test {
    (
        test $test_block:block $(,)?
        verify |$status_param:ident, $stdout_param:ident| $verify_block:block $(,)?
    ) => {
        #[test]
        fn bin_test() {
            $crate::run_test(file!(), |$status_param, $stdout_param| $verify_block);
        }

        fn main() {
            $test_block
        }
    };
    (
        test $test_block:block $(,)?
    ) => {
        $crate::bin_test! {
            test $test_block
            verify |status, _stdout| {
                assert!(status.success(), "Test process failed with {status}");
            }
        }
    };
}

#[doc(hidden)]
pub fn run_test(file_name: &str, verify_fn: impl FnOnce(ExitStatus, String)) {
    let cargo = var_os("CARGO").unwrap_or("cargo".into());
    let bin_name = Path::new(file_name)
        .file_stem()
        .expect("Couldn't fetch current binary's name. Make sure you used `bin_test` macro");
    // Build separately, to not mix build errors;
    // use tmp file for output to not clutter console if everything goes well
    let (tmpfile, stdout, stderr) = tmpfile_buffer();

    let status = Command::new(&cargo)
        .args(["build", "-p"])
        .arg(env!("CARGO_PKG_NAME"))
        .arg("--bin")
        .arg(bin_name)
        .stdin(Stdio::null())
        .stdout(stdout)
        .stderr(stderr)
        .status()
        .expect("Failed to execute test binary build");

    if !status.success() {
        eprintln!("{}", read_file(tmpfile));
        panic!("Binary build failed with {status}");
    }
    // Execute test binary
    let (tmpfile, stdout, stderr) = tmpfile_buffer();

    let status = Command::new(&cargo)
        .args(["run", "-q", "-p"])
        .arg(env!("CARGO_PKG_NAME"))
        .arg("--bin")
        .arg(bin_name)
        .stdin(Stdio::null())
        .stdout(stdout)
        .stderr(stderr)
        .status()
        .expect("Failed to execute test binary");

    let output = read_file(tmpfile);
    // Verify test output
    verify_fn(status, output);
}

fn tmpfile_buffer() -> (File, File, File) {
    let file = tempfile().expect("Failed to create temporary file for subprocess output");
    let stdout = file
        .try_clone()
        .expect("Failed to clone tmpfile descriptor");
    let stderr = file
        .try_clone()
        .expect("Failed to clone tmpfile descriptor");

    (file, stdout, stderr)
}

fn read_file(mut file: File) -> String {
    file.seek(SeekFrom::Start(0))
        .expect("Rewind to start failed");

    let mut buffer = String::new();
    file.read_to_string(&mut buffer)
        .expect("Failed to read file into buffer");

    buffer
}
