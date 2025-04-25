use scoped_panic_hook::{catch_panic, hook};

#[cfg(not(test))]
const _: () = {
    assert!(cfg!(panic = "abort"));
};

fn main() {
    match std::env::args()
        .nth(1)
        .expect("Missing test case name")
        .as_str()
    {
        "panic_hook_fires" => panic_hook_fires(),
        "panic_from_within" => panic_from_within(),
        otherwise => panic!("Unknown test case name '{otherwise}'"),
    };
}

fn panic_hook_fires() {
    let _ = hook::catch_unwind_with_scoped_hook(
        |_| {
            println!("Hook fired");
            hook::NextHook::Break
        },
        || panic!("Ooops!"),
    );
}

#[test]
fn panic_hook_fires_verify() {
    let (ok, output) = run_case("panic_hook_fires");
    assert!(!ok);
    assert_eq!(output, "Hook fired\n");
}

fn panic_from_within() {
    let _ = catch_panic(|| panic!("Oops!"));
}

#[test]
fn panic_from_within_verify() {
    let (ok, output) = run_case("panic_from_within");
    assert!(!ok);
    let panic_msg = find_panic_message(&output, module_path!(), "main", "Oops!");
    assert!(panic_msg.is_ok(), "{output}");
}

#[cfg(test)]
fn run_case(case: &str) -> (bool, String) {
    use std::{
        io::{Read, Seek, SeekFrom},
        process::{Command, Stdio},
    };
    const CARGO: &str = env!("CARGO");

    let mut filebuf = tempfile::tempfile().expect("Failed to create tempfile for output buffer");
    let stdout = filebuf
        .try_clone()
        .expect("Failed to clone tempfile handle");
    let stderr = filebuf
        .try_clone()
        .expect("Failed to clone tempfile handle");

    let ok = Command::new(CARGO)
        .args(["run", "-q", "-p"])
        .arg(env!("CARGO_PKG_NAME"))
        .arg("--")
        .arg(case)
        .stdin(Stdio::null())
        .stdout(stdout)
        .stderr(stderr)
        .status()
        .expect("Failed to execute test subprocess command")
        .success();

    let mut buf = String::new();
    filebuf
        .seek(SeekFrom::Start(0))
        .expect("Failed to rewind tempfile to start");
    filebuf
        .read_to_string(&mut buf)
        .expect("Failed to read tempfile contents");

    (ok, buf)
}
/// Find panic message in output
///
/// # Parameters
/// * `string` - source string
/// * `module_path` - `module_path!()` value
/// * `test_name` - test function name
/// * `panic_message` - concrete panic message, if specified, or any panic message, if not
///
/// # Returns
/// * `Ok(str)` - found pattern, returns string's tail after pattern
/// * `Err(str)` - pattern not found, returns initial string
#[cfg(test)]
fn find_panic_message<'a, 'b>(
    string: &'a str,
    module_path: &str,
    test_name: &str,
    panic_message: impl Into<Option<&'b str>>,
) -> Result<&'a str, &'a str> {
    let init_string = Err(string);
    let thread_name = format!("{module_path}::{test_name}");
    let thread_name = &thread_name[thread_name
        .find("::")
        .expect("Full test path is expected to include crate name")
        + 2..];
    let panic_message: Option<&str> = panic_message.into();

    let panic_prefix = format!("thread '{thread_name}' panicked at");
    let string = if let Some(next) = string.find(&panic_prefix) {
        &string[next + panic_prefix.len()..]
    } else {
        return init_string;
    };
    // Start of next line after "thread panicked..."
    let string = if let Some(next) = string.find('\n') {
        &string[(next + 1)..]
    } else {
        return init_string;
    };
    // Panic message line and text after it
    let (msg_line, string) = if let Some(next) = string.find('\n') {
        (&string[..next], &string[(next + 1)..])
    } else {
        (string, "")
    };

    if let Some(panic_message) = panic_message {
        if panic_message == msg_line {
            Ok(string)
        } else {
            init_string
        }
    } else {
        Ok(string)
    }
}
