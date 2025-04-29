#[must_use]
pub fn plural_s(n: usize) -> &'static str {
    if n == 1 { "" } else { "s" }
}

#[must_use]
pub fn plural_is_are(n: usize) -> &'static str {
    if n == 1 { "is" } else { "are" }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::{
        fs::{File, read_to_string},
        io::{self, BufRead},
        path::{Path, PathBuf},
    };

    pub fn read_sample_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
    where
        P: AsRef<Path>,
    {
        let filename = sample_path(filename.as_ref());
        read_lines(filename)
    }

    pub fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
    where
        P: AsRef<Path>,
    {
        let file = File::open(filename)?;
        Ok(io::BufReader::new(file).lines())
    }

    pub fn read_sample_file_to_string<P>(filename: P) -> String
    where
        P: AsRef<Path>,
    {
        read_to_string(sample_path(filename)).unwrap()
    }

    /// Returns the path to the given sample file.
    pub fn sample_path<P>(filename: P) -> PathBuf
    where
        P: AsRef<Path>,
    {
        let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
        workspace_root.join("src").join("samples").join(filename)
    }

    #[test]
    fn sample_pathing() {
        let strings_file = read_sample_file_to_string("91_strings");
        assert_eq!(&strings_file[..26], r#""double quoted string: '`""#);
    }
}
