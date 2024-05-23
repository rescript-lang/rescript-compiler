use crate::helpers::{is_source_file, LexicalAbsolute};
use ahash::AHashMap;
use std::path::PathBuf;
use std::{error, fs};

pub fn read_folders(
    filter: &Option<regex::Regex>,
    path: &str,
    recurse: bool,
) -> Result<AHashMap<String, fs::Metadata>, Box<dyn error::Error>> {
    let mut map: AHashMap<String, fs::Metadata> = AHashMap::new();

    let path_buf = PathBuf::from(path);
    let abs_path = path_buf
        .to_lexical_absolute()
        .map(|x| x.to_str().map(|y| y.to_string()).unwrap_or("".to_string()))
        .and_then(|x| fs::metadata(x.to_owned()).map(|m| (x.to_owned(), m)));

    for entry in fs::read_dir(path.replace("//", "/"))? {
        let path_buf = entry.map(|entry| entry.path())?;
        let metadata = fs::metadata(&path_buf)?;
        let name = path_buf
            .file_name()
            .and_then(|x| x.to_str())
            .unwrap_or("Unknown")
            .to_string();

        let path_ext = path_buf.extension().and_then(|x| x.to_str());

        if metadata.file_type().is_dir() && recurse {
            match read_folders(&filter, &(path.to_owned() + "/" + &name + "/"), recurse) {
                Ok(s) => map.extend(s),
                Err(e) => println!("Error reading directory: {}", e),
            }
        }
        match path_ext {
            Some(extension) if is_source_file(extension) => match abs_path {
                Ok((ref path, _))
                    if filter
                        .as_ref()
                        .map(|re| !re.is_match(&name))
                        .unwrap_or(true) =>
                {
                    map.insert(path.to_owned() + "/" + &name, metadata);
                }
                Ok(_) => println!("Filtered: {:?}", name),
                Err(ref e) => println!("Error reading directory: {}", e),
            },
            _ => (),
        }
    }

    Ok(map)
}
