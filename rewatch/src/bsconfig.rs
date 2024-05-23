use crate::build::packages;
use convert_case::{Case, Casing};
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum OneOrMore<T> {
    Multiple(Vec<T>),
    Single(T),
}

#[derive(Deserialize, Debug, Clone, PartialEq, Hash)]
#[serde(untagged)]
pub enum Subdirs {
    Qualified(Vec<Source>),
    Recurse(bool),
}
impl Eq for Subdirs {}

#[derive(Deserialize, Debug, Clone, PartialEq, Hash)]
pub struct PackageSource {
    pub dir: String,
    pub subdirs: Option<Subdirs>,
    #[serde(rename = "type")]
    pub type_: Option<String>,
}

/// `to_qualified_without_children` takes a tree like structure of dependencies, coming in from
/// `bsconfig`, and turns it into a flat list. The main thing we extract here are the source
/// folders, and optional subdirs, where potentially, the subdirs recurse or not.
pub fn to_qualified_without_children(s: &Source, sub_path: Option<PathBuf>) -> PackageSource {
    match s {
        Source::Shorthand(dir) => PackageSource {
            dir: sub_path
                .map(|p| p.join(Path::new(dir)))
                .unwrap_or(Path::new(dir).to_path_buf())
                .to_string_lossy()
                .to_string(),
            subdirs: None,
            type_: None,
        },
        Source::Qualified(PackageSource {
            dir,
            type_,
            subdirs: Some(Subdirs::Recurse(should_recurse)),
        }) => PackageSource {
            dir: sub_path
                .map(|p| p.join(Path::new(dir)))
                .unwrap_or(Path::new(dir).to_path_buf())
                .to_string_lossy()
                .to_string(),
            subdirs: Some(Subdirs::Recurse(*should_recurse)),
            type_: type_.to_owned(),
        },
        Source::Qualified(PackageSource { dir, type_, .. }) => PackageSource {
            dir: sub_path
                .map(|p| p.join(Path::new(dir)))
                .unwrap_or(Path::new(dir).to_path_buf())
                .to_string_lossy()
                .to_string(),
            subdirs: None,
            type_: type_.to_owned(),
        },
    }
}

impl Eq for PackageSource {}

#[derive(Deserialize, Debug, Clone, PartialEq, Hash)]
#[serde(untagged)]
pub enum Source {
    Shorthand(String),
    Qualified(PackageSource),
}
impl Eq for Source {}

#[derive(Deserialize, Debug, Clone)]
pub struct PackageSpec {
    pub module: String,
    #[serde(rename = "in-source")]
    pub in_source: bool,
    pub suffix: Option<String>,
}

#[derive(Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum Error {
    Catchall(bool),
    Qualified(String),
}

#[derive(Deserialize, Debug, Clone)]
pub struct Warnings {
    pub number: Option<String>,
    pub error: Option<Error>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct Reason {
    #[serde(rename = "react-jsx")]
    pub react_jsx: i32,
}

#[derive(Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum NamespaceConfig {
    Bool(bool),
    String(String),
}

#[derive(Deserialize, Debug, Clone)]
pub enum JsxMode {
    #[serde(rename = "classic")]
    Classic,
    #[serde(rename = "automatic")]
    Automatic,
}

#[derive(Deserialize, Debug, Clone)]
pub enum JsxModule {
    #[serde(rename = "react")]
    React,
}

#[derive(Deserialize, Debug, Clone)]
pub struct JsxSpecs {
    pub version: Option<i32>,
    pub module: Option<JsxModule>,
    pub mode: Option<JsxMode>,
    #[serde(rename = "v3-dependencies")]
    pub v3_dependencies: Option<Vec<String>>,
}

/// # bsconfig.json representation
/// This is tricky, there is a lot of ambiguity. This is probably incomplete.
#[derive(Deserialize, Debug, Clone)]
pub struct Config {
    pub name: String,
    pub sources: OneOrMore<Source>,
    #[serde(rename = "package-specs")]
    pub package_specs: Option<OneOrMore<PackageSpec>>,
    pub warnings: Option<Warnings>,
    pub suffix: Option<String>,
    #[serde(rename = "pinned-dependencies")]
    pub pinned_dependencies: Option<Vec<String>>,
    #[serde(rename = "bs-dependencies")]
    pub bs_dependencies: Option<Vec<String>>,
    #[serde(rename = "bs-dev-dependencies")]
    pub bs_dev_dependencies: Option<Vec<String>>,
    #[serde(rename = "ppx-flags")]
    pub ppx_flags: Option<Vec<OneOrMore<String>>>,
    #[serde(rename = "bsc-flags")]
    pub bsc_flags: Option<Vec<OneOrMore<String>>>,
    pub reason: Option<Reason>,
    pub namespace: Option<NamespaceConfig>,
    pub jsx: Option<JsxSpecs>,
    pub uncurried: Option<bool>,
    // this is a new feature of rewatch, and it's not part of the bsconfig.json spec
    #[serde(rename = "namespace-entry")]
    pub namespace_entry: Option<String>,
    // this is a new feature of rewatch, and it's not part of the bsconfig.json spec
    #[serde(rename = "allowed-dependents")]
    pub allowed_dependents: Option<Vec<String>>,
}

/// This flattens string flags
pub fn flatten_flags(flags: &Option<Vec<OneOrMore<String>>>) -> Vec<String> {
    match flags {
        None => vec![],
        Some(xs) => xs
            .iter()
            .flat_map(|x| match x {
                OneOrMore::Single(y) => vec![y.to_owned()],
                OneOrMore::Multiple(ys) => ys.to_owned(),
            })
            .collect::<Vec<String>>()
            .iter()
            .flat_map(|str| str.split(' '))
            .map(|str| str.to_string())
            .collect::<Vec<String>>(),
    }
}

/// Since ppx-flags could be one or more, and could be nested potentiall, this function takes the
/// flags and flattens them outright.
pub fn flatten_ppx_flags(
    node_modules_dir: &String,
    flags: &Option<Vec<OneOrMore<String>>>,
    package_name: &String,
) -> Vec<String> {
    match flags {
        None => vec![],
        Some(xs) => xs
            .iter()
            .flat_map(|x| match x {
                OneOrMore::Single(y) => {
                    let first_character = y.chars().next();
                    match first_character {
                        Some('.') => {
                            vec![
                                "-ppx".to_string(),
                                node_modules_dir.to_owned() + "/" + package_name + "/" + y,
                            ]
                        }
                        _ => vec!["-ppx".to_string(), node_modules_dir.to_owned() + "/" + y],
                    }
                }
                OneOrMore::Multiple(ys) if ys.is_empty() => vec![],
                OneOrMore::Multiple(ys) => {
                    let first_character = ys[0].chars().next();
                    let ppx = match first_character {
                        Some('.') => node_modules_dir.to_owned() + "/" + package_name + "/" + &ys[0],
                        _ => node_modules_dir.to_owned() + "/" + &ys[0],
                    };
                    vec![
                        "-ppx".to_string(),
                        vec![ppx]
                            .into_iter()
                            .chain(ys[1..].to_owned())
                            .collect::<Vec<String>>()
                            .join(" "),
                    ]
                }
            })
            .collect::<Vec<String>>(),
    }
}

/// Try to convert a bsconfig from a certain path to a bsconfig struct
pub fn read(path: String) -> Config {
    fs::read_to_string(path.clone())
        .map_err(|e| format!("Could not read bsconfig. {path} - {e}"))
        .and_then(|x| {
            serde_json::from_str::<Config>(&x).map_err(|e| format!("Could not parse bsconfig. {path} - {e}"))
        })
        .expect("Errors reading bsconfig")
}

fn check_if_rescript11_or_higher(version: &str) -> bool {
    version.split('.').next().unwrap().parse::<usize>().unwrap() >= 11
}

fn namespace_from_package_name(package_name: &str) -> String {
    package_name
        .to_owned()
        .replace('@', "")
        .replace('/', "_")
        .to_case(Case::Pascal)
}

impl Config {
    pub fn get_namespace(&self) -> packages::Namespace {
        let namespace_from_package = namespace_from_package_name(&self.name);
        match (self.namespace.as_ref(), self.namespace_entry.as_ref()) {
            (Some(NamespaceConfig::Bool(false)), _) => packages::Namespace::NoNamespace,
            (None, _) => packages::Namespace::NoNamespace,
            (Some(NamespaceConfig::Bool(true)), None) => {
                packages::Namespace::Namespace(namespace_from_package)
            }
            (Some(NamespaceConfig::Bool(true)), Some(entry)) => packages::Namespace::NamespaceWithEntry {
                namespace: namespace_from_package,
                entry: entry.to_string(),
            },
            (Some(NamespaceConfig::String(str)), None) => match str.as_str() {
                "true" => packages::Namespace::Namespace(namespace_from_package),
                namespace if namespace.is_case(Case::UpperFlat) => {
                    packages::Namespace::Namespace(namespace.to_string())
                }
                namespace => packages::Namespace::Namespace(namespace.to_string().to_case(Case::Pascal)),
            },
            (Some(self::NamespaceConfig::String(str)), Some(entry)) => match str.as_str() {
                "true" => packages::Namespace::NamespaceWithEntry {
                    namespace: namespace_from_package,
                    entry: entry.to_string(),
                },
                namespace if namespace.is_case(Case::UpperFlat) => packages::Namespace::NamespaceWithEntry {
                    namespace: namespace.to_string(),
                    entry: entry.to_string(),
                },
                namespace => packages::Namespace::NamespaceWithEntry {
                    namespace: namespace.to_string().to_case(Case::Pascal),
                    entry: entry.to_string(),
                },
            },
        }
    }
    pub fn get_jsx_args(&self) -> Vec<String> {
        match (self.reason.to_owned(), self.jsx.to_owned()) {
            (_, Some(jsx)) => match jsx.version {
                Some(version) if version == 3 || version == 4 => {
                    vec!["-bs-jsx".to_string(), version.to_string()]
                }
                Some(_version) => panic!("Unsupported JSX version"),
                None => vec![],
            },
            (Some(reason), None) => {
                vec!["-bs-jsx".to_string(), format!("{}", reason.react_jsx)]
            }
            _ => vec![],
        }
    }

    pub fn get_jsx_mode_args(&self) -> Vec<String> {
        match self.jsx.to_owned() {
            Some(jsx) => match jsx.mode {
                Some(JsxMode::Classic) => {
                    vec!["-bs-jsx-mode".to_string(), "classic".to_string()]
                }
                Some(JsxMode::Automatic) => {
                    vec!["-bs-jsx-mode".to_string(), "automatic".to_string()]
                }

                None => vec![],
            },
            _ => vec![],
        }
    }

    pub fn get_jsx_module_args(&self) -> Vec<String> {
        match self.jsx.to_owned() {
            Some(jsx) => match jsx.module {
                Some(JsxModule::React) => {
                    vec!["-bs-jsx-module".to_string(), "react".to_string()]
                }
                None => vec![],
            },
            _ => vec![],
        }
    }

    pub fn get_uncurried_args(&self, version: &str) -> Vec<String> {
        if check_if_rescript11_or_higher(version) {
            match self.uncurried.to_owned() {
                // v11 is always uncurried except iff explicitly set to false in the root rescript.json
                Some(false) => vec![],
                _ => vec!["-uncurried".to_string()],
            }
        } else {
            vec![]
        }
    }

    pub fn get_module(&self) -> String {
        match &self.package_specs {
            Some(OneOrMore::Single(PackageSpec { module, .. })) => module.to_string(),
            Some(OneOrMore::Multiple(vec)) => match vec.first() {
                Some(PackageSpec { module, .. }) => module.to_string(),
                None => "commonjs".to_string(),
            },
            _ => "commonjs".to_string(),
        }
    }

    pub fn get_suffix(&self) -> String {
        match &self.package_specs {
            Some(OneOrMore::Single(PackageSpec { suffix, .. })) => suffix.to_owned(),
            Some(OneOrMore::Multiple(vec)) => match vec.first() {
                Some(PackageSpec { suffix, .. }) => suffix.to_owned(),
                None => None,
            },

            _ => None,
        }
        .or(self.suffix.to_owned())
        .unwrap_or(".js".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_getters() {
        let json = r#"
        {
            "name": "my-monorepo",
            "sources": [ { "dir": "src/", "subdirs": true } ],
            "package-specs": [ { "module": "es6", "in-source": true } ],
            "suffix": ".mjs",
            "pinned-dependencies": [ "@teamwalnut/app" ],
            "bs-dependencies": [ "@teamwalnut/app" ]
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        assert_eq!(config.get_suffix(), ".mjs");
        assert_eq!(config.get_module(), "es6");
    }
}
