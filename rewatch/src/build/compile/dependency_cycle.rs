use super::super::build_types::*;
use crate::helpers;
use ahash::AHashSet;

pub fn find(modules: &Vec<(&String, &Module)>) -> Vec<String> {
    let mut visited: AHashSet<String> = AHashSet::new();
    let mut stack: Vec<String> = vec![];

    // we want to sort the module names so that we always return the same
    // dependency cycle (there can be more than one)
    let mut module_names = modules
        .iter()
        .map(|(name, _)| name.to_string())
        .collect::<Vec<String>>();

    module_names.sort();
    for module_name in module_names {
        if find_dependency_cycle_helper(&module_name, modules, &mut visited, &mut stack) {
            return stack;
        }
        visited.clear();
        stack.clear();
    }
    stack
}

fn find_dependency_cycle_helper(
    module_name: &String,
    modules: &Vec<(&String, &Module)>,
    visited: &mut AHashSet<String>,
    stack: &mut Vec<String>,
) -> bool {
    if let Some(module) = modules
        .iter()
        .find(|(name, _)| *name == module_name)
        .map(|(_, module)| module)
    {
        visited.insert(module_name.to_string());
        // if the module is a mlmap (namespace), we don't want to show this in the path
        // because the namespace is not a module the user created, so only add source files
        // to the stack
        if let SourceType::SourceFile(_) = module.source_type {
            stack.push(module_name.to_string())
        }
        for dep in &module.deps {
            if !visited.contains(dep) {
                if find_dependency_cycle_helper(dep, modules, visited, stack) {
                    return true;
                }
            } else if stack.contains(dep) {
                stack.push(dep.to_string());
                return true;
            }
        }
        // because we only pushed source files to the stack, we also only need to
        // pop these from the stack if we don't find a dependency cycle
        if let SourceType::SourceFile(_) = module.source_type {
            let _ = stack.pop();
        }
        return false;
    }
    false
}

pub fn format(cycle: &[String]) -> String {
    cycle
        .iter()
        .map(|s| helpers::format_namespaced_module_name(s))
        .collect::<Vec<String>>()
        .join(" -> ")
}
