use criterion::{criterion_group, criterion_main, Criterion};
use rewatch::build;
use rewatch::build::clean;
use rewatch::build::packages;
use rewatch::helpers;

use std::fs::File;
use std::io::prelude::*;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("build-package-tree", |b| {
        // Folder for the testrepo
        let folder = "walnut_monorepo";
        let project_root = helpers::get_abs_path(folder);

        b.iter(|| {
            packages::make(&None, &project_root, &None);
        })
    });

    c.bench_function("clean-build-change-build", |b| {
        // Folder for the testrepo
        let folder = "testrepo";
        let filename = "testrepo/packages/dep02/src/Dep02.res";
        // Clean the build
        clean::clean(folder);
        // Read the file we'll be mutating
        let mut file = File::options()
            .read(true)
            .write(true)
            .append(true)
            .open(filename)
            .unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        b.iter(|| {
            // Create initial build
            let _ = build::build(&None, folder, false);
            // Update the file
            let _ = writeln!(file, r#"let log2 = () => ["a", "b"]->forEach(Js.log);log2()"#);
            // Create another build
            let _ = build::build(&None, folder, false);

            // Reset state
            File::create(filename).unwrap();
            file.write_all(contents.as_bytes()).unwrap();
            let _ = build::build(&None, folder, false);
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
