[![Dependent, reproducible, static OS configuration](/docs/assets/banner.svg)](https://github.com/semc-labs/salo)

<h1 align="center">Salo</h1>

<h4 align="center">
    <a href="https://semc-labs.github.io/salo/">Documentation</a>
  | <a href="https://github.com/orgs/semc-labs/projects/2/views/1?filterQuery=salo">Roadmap</a>
  | <a href="https://discord.gg/uxGFjp65pK">Community</a>
  | <a href="./CONTRIBUTING.org">Contributing</a>
</h1>

<p align="center"> A declarative, reproducible, statically-typed build/deployment toolset </p>

**Salo** is a declarative, reproducible, statically-typed configuration language for building and deploying operating system images. This leads to composable, scalable, and functional operating systems in-situ. For more information about features, read [our documentation](https://semc-labs.github.io/salo).

## Features

 * Expressive type system (dependent, first-class functions and types)
 * Efficient deployment (leveraging diffs to only deploy what you need)
 * On-the-fly changes (configuring systems as quickly as possible)
 * Powerful history management (view configuration history, and rollback to previous deployments)
 * Staged changes (see exactly what your configuration does, before it's deployed)
 
## Getting started

To get started with Salo, run (with Cargo):

```shell
cargo t
```

## Documentation

Now that you're ready with Salo, you can learn more about how to develop operating system configuration in [the documentation](https://semc-labs.github.io/salo/).

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change. Furthermore, please make sure to update tests as appropriate.
