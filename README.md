![ForEig](media/logo.png)
============

**ForEig**: A Fortran library for eigenvalue and eigenvector calculations.


## Installation

### fpm
ForEig can be cloned and then built using [fpm](https://github.com/fortran-lang/fpm), following the instructions provided in the documentation available on Fortran Package Manager.

```bash
git clone https://github.com/gha3mi/foreig.git
cd foreig
fpm install --prefix .
```

Or you can easily include this package as a dependency in your `fpm.toml` file.

```toml
[dependencies]
foreig = {git="https://github.com/gha3mi/foreig.git"}
```
-----

## Documentation
To generate the documentation for the `ForEig` module using [ford](https://github.com/Fortran-FOSS-Programmers/ford) run the following command:
```bash
ford ford.yml
```
-----

## Contributing
Contributions to `ForEig` are welcome! If you find any issues or would like to suggest improvements, please open an issue or submit a pull request.