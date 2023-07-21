![ForEig](media/logo.png)
============

**ForEig**: A Fortran library for eigenvalue and eigenvector calculations.


## How to Use ForEig

### Installation of ForEig Library

To use ForEig, follow the steps below:

- **Reuirements:**

  Fortran Compiler, LAPACK or MKL Libraries

- **Clone the repository:**

   You can clone the ForEig repository from GitHub using the following command:

   ```shell
   git clone https://github.com/gha3mi/foreig.git
   cd foreig
   ```

- **Build using the Fortran Package Manager (fpm):**

   ForEig can be built using [fpm](https://github.com/fortran-lang/fpm).
   Make sure you have fpm installed, and then execute the following command:

  **GNU Fortran Compiler (gfortran)**

   ```shell
   fpm install --prefix . --compiler gfortran --flag "-Wno-line-truncation -Ofast -march=native -llapack"
   ```

  **Intel Fortran Compiler (ifort)**

   ```shell
   fpm install --prefix . --compiler ifort --flag "-Ofast -xHost -mtune=native -qmkl=parallel"
   ```

  **Intel Fortran Compiler (ifx)**

    ```shell
   fpm install --prefix . --compiler ifx --flag "-Ofast -xHost -mtune=native -qmkl=parallel"
   ```

### Adding ForEig as an fpm Dependency

If you want to use ForEig as a dependency in your own fpm project,
you can easily include it by adding the following line to your `fpm.toml` file:

```toml
[dependencies]
foreig = {git="https://github.com/gha3mi/foreig.git"}
```

## API Documentation

To generate the API documentation for the `ForEig` module using
[ford](https://github.com/Fortran-FOSS-Programmers/ford) run the following
command:

```shell
ford ford.yml
```

## Contributing
Contributions to `ForEig` are welcome! If you find any issues or would like to suggest improvements, please open an issue or submit a pull request.