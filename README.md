[![GitHub](https://img.shields.io/badge/GitHub-ForEig-blue.svg?style=social&logo=github)](https://github.com/gha3mi/foreig)
[![Version](https://img.shields.io/github/release/gha3mi/foreig.svg)](https://github.com/gha3mi/foreig/releases/latest)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/foreig/)
[![License](https://img.shields.io/github/license/gha3mi/foreig?color=green)](https://github.com/gha3mi/foreig/blob/main/LICENSE)
[![Build](https://github.com/gha3mi/foreig/actions/workflows/ci.yml/badge.svg)](https://github.com/gha3mi/foreig/actions/workflows/ci.yml)

<img alt="ForEig" src="https://github.com/gha3mi/foreig/raw/main/media/logo.png" width="750">

**ForEig**: A Fortran library for eigenvalue and eigenvector calculations.

## Usage

```Fortran
use foreig, only: eig

call eig(A, eig_vec, eig_val ,method='ggev') ! method='syev' .or. 'geev' .or. 'ggev'
```

## fpm dependency

If you want to use `ForEig` as a dependency in your own fpm project,
you can easily include it by adding the following line to your `fpm.toml` file:

```toml
[dependencies]
forieig = {git="https://github.com/gha3mi/forieig.git"}
```

## How to run tests

**Reuirements:**

Fortran Compiler, LAPACK or MKL Libraries

**Clone the repository:**

You can clone the `ForEig` repository from GitHub using the following command:

```shell
git clone https://github.com/gha3mi/forieig.git
```

```shell
cd forieig
```

**Intel Fortran Compiler (ifort)**

```shell
fpm @ifort-test
```
**Intel Fortran Compiler (ifx)**

```shell
fpm @ifx-test
```

**GNU Fortran Compiler (gfortran)**

```shell
fpm @gfortran-test
```

**NVIDIA Compiler (nvfortran)**

```shell
fpm @nvfortran-test
```

## API documentation

The most up-to-date API documentation for the master branch is available
[here](https://gha3mi.github.io/forieig/).
To generate the API documentation for `ForEig` using
[ford](https://github.com/Fortran-FOSS-Programmers/ford) run the following
command:

```shell
ford ford.yml
```

## Contributing

Contributions to `ForEig` are welcome!
If you find any issues or would like to suggest improvements, please open an issue.
