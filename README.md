# MTools

[![release v0.3.3](http://img.shields.io/badge/release-v0.3.3-blue.svg)](https://github.com/faysou/MTools/releases/latest)
[![Semantic Versioning](https://img.shields.io/badge/SemVer-2.0.0-green.svg)](http://semver.org/spec/v2.0.0.html)
[![license MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/faysou/MTools/blob/master/LICENSE)
[![Mathematica 10.0](https://img.shields.io/badge/Mathematica-10+-green.svg)](#compatibility)

The main contribution of MTools is to allow object oriented programming in Mathematica in a very natural way .

The package also contains:
 - Generic classes for manipulating trees of objects and displaying them
 - Automatic interface generation for displaying and editing objects
 - Functions for doing asynchronous evaluation easily using parallel kernels (MSync)
 - Tools for accessing Couchbase, serializing and deserializing objects.


* [Usage example](#usage-example)
* [Installation](#installation)
	* [Automatic installation](#automatic-installation)
    * [Manual installation](#manual-installation)
* [Compatibility](#compatibility)
* [Bugs and requests](#bugs-and-requests)
* [Contributing](#contributing)
* [License](#license)
* [Versioning](#versioning)

## Usage example

You can find usage examples on the [Wolfram Community](http://community.wolfram.com/groups/-/m/t/880686) site and
in the [answer to "Once more on object orientation..." question](http://mathematica.stackexchange.com/a/119896/66)
on Mathematica Stack Exchange.

More examples will be added in the MTools.nb notebook located in the MTools package.


## Installation

### Automatic installation

To install the MTools package evaluate:
```Mathematica
Get["https://raw.githubusercontent.com/faysou/MTools/master/BootstrapInstall.m"]
```

Note that this will also install the
[ProjectInstaller](https://github.com/lshifr/ProjectInstaller)
package, if you don't have it already installed.

To load the MTools package evaluate: ``Needs["MTools`"]``.

### Manual installation

1. Download the latest released
   [MTools.zip](https://github.com/faysou/MTools/releases/download/0.3.3/MTools.zip)
   file.

2. Extract the downloaded `MTools.zip` file to any directory which is on the
   Mathematica `$Path`, e.g. to one obtained by evaluating
   `FileNameJoin[{$UserBaseDirectory,"Applications"}]`.

3. To load the package evaluate: ``Needs["MTools`"]``


## Compatibility

This package requires Mathematica 10 and above, as it makes extensive use of Association.



## Bugs and requests

If you find any bugs or have feature requests please create an
[issue on GitHub](https://github.com/faysou/MTools/issues).



## Contributing

Feel free to fork and send pull requests.

All contributions are welcome!



## License

This package is released under
[The MIT License](https://github.com/faysou/MTools/blob/master/LICENSE).


### Attribution

I used as template the README of the [ClasslessObjects](https://github.com/jkuczm/MathematicaClasslessObjects) project.

I used the following contributions posted by people on Mathematica StackExchange for some functions in the Utils.m package:  

- [Date Picker by R. M.](http://mathematica.stackexchange.com/a/16550/66)  
- [Frozen Pane Grid by Mike Honeychurch](http://mathematica.stackexchange.com/a/21122/66)     
- [Observer Pattern by Leonid Shifrin](http://mathematica.stackexchange.com/a/47664/66)     
- [A tricky evaluation issue solved by ybeltukov](http://mathematica.stackexchange.com/a/73017/66)   




## Versioning

Releases of this package will be numbered using
[Semantic Versioning guidelines](http://semver.org/).