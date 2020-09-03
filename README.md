# SMLP2020

This code base is using the Julia Language and [DrWatson](https://juliadynamics.github.io/DrWatson.jl/stable/)
to make a reproducible scientific project named
> SMLP2020

It is authored by Phillip Alday, Douglas Bates, Reinhold Kliegl.

To (locally) reproduce this project, do the following:

0. Clone this repository

1. Open a Julia console and do:
   ```
   julia> using Pkg
   julia> Pkg.activate("path/to/this/project")
   julia> Pkg.instantiate()
   ```

This will install all necessary packages for you to be able to run the scripts and
everything should work out of the box.  See the `.jmd` scripts in the `scripts`
directory for methods to access data, fit models, etc.
