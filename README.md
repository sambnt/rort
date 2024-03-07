# Rendering Engine using Haskell and Vulkan

Streamed live @sambnt_tv <3

```
nix build .#rort
```

Cross-compile for Windows:

```
nix build .#rort-win64
```

For distribution to other machines:

```
nix build .#hydraJobs.dist-linux64
nix build .#hydraJobs.dist-win64
```

Enter development shell:

```
nix develop
cabal run exe:rort
```

## Issues

### Missing GLIBC version

When trying to use the "VK_LAYER_RENDERDOC_Capture" validation layer, you may encounter the following error:

```
*** Error: Layer not found
```

If you re-run the program with `VK_LOADER_DEBUG=layer` set, you'll see:

```
Missing GLIBC_2.38
```

The system isn't missing GLIBC, there is just a mismatch in GLIBC version between your program and your system.

For example, check the version of glibc the program builds with:
```
nix repl
> :lf .#
> legacyPackages.x86_64-linux.glibc
«derivation ...-glibc-2.38-27.drv»
```

Then check the same for your system config:
```
nix repl
> :lf .#
> legacyPackages.x86_64-linux.glibc
«derivation ...-glibc-2.38-44.drv»
```

As long as the major and minor versions are the same, you should have no problem using the RenderDoc layer.

If there is a significant difference, upgrade either your system or the project to correct this.

This problem is isolated to the way Vulkan loads validation layers, it shouldn't affect normal usage of the program.
