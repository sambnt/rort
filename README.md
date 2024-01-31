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
