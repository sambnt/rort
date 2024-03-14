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

## Tips
https://vulkan.org/user/pages/09.events/vulkanised-2024/vulkanised-2024-charles-giessen-2-lunarg.pdf
 - Use profiler.
 - Measure using milliseconds, not FPS.
 - Add ImGUI.
 - Use deletion queue to manage cleanup
 - Probably don't need to use all available VkQueues.
 - Understand VkPresentModeKHR.
 - Use Dynamic Rendering extension.
 - Don't prioritize smooth swapchain resizing (oops).

## Issues

### The function pointer for "..." is null

```
*** Exception: invalid argument (The function pointer for vkCmdPipelineBarrier2 null)
```

Make sure you specifying a recent enough version of Vulkan in your `VkSettings`:

```
  cfg = VkSettings { requiredExtensions =
                       windowExts <> Vector.fromList []
                   , requiredValidationLayers =
                       Vector.fromList [ "VK_LAYER_KHRONOS_validation" ]
                       -- Vector.fromList [ "VK_LAYER_RENDERDOC_Capture" ]
                   , applicationInfo =
                       Vk.ApplicationInfo
                         (Just "Example: Buffer")  -- application name
                         (Vk.MAKE_API_VERSION 1 0 0) -- application version
                         (Just "No engine")          -- engine name
                         (Vk.MAKE_API_VERSION 1 0 0) -- engine version
                         (Vk.MAKE_API_VERSION 1 3 0) -- Vulkan API version (patch version ignored)
                   }
```


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
