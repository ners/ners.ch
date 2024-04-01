# Reproducibility

Reproducibile builds are an often-cited benefit of the [[Nix]] ecosystem. But what is reproducibility really?

> Reproducible builds are a set of software development practices that create an independently-verifiable path from source to binary code.
>
> A build is reproducible if given the same source code, build environment and build instructions, any party can recreate bit-by-bit identical copies of all specified artifacts.
> :::cite
> [Reproducible builds project](https://reproducible-builds.org/)

In other words, by building software reproducibly, we have strong guarantees that a task that works today will continue to work tomorrow.

## Repeatability

Any task that has mutiple steps, especially laborious or error-prone ones, which must be performed more than once, benefits from a transfer of knowledge between those who perform it.

These knowledge transfers happen in many ways:
 - verbal instructions
 - written notes (e.g. in README files)
 - code (e.g. shell scripts, Makefiles, containerfiles, ...)

The desired goal is that if the instructions are carefully followed, the same process in the same conditions will yield the same (or at least, predictable) results.

Repeatability is a *necessary*, but not *sufficient*, condition for reproducibility:
 - [Matthew Croughan - Use flake.nix, not Dockerfile - MCH2022](https://youtu.be/0uixRE8xlbY)

## Input hashing

Most tasks depend on external resources (e.g. FOSS projects) that we call inputs.
Even the *task instructions themselves* are an input that we depend on.

If we cannot guarantee that the *exact same* inputs will be used for every repetition of the task, then we cannot guarantee that the outcome of the task will always be the same.

We can compute cryptographic hashes of our inputs, and then depend on inputs with exact known hashes.
This is called *source pinning*.

## Hermetic build environment

To ensure that we perform our task only with pinned inputs, we must perform it in an environment that does not give us access to other resources.

For example, a hermetic build environment should not have access to the Internet, hardware devices, or software which may exist on the system, but has not been declared as an input to our build.

- When building software with Bazel, we can pin most toolchains, but [not the C/C++ toolchain][bazel-c], which it takes from the system.
- When building OCI container images, the OCI build environment does not guarantee that our transformations are reproducible.
  It is quite common to build container images using Linux package managers such as Apt, which are *not* reproducible.
  The same container image built at a different time will have different contents!

## Output hashing

If a task has been performed with the exact same inputs, then we may reasonably expect it to yield exact same outputs.

Similar to inputs, we may hash the outputs to verify that this is the case.

This is a great idea in principle, but in practice some tools may introduce [timestamps] to the outputs, which will differ between builds despite being performed in a hermetic environment.

[bazel-c]: https://blog.aspect.dev/hermetic-c-toolchain
[timestamps]: https://reproducible-builds.org/docs/timestamps/
