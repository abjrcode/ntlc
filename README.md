# NTLC - Neo Typed Lambda Calculus

I wanted to build a compiler from scratch that has type checking support, native code generation, and basic IDE integration through a language server. I also wanted to learn Rust at the same time _(who thought that's a good idea)_ so I used it to build the compiler.

In order to cover all these aspects -- the language we are building is very restricted and very basic. It is inspired by "Untyped Arithmetic Expressions" from the book "Types and Programming Languages" by Benjamin C. Pierce.

[I have written a series of blog posts](https://madin.dev/ntlc/intro/) that accompany this project and explain the various stages of the compiler and how it works.

The code is also heavily commented and assumes almost no knowledge of Rust.

Needless to say _(but here I am saying it)_, the code is not idiomatic Rust nor is it intended to be performant or used for anything practical except learning. But, it works and it has automated tests.

# Requirements & Building

The project requires quite some dependencies, so I added a `Dockerfile` to make it easier to build and run the project without having to install all of them.

## The Docker Way

If you are using VSCode, there is an associated `devcontainer.json` file that will allow you to open the project in a container and have all the dependencies installed for you.

I recommend this approach since just building the compiler inside the container will mean the compiler will generate native code that matches the container environment itself.

## The Native Way

If you would still like to build natively then you will need the following _(consult the Dockerfile to get a good idea for what might be missing if you run into issues)_:

-   `Rust 1.72.1 (eb26296b5 2023-08-03)` or later _(I recommend to use [rustup](https://rustup.rs/) to install and manage Rust)_.
-   NodeJS 18.x.x _(needed for VSCode Language Server TypeScript client)_
-   LLVM 16.x.x _(needed for the LLVM backend, you need a distribution that ships with `llvm-config` which most binary distribution don't have)_

Then run the following:

1. `npm install`
   This will install the node modules for the VSCode Language Server client.
    1. If you want to check the VSCode integration, you will find a launch configuration called `Launch Client` in the VSCode debugger.
2. `cargo run`
   This will build the compiler and run it on the input file `/examples/good.ntlc` producing a native executable which you can find in the `/bin` directory.
    1. The generated executable doesn't print any output but you can check its exit code which matches the evaluation result of the corresponding NTLC program.
    1. That can be done by running `echo $?` after running the compiler.

# How is the code organized?

-   `bin` directory contains the output of the compiler that we are building after it has been applied to the input file `/examples/good.ntlc`
-   `compiler` contains the source code of the NTLC compiler. Each stage of the compiler is in a separate file.
-   `examples` directory contains example NTLC programs _(barely)_.
-   `lsp-extension`: contains the source code of the VSCode Language Server TypeScript client.
-   `lsp-server`: contains the source code of the NTLC Language Server _(this is started automatically by the VSCode when it activates the extension)_.

# Feedback or Questions

Please open an issue
