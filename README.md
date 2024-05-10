# xia-lang
A statically typed, compiled functional language focused on ergonomics, ease of use and code composition.

## WARNING

This project has just started (04/22/2024), nothing is ready, everything is unstable. The first stage of development is going to use commit messages and marked code comments as its "issue tracker". Once there is some kind of basic structure to the project, I'll move to using GitHub issues and being more relentless about comments and documentation. For now, it will just slow me down.

## Why new language?

There are several reasons, but the biggest one is this. I have been looking for the **perfect** programming language for myself to fully focus on just developing the stuff I like, but I have unfortunately been unable to find such a language for myself. Nothing has fully clicked.

The two biggest candidates have been Rust and Haskell, but each of them have things I don't like. 

In case of Rust, I adore its enums, safety features and iterators (and speed, of course), but unfortunately the more tedious and frustrating parts like lifetimes, syntax bloat, lack of some convenient functional features like advanced pattern matching (trying to pattern match something other than an enum or tuple in Rust is a pain) and, most of all, its overly verbose handling of common functiontal types like Results and Options (.unwrap().unwrap().unwrap()) become frustrating the more you develop in Rust and the bigger your projects grow.

In case of Haskell, it can be incredibly succinct, type safe and clean, but its pure functionality leads to struggles that frankly should not even be a thing, sometimes making it a bit impractical to use. Plus, Haskell's toolchain and infrastructure are kind of a mess. Compilation is slow, Stack is messy, and the situation with some important dependency packages being available only on some specific versions of the compiler is just mind-boggling.

Therefore, Xia is being built to hopefully fix these issues. I'm a single developer, my life situation is unstable, my understanding of hyper-complex compiler issues is low, and therefore this is going to be a journey. However, I'm hoping against hope that it's going to be a good one.

## Explicit goals of the language

- A functional programming language
- Not a pure functional language like Haskell, embraces pragmatic practicality and code clarity (probably like ML languages?), therefore not completely forbidding side effects
- Statically typed, strong type system
- Type inference
- Compiled to native binaries (via LLVM and/or x64 ASM)
- Starting to use it is easy (low barrier of entry)
- Advanced usage is expressive, flexible and (hopefully) not a pain
- In general, the most important keywords for Xia are: `functional`, `practicality` and `code clarity`

## Explicit non-goals

- This language exists to satisfy the needs of a single person, me, therefore if I deem a feature not suitable for Xia, I don't implement it, period
- This language is not trying to be an exact copy of anything else

## Planned stages of development

- => [IN PROGRESS] Stage 0, REPL/Interpreter + interpreting files (not bytecode or native) - this stage is intended to last until the basics of language's syntax and architecture are ready. Thinking about more advanced backends would just slow me down at this stage.
- => Stage 1, VM/Bytecode: at this stage I'm going to implement a simple virtual machine that compiles Xia to bytecode, and this is probably going to be a somewhat longer stage of development, at least until I feel confident in all of the most basic features and syntax of the language (this stage is going to solidify and stabilize Stage 0 and make it execute faster).
- => Stage 2, implementing the LLVM backend: to make Xia compile to native binaries, I'll implement a backend for LLVM. Depending on how terribad slow LLVM compilation is, this may be the final stage before moving to self-hosted.
- => Stage 2.5 (potentially): implementing one or two additional backends like Windows and Linux x64 ASM. This is mainly for compilation speed (compared to LLVM).
- => Stage 3: Moving to self-hosted. This is going to be a tremendous endeavor for little old me, but may potentially be extremely rewarding and fun.
- => Stage 4: Final stage (for now): extending the standard library as far as possible, potentially making Xia a batteries-included language that allows you to start working on most types of programming tasks out of the box.
- => Stage 5 (potentially in parallel with Stage 4): toolchain (REPL and dependency manager): this part is planned to be as simple to use as possible. A single central repository is not planned for now. I'm planning to hopefully simplify the strategy of using GitHub/GitLab repos as dependencies. As long as the repository is another Xia project (determined via a simple markdown file and folder structure), the dependency manager (probably integrated into the Xia executable itself, not a separate one) should be able to just auto-fetch and build the dependency for you. Overcomplicated build systems like CMake etc. are not planned for now.

## The purpose

Answering the question of `What are you planning to use the language for?`, I would answer `for console/command line development`. I want the development of command line utilities in Xia to be as easy and seamless as possible. Plus, i want the REPL itself to be a command shell (similar to say Bash) where the developer could just spend all of their development time like they would in Bash. Making the shell Bash-compatible and yet a bit smarter and more modern would be ideal. The main "scripting language" and expression evaluation language in this shell is going to of course be Xia itself. The shell is going to be the main project written in Xia and a `live walking unit test and example base` for Xia, so to speak. The shell/REPL is planned to be written in Xia from start to finish, no other languages involved.

## The language(s) of implementation

The main language of implementation is going to be Haskell. However, when it's time to interface with more low level/systems features of the OS and language, some parts may be written in Rust instead and FFI'd into Haskell. Especially where LLVM is concerned. Also some of the more performance critical parts of the language may be written in Rust instead.

## Licensing

Xia is licensed under the standard MIT license. This means that provided you redistribute the text of the MIT license with this project's code, you can do whatever you want with it.