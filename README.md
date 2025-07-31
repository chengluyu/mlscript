# Artifact Documentation for A Lightweight Type-and-Effect System for Invalidation Safety

## 1. Overview of the Artifact

Our paper introduces a new type system, InvalML, for permanent and temporary invalidation. In the paper, we propose a type inference algorithm for InvalML and prove its soundness and completeness.

This artifact implements InvalML and the type inference algorithm based on the MLscript language. We reuse the infrastructure of the MLscript language, providing InvalML as an alternative type system and type inference algorithm, along with some necessary syntax and code generation extensions. Our artifact consists of two parts:

1. The type checker, written in Scala, can be found in an `invalml` folder in the MLscript compiler source.
2. The test cases for InvalML and the inference algorithm can be found in an `invalml` folder in the MLscript tests.
3. The [web demo](https://anonymous8538.github.io/780/) offers a convenient way to compile and run InvalML programs directly in the browser, eliminating the need for the toolchain dependencies. The results of compilation, such as type checking, can be viewed in the web demo.

The type checker is our main contribution, while the web demo illustrates the reusability of our project. We do not provide a command-line tool specifically for compiling an MLscript file.

This documentation contains the evaluation instructions for the type checker and the web demo. We also introduce the implementation of our algorithm and provide a user guide for the web demo.

### 1.1 List of Claims

Most claims in the paper can be verified in the test suite. One can modify the examples in the test cases or try them out in the web demo. The lines of code for the implementation we claimed in the submitted paper are not accurate now. We will put the real number in the paper’s revision.

1. The implementation can handle the examples in the paper, which can be found in both `hkmc2/shared/src/test/mlscript/invalml/web-demos/ExamplesInThePaper.mls` and the web demo.
2. The paper proposes some extensions to InvalML. These are supported by the implementation. One can check the following test files or find them in the web demo:
    - Scope-safe metaprogramming: `hkmc2/shared/src/test/mlscript/invalml/web-demos/Staging.mls`
    - Exceptions: `hkmc2/shared/src/test/mlscript/invalml/web-demos/Exception.mls`
    - Stack-based memory management: `hkmc2/shared/src/test/mlscript/invalml/web-demos/StackMM.mls`
3. We also include several case studies that have not been mentioned in the paper. One can check the following test files or find them in the web demo: 
    - A simple constraint solver: `hkmc2/shared/src/test/mlscript/invalml/web-demos/SimpleConstraintSolver.mls`
    - Parallel merge sort algorithm: `hkmc2/shared/src/test/mlscript/invalml/web-demos/reml/MergeSort.mls`
    - An interpreter for arithmetic and boolean expressions: `hkmc2/shared/src/test/mlscript/invalml/web-demos/flix/Interpreter.mls`
    - A simple GUI example that requires an event listener not blocking the thread: `hkmc2/shared/src/test/mlscript/invalml/web-demos/flix/GUI.mls`

### 1.2 Syntax Differences

There are some minor differences in the syntax of the artifact, as compared to the paper:

- The artifact uses `|`, `&`, and `~` rather than the paper's stylized $\vee$, $\wedge$, and $\neg$ respectively
- The artifact uses `region r in ...` rather than the paper's `region r; ...`
- The artifact uses syntax like `[outer, T, E extends outer] -> T ->{E} T` rather than the paper's $\forall \omega, T, E \{E \le \omega\}.\, T \stackrel{E}{\to} T$
- The artifact uses the following syntax to declare ADTs:
    
    ```fsharp
    class List[T] with
      constructor
        Nil
        Cons(x: T, xs: List[out T])
    ```
    
    Which is equivalent to the syntax used in the paper:
    
    ```scala
    enum List[T]:
      case Nil
      case Cons(x: T, xs: List[out T])
    ```
    

### 1.3 Hardware Requirements

This artifact does not require any specific hardware to run. A normal computer with Java 11 and Node.js 24 can handle the project from scratch, and a computer with Docker can complete the artifact evaluation by using the Docker image. Devices connected to the Internet with modern browsers can access the web demo.

### 1.4 Reusability Guide

The project can be reused as follows:

1. The project can be extended directly. One can modify the corresponding parts to extend the system and the whole compiler, as we will explain in Section 3.
2. The web demo shows the other approach to reusing the project. The project can be compiled to JavaScript via [Scala.js](https://www.scala-js.org/), which produces an artifact that the web demo uses. This demonstrates that the project can be invoked by other programs to output both intermediate and final results of the compilation.

## 2. Getting Started

This section displays the instructions for setup. There are three ways of evaluating the artifact, and we provide a detailed guide for each of them.

### 2.1 Running the Project (Using Docker)

Docker images are prepared with all necessary dependencies to compile the project. The following instructions are for `amd64`. If you are using `arm64`, you need to substitute `invalml` in the following commands with `invalml-arm64`.

1. Run the command `docker load < invalml.tar.gz` to load the Docker image.
2. Run the command `docker run -it -p 8080:8080 --rm mlscript/invalml` to launch a temporary container. You can substitute the second `8080` with another port number that is available on your device (e.g., you can run `docker run -it -p 8080:3154 --rm mlscript/invalml`) if the 8080 port is occupied.
3. Run the following commands to execute all tests:
    
    ```bash
    cd /home/ubuntu/mlscript
    sbt hkmc2AllTests/test
    ```
    
    All tests should pass. The generated web demo can be found at `/home/ubuntu/web-demo`. To use the web demo locally, please refer to Section 2.3.
    
4. After executing the tests, you can exit the container. The container will be destroyed automatically.

### 2.2 Running the Project (From Scratch)

To build the project from scratch, please follow the steps below to ensure that all dependencies are installed correctly.

1. We recommend that you use [Coursier](https://get-coursier.io/) to set up the Scala development environment. You can follow [these instructions](https://get-coursier.io/docs/cli-installation) to install Coursier on your device. You may need to install Java manually on some platforms. After the installation of Coursier, you can run the command `cs install sbt scala` to install **sbt** and **Scala**. Other approaches to installing **Java**, **sbt**, and **Scala** should also work. Please ensure you have Java 11 or above.
2. Please install [Node.js 24](https://nodejs.org/en) and **npm** (Node Package Manager). If there are already other versions of **Node.js** on your device, you can consider using a version manager like [nvm](https://github.com/nvm-sh/nvm). Please ensure the node executable is in the `PATH` so that the tests can be executed correctly.

Then, you can download and unpack the artifact. Suppose that the artifact is unpacked at `mlscript/`. Please create a folder called `web-demo` at the same location as where the `mlscript` folder is. Tests can be executed as follows:

1. Run `sbt` at `mlscript/`. An interactive shell of **sbt** will be started.
2. Run the command `hkmc2JS/fullLinkJS` in the sbt shell. It will compile the MLscript compiler and generate the corresponding JavaScript files.
3. Run the command `hkmc2AllTests/test` in the sbt shell. It will execute all the tests and generate the web demo. To use the web demo locally, please refer to Section 2.3.
4. All tests should be passed, and the web demo files should be available at `web-demo`.
5. After executing the tests, you can run `exit` to exit the interactive shell.

### 2.3 Running the Web Demo

The web demo can be accessed via the link https://anonymous8538.github.io/780/. It is designed to work on desktop computers and laptops. Please ensure that JavaScript is enabled.

If you have already compiled and executed the test from scratch/in Docker, you can also access the web demo locally. For example, if you are using VSCode, you can install the extension [Live Server](https://marketplace.visualstudio.com/items?itemName=ritwickdey.LiveServer) to start a server on your device, or use [http-server](https://www.npmjs.com/package/http-server) if you are using another editor. In the Docker image, the http-server is already installed. You can run the command `cd /home/ubuntu/web-demo && http-server .` to start the server. If you create the container by using the command `docker run -it -p 8080:x --rm mlscript/invalml`, you should be able to access the web demo via the link `http://127.0.0.1:x/`, where x is the port number you pick.

You may refer to Section 4 for more details about the web demo.

## 3. Introduction to the Project

### 3.1 Project Structure

Our artifact is based on MLscript. The structure of the project is shown as follows:

- `hkmc2/shared/src`
    - `main/scala`
        - `hkmc2/invalml`: InvalML type checker implementation.
        - Other subdirectories: original implementations for MLscript.
    - `test`
        - `mlscript/invalml`: InvalML type checking test files.
        - Other subdirectories: test files for other implementations of MLscript.
- `hkmc2DiffTests/src/test/scala/hkmc2`: test framework for the current version of MLscript.
- Other directories: other subprojects and legacy MLscript components.

The files in `hkmc2/invalml` are the implementation of our type inference algorithm, and `mlscript/invalml` contains the corresponding test files. In the following subsections, we introduce how these files are related to the system in the paper. For the remaining files, we only introduce the parts that we’ve modified and how we extended the existing MLscript compiler to support our system. The introduction to other parts is omitted for simplicity.

### 3.2 Correspondence between Paper and Implementation

The implementation of InvalML and its type checker is located at directory `hkmc2/shared/src/main/scala/hkmc2/invalml/`. 

| Definition | Location in the Paper | Source Code Files |
| --- | --- | --- |
| Syntax of types | Fig. 1 in Section 3.1 | `hkmc2/shared/src/main/scala/hkmc2/invalml/types.scala` |
| Type inference algorithm | Fig. 6 in Section 4.1 | `hkmc2/shared/src/main/scala/hkmc2/invalml/InvalML.scala` |
| Normal forms | Section 4.2 | `hkmc2/shared/src/main/scala/hkmc2/invalml/NormalForm.scala` |
| Skolem bound inlining function, constraint solver, and extrusion | Fig.7 in Section 4.3, Fig. 8 in Section 4.3, and Fig. 9 in Section 4.4 | `hkmc2/shared/src/main/scala/hkmc2/invalml/ConstraintSolver.scala` |

Other auxiliary definitions that are not mentioned in the paper are given in the same directory. These files mainly serve for type output.

| Definition | Source Code Files |
| --- | --- |
| Type pretty-printer | `hkmc2/shared/src/main/scala/hkmc2/invalml/PrettyPrinter.scala` |
| Simple type simplifier | `hkmc2/shared/src/main/scala/hkmc2/invalml/TypeSimplifier.scala` |
| Type traverser | `hkmc2/shared/src/main/scala/hkmc2/invalml/TypeTraverser.scala` |

Besides, we also changed several files originally belonging to the MLscript compiler to support corresponding parsing and code generation.

| Definition | Location in the Paper | Source Code Files |
| --- | --- | --- |
| Term Syntax | Fig. 1 in Section 3.1 | `hkmc2/shared/src/main/scala/hkmc2/syntax/*.scala` |
| Semantics | Fig. 18 in Appendix B.4 | `hkmc2/shared/src/main/scala/hkmc2/codegen/Lowering.scala` |

### 3.3 Running Tests

The InvalML tests specifically can be run by first running the SBT command `hkmc2JVM/test` once (to compile MLscript’s standard library), and then running the SBT command `hkmc2DiffTests/testOnly -- -z invalml` as many times as desired. (The command `hkmc2AllTests/test` is for all tests of the current MLscript implementation, including the standard library and InvalML tests.)

The suffix of test files is `.mls`. Test cases are separated from each other by empty lines. For instance, the following code has three blocks of test cases:

```fsharp
fun fork: [A, B extends ~A, T1, T2] -> (() ->{A} T1, () ->{B} T2) ->{A | B} Pair[out T1, out T2]
fork
//│//│ Type: ['A, 'B, 'T1, 'T2] -> (() ->{'A} 'T1, () ->{'B} 'T2) ->{'A ∨ 'B} Pair[out 'T1, out 'T2] Type: ['A, 'B, 'T1, 'T2] -> ((⊤) ->{'A} 'T1, (⊤) ->{'B} 'T2) ->{'A ∨ 'B} Pair[out 'T1, out 'T2]
//│ Where:
//│   'B <: ¬'A

:e
region x in
  fork((() => x.ref 1), (() => x.ref 2))
//│ ╔══[ERROR] Type error in function literal with expected type () ->{'B} 'T2
//│ ║  l.24: 	  fork((() => x.ref 1), (() => x.ref 2))
//│ ║        	                               ^^^^^^^
//│ ╟── because: cannot constrain  'reg  <:  'B
//│ ╟── because: cannot constrain  x  <:  'B
//│ ╟── because: cannot constrain  x  <:  ¬'A
//│ ╟── because: cannot constrain  'A  <:  ¬x
//│ ╙── because: cannot constrain  x  <:  ¬x
//│ Type: Pair[out Ref[Int, ?], out Ref[Int, ?]]

region x in
  region y in
    fork((() => x.ref 1), (() => y.ref 2))
//│ Type: Pair[out Ref[Int, ?], out Ref[Int, ?]]
```

The test output of each test block, starting with `//|`, is appended to the test code. Modern code editors like VSCode can automatically reload the changes from the disk. The test output shows the inferred type of the given program and type errors (if any). The test flag`:e` indicates that the type errors that occurred are expected. In the second example above, one cannot modify the same region in parallel. Therefore, our system rejects the program by raising a type error saying that `cannot constrain x <: ¬x`.

You can use the watch mode by running the command `~hkmc2DiffTests/Test/run` in the `sbt` interactive shell instead. In watch mode, you can modify the test files, and the compiler will detect the modification automatically, recompiling the modified files for you. You can also use incantation `~; hkmc2JS/fullLinkJS; hkmc2JVM/test; hkmc2DiffTests/testOnly -- -z BuildWebDemo` to automatically rebuild all relevant source code and regenerate the web demo on change.

### 3.4 Other Components of The Project

This subsection briefly introduces the other important components in the project. For simplicity, we omit the introduction to legacy compiler code and subprojects, focusing on the main infrastructure that our implementation depends on. All the paths mentioned below are relative to `hkmc2/shared/src/main/scala/hkmc2`.

- Lexing and parsing: The lexer (`syntax/Token.scala`) consumes the input string and generates tokens, defined in `syntax/Token.scala`. The parser (`syntax/Parser.scala`) handles the tokens according to the parse rules defined in `syntax/ParseRule.scala` and generates a parsed tree. The structure of parsed trees is given in `syntax/Tree.scala`.
- Elaboration: The elaborator (`semantics/Elaborator.scala`) accepts a parsed tree, generating symbols (`semantics/Symbol.scala`) and handling the symbol references. It elaborates the given parsed tree into terms (`semantics/Term.scala`) that can be type-checked or lowered later. The pattern-matching syntax of MLscript, called [ultimate conditional syntax](https://dl.acm.org/doi/abs/10.1145/3689746), is also desugarred in this stage. The process is defined in files `semantics/ucs/*.scala`.
- Lowering and code generation: To support various backends, MLscript adopts a block IR (`codegen/Block.scala`). An elaborated term needs to be lowered (`codegen/Lowering.scala`) to the block IR before the code generation. Such blocks can later be translated to other languages, like JavaScript (`codegen/js/JSBuilder.scala`) and C++ (`codegen/cpp/*.scala`).

### 3.5 Compatibility Check

We have tested two methods on the following operating systems and Docker versions.

|  | Using Docker | Starting from scratch |
| --- | --- | --- |
| macOS | Test passed with Docker v28.2.2 | Test passed |
| Arch Linux | Test passed with Docker v28.3.0 | Test passed |

## 4. Guide to the Web Demo

### 4.1 User Interface

![screenshot.webp](screenshot.webp)

The user interface of the web demo, as shown in the above screenshot, consists of two parts: the edit area and the output area. In the left-hand side’s edit area, one can write MLscript programs or load the built-in examples via the drop-down box. Once the program is done, you can click the compile button to compile the program, and the results of each stage can be inspected in the right-hand side’s output area.

- The typer tab shows the type of the whole program inferred by our algorithm. If there is any type error, it will be displayed in the diagnostics area. The unit type will be omitted for simplicity.
- The parser tab shows the parsed tree of the given program. You can click on nodes to fold/unfold them.
- The elaborator and resolver tabs display the result of elaboration. The former prints the elaborated terms with the original names used in the parsed trees. The latter substitutes those names with resolved symbol references.
- The lowering tab shows the lowered blocks, and the code generation tab displays the generated JavaScript code. Some test cases might raise errors in the code generation tab. This is because we do not provide implementations for some functions, and they are mainly used for type-checking tests. You may ignore these errors if you see them.

### 4.2 Compatibility Check

We have tested the web demo on the following operating systems and browsers. The table shows the version of the browsers on different operating systems.

|  | macOS | Arch Linux |
| --- | --- | --- |
| Apple Safari | 18.5 (20621.2.5.11.8) | N/A |
| Google Chrome | 138.0.7204.49 | 138.0.7204.49 |
| Microsoft Edge | 137.0.3296.83 | Did Not Test |
| Firefox | Did Not Test | 140.0.1 |
