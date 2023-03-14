package compilerPlugins

import dotty.tools.*
import dotc.*
import util.*
import reporting.*
import ast.Trees.*
import ast.tpd.Tree
import core.*
import Contexts.*
import Decorators.*
import StdNames.*
import plugins.*
/**
 * COMPILER PLUGINS
 *
 * Compiler plugins allow us to insert phases into the compiler to perform additional steps during
 * a compilation. These phases can "see" the state of the code as it is transformed from source to
 * bytecode via ASTs, and can even make modifications to it.
 */

/**
 * EXERCISE 1
 *
 * Let's find out about the compilation process. Run the `scalac` binary with the `-Vphases`
 * option. You should see a list of phases. The latest version of the compiler has 102 phases,
 * though some of them run concurrently.
 *
 * The first phase is the `parser`, which creates an untyped syntax tree from sources. The second
 * phase is `typer`, which constructs a typed AST and symbol table from the untyped AST. This is
 * by far the biggest (and slowest) phase. The last phase is bytecode generation (`genBCode`), which
 * creates .class files. Almost everything between `typer` and `genBCode` performs transformations
 * on typed ASTs, migrating them step-by-step from a structure that's close to source-code, to a
 * structure that's closer to bytecode instructions.
 *
 * The best documentation for compiler is the Dotty source code, at `github:lampepfl/dotty`. Clone
 * the repository and try searching for one of the phase names, in double-quotes: each one exists as
 * a string literal in the place it is defined.
 */

/**
 * EXERCISE 2
 * 
 * A compiler plugin can introduce one phase or many to the compilation pipeline. Each phase can
 * define where it should be executed, relative to other phases.
 *
 * To create a new plugin, we need to define a plugin class and at least one phase.
 *
 * 1. Add "org.scala-lang" %% "scala3-compiler" % "3.3.0-RC3" to your SBT build.
 * 2. Create a plugin phase class, `MyPhase`, which extends
 *    `dotty.tools.dotc.plugins.PluginPhase`
 * 3. Implement the `phaseName` -- pick a name!
 * 4. Create a plugin class, `MyPlugin`, which extends `dotty.tools.dotc.plugins.StandardPlugin`.
 * 5. Implement the missing methods. `init` should be a single-element `List` containing a new
 *    instance of the phase.
 * 6. Add a `println` with a welcome message to the `init` implementation
 * 7. Create a new file called `plugin.properties` in the project root, containing just one line:
 *    ```
 *    pluginClass=plugins.MyPlugin
 *    ```
 * 8. Package the compiler plugin as a JAR file with the `jar` command in the project root:
 *    ```
 *    mkdir -p lib
 *    jar cf lib/plugin.jar plugin.properties -C target/scala-3.3.0-RC3/classes plugins
 *    ```
 * 9. Finally, launch the Scala REPL, setting the plugins directory to `lib`:
 *    ```
 *    scala -Xpluginsdir lib
 *    ```
 *    Make sure you use the same version of Scala as you used to compile the plugin!
 *
 * Now, every line you compile in the REPL should print the welcome message. We have now created
 * a very basic compiler plugin which we can modify, recompile, repackage, and test with the REPL.
 *
 * Try making a change to the welcome message a couple of times to test this iterative process.
 */

//class MyPlugin() extends dotty.tools.dotc.plugins.StandardPlugin
//class MyPhase() extends dotty.tools.dotc.plugins.PluginPhase

/**
 * EXERCISE 3
 *
 * We are now going to explore what is available to a compiler plugin.
 *
 * Modify `MyPhase` to override the `runsBefore` and `runsAfter` members:
 * ```
 * override val runsBefore = Set("typer")
 * override val runsAfter = Set("parser")
 * ```
 * This will ensure our plugin is run as early as possible -- before the typer.
 *
 * And also,
 * ```
 * override def transformUnit(tree: Tree)(using Context): Tree = tree
 * ```
 *
 * Within the body of `transformUnit` we can explore, but we need to use tab-completion to navigate
 * the API. Take a look at the members of the following values inside `transformUnit`, such as:
 *  - `ctx`
 *  - `ctx.compilationUnit`
 *  - `ctx.compilationUnit.source`
 *  - `ctx.compilationUnit.untpdTree`
 *  - `ctx.compilationUnit.tpdTree`
 *  - `ctx.implicits`
 * and print them out.
 *
 * Now try modifying `runsBefore` and `runsAfter` to have the phase run at a different time. How
 * have the values changed?
 */
type Dummy = Unit