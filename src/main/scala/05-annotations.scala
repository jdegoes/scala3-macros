package macroAnnotations

/**
 * MACRO ANNOTATIONS
 *
 * In Scala, it has always been possible to annotate a variety of different parts of the syntax with
 * @-annotations. Annotations can be created as classes which extend `StaticAnnotation`, and syntax
 * can be annotated without any effect.
 *
 * Unfortunately, it was never so easy to _use_ those annotations. They could be read at runtime
 * with Java reflection, or at compile-time with macros. But if you wanted to use an annotation to
 * indicate code that should be *modified*, it meant writing a compiler plugin... until now!
 *
 * Macro annotations provide a convenient entry point which gives the programmer full access to the
 * AST of the annotated definition (class, trait, def, val, etc) in an ordinary method, and allows
 * us to return new definitions -- maybe a modification of the original, or maybe more than one
 * definition.
 *
 * Implementing a macro annotation is as simple as implementing a class with one abstract member:
 * ```
 * class MacroAnnotation:
 *   def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition]
 * ```
 */

import scala.quoted.*
import annotation.*
import scala.collection.mutable.HashMap

// This is an experimental feature, so we need to use the `@experimental` annotation!
// Our macro annotation will be used as `@memo()` in front of a single-parameter `def`. We implement
// it as a case class extending `MacroAnnotation`.
@experimental
case class memo() extends MacroAnnotation:
  
  // `MacroAnnotation` has a single abstract methad, `transform`. Note that `Quotes` is its first
  // parameter (and is contextual) since the type of the `tree` parameter is path dependent on the
  // `quotes` value that `Quotes` provides (and so is the return type).
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    
    // We import the familiar `quotes.reflect` package
    import quotes.reflect.*
    
    tree match
      
      // We start by matching on the type of definition. `DefDef` represents a `def`. The second
      // parameter matches exactly one parameter with one parameter block. We also bind the
      // return type and RHS of the `def`.
      case DefDef(name, TermParamClause(param :: Nil):: Nil, returnType, Some(rhs)) =>
        (Ref(param.symbol).asExpr, rhs.asExpr) match
          
          // We get a reference to the parameter and the RHS, then match on these as `Expr`s
          case ('{ $paramRef: paramType }, '{ $rhs: rhsType }) =>
            
            // We construct the type of the map which will hold the memoized values
            val cacheType = TypeRepr.of[HashMap[paramType, rhsType]]
            
            // We need to construct a symbol referring to a `val` which will hold the cache map
            // Note that we give it the same name with `Cache` appended, and we make it private.
            val cacheSymbol = Symbol.newVal(tree.symbol.owner, name+"Cache", cacheType, Flags.Private, Symbol.noSymbol)
            
            // We construct an expression of the initialization of the empty cache...
            val cacheRhs = '{ HashMap[paramType, rhsType]() }.asTerm
            
            // ...and construct a new `ValDef` representing a `val` which binds the `HashMap` to
            // the symbol we just created.
            val cacheVal = ValDef(cacheSymbol, Some(cacheRhs))
            
            // Now we construct a new reference to the `val` we just constructed
            val cacheRef = Ref(cacheSymbol).asExprOf[HashMap[paramType, rhsType]]

            // We will replace the RHS of our original method with a lookup in the memoization map.
            val newRhs = '{ $cacheRef.getOrElseUpdate($paramRef, $rhs) }.asTerm

            // We construct a new `DefDef` to replace the original, but based on the original `tree`
            // Note that the only change is that we use `newRhs` as the method implementation.
            val newTree = DefDef.copy(tree)(name, TermParamClause(param :: Nil) :: Nil, returnType, Some(newRhs))

            // We return two definitions: the `val` containing the cache map, and our rewritten
            // `def`.
            List(cacheVal, newTree)

      case _ =>
        // If the macro is applied to something other than a single-parameter `def`, then we report
        // an error.
        report.errorAndAbort("@memo() cannot be used here")

// A significant limitation of annotation macros is that any new definitions they introduce will not
// be visible to the typechecker, so it's not possible for code elsewhere to refer to definitions
// which aren't visible (statically) in the code.