package dotty.tools.dotc

import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.collection.immutable
import scala.collection.mutable

/**
  * Optimizes a subset of for-loops.
  */
object ForOpt {
  import tpd._

  private def getSymbol(name: PreName)(implicit ctx: Context): Option[Symbol] = {
    ctx.base.staticRef(name.toTypeName).symbol match {
      case NoSymbol => None
      case x => Some(x)
    }
  }

  // List of classes that contain methods in `methods`. Only templates of this class will have
  // proxy methods rewritten.
  private def classes(implicit ctx: Context): immutable.Set[ClassSymbol] =
    getSymbol("com.wojtechnology.collection.Range") match {
      case Some(x) if x.isClass => immutable.Set(x.asClass)
      case x => immutable.Set()
    }

  // List of methods for which we want to create proxy methods
  private def methods(implicit ctx: Context): immutable.Set[Symbol] = {
    classes.map(cls => cls.requiredMethodRef(nme.foreach).symbol)
  }

  // Only do optimization for specified packages
  private def whitelistedPackages(implicit ctx: Context): immutable.Set[Symbol] = {
    getSymbol("com.wojtechnology") match {
      case Some(x) => immutable.Set(x)
      case None => immutable.Set()
    }
  }

  // Contains map from the method symbol to all proxy symbols
  private val methToProxies = new mutable.HashMap[Symbol, mutable.Set[TermSymbol]]() {
    override def default(key: Symbol) = mutable.Set()
  }

  // Appended to end of proxy methods to guarantee uniqueness
  private var counter: Int = 0

  class ForOptCollect extends MiniPhaseTransform {
    override def phaseName: String = "forOptCollect"

    private def buildProxySymbol(oldSym: Symbol)(implicit ctx: Context): Symbol = {
      val proxySym = ctx.newSymbol(
        oldSym.owner,
        (oldSym.name ++ ("$proxy" + counter)).asTermName,
        Synthetic | oldSym.flags,
        oldSym.info,
        coord = oldSym.pos
      )
      counter += 1
      methToProxies.get(oldSym) match {
        case Some(s) => s += proxySym
        case None => methToProxies += (oldSym -> mutable.Set(proxySym))
      }

      proxySym
    }

    override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
      val sym = tree.symbol
      // TODO: how to get corresponding object for a class (i.e. Test$ from Test)
      if (methods.contains(sym) && whitelistedPackages.contains(ctx.owner.enclosingClass.owner)) {
        tree.qualifier.select(buildProxySymbol(tree.symbol))
      } else tree
    }

  }

  class ForOptTransform extends MiniPhaseTransform {
    override def phaseName: String = "forOptTransform"

    private def proxyMethod(oldSym: Symbol, proxySym: TermSymbol)(implicit ctx: Context): DefDef = {
      polyDefDef(
        proxySym,
        ttypes =>
          vparamss =>
            ref(oldSym).appliedToTypes(ttypes).appliedToArgss(vparamss)
      )
    }

    private def copyMethod(oldTree: DefDef, copySym: TermSymbol)(implicit ctx: Context, info: TransformerInfo): DefDef = {
      val oldSym = oldTree.symbol

      // changeOwner needed for things like defDef, ident, etc
      // Might also want to run subst, but works without it for now. It was my previous solution
      // to changing the Return statement below
      val rhs = oldTree.rhs.changeOwner(oldSym, copySym)

      polyDefDef(
        copySym,
        ttypes =>
          vparamss => {
            // For some reason, changing the owner is not enough, we also have to replace any references
            // to parameters with the ones given to us above.
            // TODO: may also need to do this for ttypes (although probably not, since types can be copied (?))
            val vparamssMap =
              oldTree.vparamss.flatten.map(_.symbol).zip(vparamss.flatten.map(_.symbol)).toMap
            new TreeTypeMap(treeMap = {
              case ident: Ident =>
                vparamssMap.get(ident.symbol) match {
                  case Some(newVparam) => ref(newVparam)
                  case None => ident
                }
              case rtn: Return =>
                Return(rtn.expr, ref(copySym))
              case other => other
            }).transform(rhs)
          }
      )
    }

    override def transformTemplate(tree: Template)(implicit ctx: Context,
                                                   info: TransformerInfo): Tree = {
      val cls = ctx.owner.asClass
      if (classes contains cls) {
        val body = tree.body.flatMap {
          case subTree: DefDef if methods contains subTree.symbol =>
            subTree :: methToProxies(subTree.symbol).map(copyMethod(subTree, _)).toList
          case subTree => subTree :: Nil
        }
        cpy.Template(tree)(body = body)
      } else tree
    }
  }
}

