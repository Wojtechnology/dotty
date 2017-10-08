package dotty.tools.dotc

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.collection.immutable
import scala.collection.mutable

/**
  * Optimizes a subset of for-loops.
  */
object ForOpt {
  import tpd._

  // List of classes that contain methods in `methods`. Only templates of this class will have
  // proxy methods rewritten.
  private def classes(implicit ctx: Context) = immutable.Set(defn.RangeClass)

  // List of methods for which we want to create proxy methods
  private def methods(implicit ctx: Context) = immutable.Set(defn.Range_foreach)

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

    override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
      val sym = tree.symbol
      if (methods contains sym) {
        def buildApply(tree: Tree): Tree = tree match {
          case tApply: TypeApply => buildApply(tApply.fun).appliedToTypeTrees(tApply.args)
          // TODO: Is it possible for this method to be returned from another method?
          case apply: Apply => buildApply(apply.fun).appliedToArgs(apply.args)
          case select: Select => select.qualifier.select(buildProxySymbol(sym))
          // TODO: Figure out Ident case, when necessary
          case _ =>
            assert(false, s"Invalid tree of type ${tree.getClass}")
            EmptyTree
        }

        buildApply(tree)
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

    override def transformTemplate(tree: Template)(implicit ctx: Context,
                                                   info: TransformerInfo): Tree = {
      val cls = ctx.owner.asClass

      if (cls == defn.RangeClass) {
        val body = tree.body.flatMap {
          case subTree: DefDef if methods contains subTree.symbol =>
            methToProxies(subTree.symbol).map(
              proxyMethod(subTree.symbol, _)).toList ::: subTree :: Nil
          case subTree => subTree :: Nil
        }
        cpy.Template(tree)(body = body)
      } else tree
    }
  }
}

