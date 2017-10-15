package dotty.tools.dotc

import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.SymUtils._
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
  private def classes(implicit ctx: Context): immutable.Set[ClassSymbol] =
    immutable.Set(defn.RangeClass)

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

    override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (methods contains tree.symbol) tree.qualifier.select(buildProxySymbol(tree.symbol))
      else tree
    }

  }

  class EnclosureTransform(newOwner: Symbol) extends MiniPhaseTransform {
    override def phaseName: String = "enclosureTransform"

    override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
      println(tree)
      val oldSym = tree.symbol
      val newSym = ctx.newSymbol(
        newOwner,
        oldSym.name.asTermName,
        oldSym.flags,
        oldSym.info,
        coord = oldSym.pos
      )
      DefDef(newSym, tree.rhs)
    }

    override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree = {

      val tr = Ident(TermRef(NoPrefix, tree.symbol.asTerm))
      println(tr)
      tr
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

    private def copyMethod(oldSym: Symbol, copySym: TermSymbol, rhs: Tree)(implicit ctx: Context, info: TransformerInfo): DefDef = {
      // changeOwner needed for things like defDef, etc
      // subst needed for the Return value, although I might want to actually transform the
      // Return statement instead since it seems like the name of that symbol is still `foreach`
      val tree = rhs.changeOwner(oldSym, copySym).subst(oldSym :: Nil, copySym :: Nil)

      polyDefDef(
        copySym,
        ttypes =>
          vparamss =>
            // For some reason, changing the owner is not enough, we also have to replace any references
            // to parameters with the ones given to us above.
            new TreeTypeMap(treeMap = {
              case d: Ident =>
                val thing = vparamss.flatten.find({
                  case vparam: Ident =>
                    vparam.name == d.name // TODO: find more legit way of doing this...
                  case bro => false
                })
                thing match {
                  case Some(other) => ref(other.symbol)
                  case None => d
                }
              case tree => tree
            }).transform(tree)
      )
    }

    override def transformTemplate(tree: Template)(implicit ctx: Context,
                                                   info: TransformerInfo): Tree = {
      val cls = ctx.owner.asClass
      if (classes contains cls) {
        val body = tree.body.flatMap {
          case subTree: DefDef if methods contains subTree.symbol =>
            subTree :: methToProxies(subTree.symbol).map(copyMethod(subTree.symbol, _, subTree.rhs)).toList
            // println(subTree)
            // subTree :: Nil
          case subTree => subTree :: Nil
        }
        cpy.Template(tree)(body = body)
      } else tree
    }
  }
}

