package dotty.tools.dotc

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.collection.immutable
import scala.collection.mutable

/**
  * Optimizes a subset of for-loops.
  */
class ForOpt extends MiniPhaseTransform {
  import tpd._
  override def phaseName: String = "forOpt"

  // Map of classes that contain methods we will be specializing
  private def clsMethods(implicit ctx: Context) = immutable.Map(
    defn.RangeClass -> immutable.Set(defn.Range_foreach)
  )
  // TODO: how would I make this a lazy val (implicit context doesn't allow me to)
  private def methods(implicit ctx: Context) = clsMethods.flatMap(_._2).toSet

  private val foundMethods = mutable.Set[Symbol]()

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (methods contains tree.symbol) foundMethods.add(tree.symbol)
    tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree
  }

  private def proxyMethod(target: DefDef)(implicit ctx: Context): DefDef = {
    val oldSym = target.symbol
    val proxySym = ctx.newSymbol(
      oldSym.owner,
      target.name ++ "$proxy",
      Synthetic | Method,
      // Q: What does "widen" and "unstable" mean?
      MethodType(Nil, target.tpe.widenIfUnstable),
      coord = target.pos
    )
    val tparamSyms: List[TypeSymbol] = target.tparams.map(_.changeOwner(oldSym, proxySym).symbol.asType)
    val vparamss: List[List[ValDef]] = target.vparamss.map(_.map(_.changeOwner(oldSym, proxySym)))
    val vparamSymss: List[List[TermSymbol]] = vparamss.map(_.map(_.symbol.asTerm))

    val tree = DefDef(proxySym, target.tparams.map(_.symbol.asType),
      target.vparamss.map(_.map(_.symbol.asTerm)), target.tpe.widenIfUnstable,
      ref(target.symbol).appliedToArgss(vparamss))
    println(tree)
    tree
  }

  // NOTE: Order in which this is called seems to depend on what order the code is compiled in.
  //   If the Range class is transformed before the rest of the code, we will not be able to write
  //   the specialized methods in the class, since we can't know ahead of time, which ones we need.
  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val cls = ctx.owner.asClass

    if (cls == defn.RangeClass) {
      val body = tree.body.flatMap {
        case subTree: DefDef if subTree.symbol eq defn.Range_foreach => subTree :: proxyMethod(subTree) :: Nil
        case subTree => subTree :: Nil
      }
      cpy.Template(tree)(body = body)
    } else tree
  }
}
