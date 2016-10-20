/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

package firrtl

import com.typesafe.scalalogging.LazyLogging
import java.io.Writer
import Annotations._

import firrtl.ir.Circuit
import passes.Pass

/**
 * RenameMap maps old names to modified names.  Generated by transformations
 * that modify names
 */
case class RenameMap(map: Map[Named, Seq[Named]])

/** Current State of the Circuit
  * Includes the AST as well as extra information like annotations
  */
case class CircuitState(
  circuit: Circuit,
  form: CircuitForm,
  annotations: Option[AnnotationMap] = None,
  renames: Option[RenameMap] = None)

/** Current form of the Firrtl Circuit */
sealed abstract class CircuitForm(private val value: Int) extends Ordered[CircuitForm] {
  def compare(that: CircuitForm): Int = this.value - that.value
}
final case object ChirrtlForm extends CircuitForm(3)
final case object HighForm extends CircuitForm(2)
final case object MidForm extends CircuitForm(1)
final case object LowForm extends CircuitForm(0)

abstract class Transform {
  def name: String = this.getClass.getSimpleName
  def inputForm: CircuitForm
  def outputForm: CircuitForm
  def execute(state: CircuitState): CircuitState
  final def getMyAnnotations(state: CircuitState): Option[Map[Named, Annotation]] =
    for {
      annotations <- state.annotations
      myAnnotations <- annotations.get(this.getClass)
    } yield myAnnotations
}

trait SimpleRun extends LazyLogging {
  def runPasses(circuit: Circuit, passSeq: Seq[Pass]): Circuit =
    passSeq.foldLeft(circuit) { (c: Circuit, pass: Pass) =>
      val x = Utils.time(pass.name) { pass.run(c) }
      logger.debug(x.serialize)
      x
    }
}

/** For PassBased Transforms and Emitters
  *
  * @note passSeq accepts no arguments
  * @todo make passes accept CircuitState so annotations can pass data between them
  */
trait PassBased extends SimpleRun {
  def passSeq: Seq[Pass]
  def runPasses(circuit: Circuit): Circuit = runPasses(circuit, passSeq)
}

/** For transformations that are simply a sequence of passes */
abstract class PassBasedTransform extends Transform with PassBased {
  def execute(state: CircuitState): CircuitState = {
    require(state.form <= inputForm,
      s"[$name]: Input form must be lower or equal to $inputForm. Got ${state.form}")
    CircuitState(runPasses(state.circuit), outputForm)
  }
}

/** Similar to a Transform except that it writes to a Writer instead of returning a
  * CircuitState
  */
abstract class Emitter {
  def emit(state: CircuitState, writer: Writer): Unit
}

object TransformUtils {
  def getLoweringTransforms(inputForm: CircuitForm, outputForm: CircuitForm): Seq[Transform] = {
    // If outputForm is equal-to or higher than inputForm, nothing to lower
    if (outputForm >= inputForm) {
      Seq.empty
    } else {
      inputForm match {
        case ChirrtlForm => Seq(new ChirrtlToHighFirrtl) ++ getLoweringTransforms(HighForm, outputForm)
        case HighForm => Seq(new IRToWorkingIR, new ResolveAndCheck, new HighFirrtlToMiddleFirrtl) ++
                         getLoweringTransforms(MidForm, outputForm)
        case MidForm => Seq(new MiddleFirrtlToLowFirrtl) ++ getLoweringTransforms(LowForm, outputForm)
        case LowForm => error("Internal Error! This shouldn't be possible") // should be caught by if above
      }
    }
  }

  def mergeTransforms(lowering: Seq[Transform], custom: Seq[Transform]): Seq[Transform] = {
    custom.foldLeft(lowering) { case (transforms, xform) =>
      val index = transforms lastIndexWhere (_.outputForm == xform.inputForm)
      assert(index >= 0,
        s"No transform in $lowering has outputForm ${xform.inputForm} as required by $xform")
      val (front, back) = transforms.splitAt(index + 1) // +1 because we want to be AFTER index
      front ++ List(xform) ++ getLoweringTransforms(xform.outputForm, xform.inputForm) ++ back
    }
  }

}

trait Compiler {
  def emitter: Emitter
  /** The sequence of transforms this compiler will execute
    * @note The inputForm of a given transform must be higher than or equal to the ouputForm of the
    *       preceding transform
    */
  def transforms: Seq[Transform]

  // Similar to (input|output)Form on [[Transform]] but derived from this Compiler's transforms
  def inputForm = transforms.head.inputForm
  def outputForm = transforms.last.outputForm

  private def transformsLegal(xforms: Seq[Transform]): Boolean =
    if (xforms.size < 2) {
      true
    } else {
      xforms.sliding(2, 1)
            .map { case Seq(p, n) => n.inputForm >= p.outputForm }
            .reduce(_ && _)
    }

  assert(transformsLegal(transforms),
    "Illegal Compiler, each transform must be able to accept the output of the previous transform!")

  def compile(state: CircuitState,
              writer: Writer,
              customTransforms: Seq[Transform] = Seq.empty): CircuitState = {
    val allTransforms = TransformUtils.mergeTransforms(transforms, customTransforms)

    val finalState = allTransforms.foldLeft(state) { (in, xform) =>
      val result = xform.execute(in)

      // Annotation propagation
      // TODO: This should be redone
      val inAnnotationMap = in.annotations getOrElse AnnotationMap(Seq.empty)
      val remappedAnnotations: Seq[Annotation] = result.renames match {
        case Some(RenameMap(rmap)) =>
          // For each key in the rename map (rmap), obtain the
          // corresponding annotations (in.annotationMap.get(from)). If any
          // annotations exist, for each annotation, create a sequence of
          // annotations with the names in rmap's value.
          for {
            (oldName, newNames) <- rmap.toSeq
            transform2OldAnnos <- inAnnotationMap.get(oldName).toSeq
            oldAnno <- transform2OldAnnos.values
            newAnno <- oldAnno.update(newNames)
          } yield newAnno
        case _ => inAnnotationMap.annotations
      }
      val resultAnnotations: Seq[Annotation] = result.annotations match {
        case None => Nil
        case Some(p) => p.annotations
      }
      val newAnnotations = AnnotationMap(remappedAnnotations ++ resultAnnotations)
      CircuitState(result.circuit, result.form, Some(newAnnotations))
    }

    emitter.emit(finalState, writer)
    finalState
  }
}

