package com.todesking.fast_function_composer.opt

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{classTag, ClassTag}

object Opt {
  import Data._

  def optimize[A: ClassTag](orig: X forSome { type X <: A }, el: EventListener, rewriteRule: RewriteRule): A = {
    val sc = Instance.Native(orig)

    val optimized = rewriteRule.apply(sc)

    optimized.instance()
  }

  lazy val defaultRewriteRule: RewriteRule = RewriteRule.sequence("default")(
    inlining,
    eliminateBoxing
  )

  lazy val inlining: RewriteRule = RewriteRule.eachPublicMethod("inlining") { method =>
    method.rewriteBody {
      case out @ InvokeVirtual(cName, mName, self @ ThisRef(_), args @ _*) =>
        val method = self.constRef.method(mName).get
        method.body.bind((self +: args): _*)
      case out @ InvokeVirtual(cName, mName, self, args @ _*) if self.isConstRef =>
        val instance = self.constRef
        val method = instance.method(mName).get
        if(!method.hasFieldReference && !method.hasThisReference) {
          method.body.bind((self +: args): _*)
        } else if(method.hasFinalFieldReference) {
          // move instance's field to this
          // inlining with field substitution
          out
        } else {
          out
        }
      case out @ InvokeSpecial(cName, mName, self @ ThisRef(_), args @ _*) =>
        self.constRef.method(mName).get.body.bind((self +: args): _*)
    }
  }

  lazy val eliminateException = RewriteRule.eachPublicMethod("eliminateThrowCatch") { method =>
    method.rewriteBody {
      case a => a
        // throw e: E
        // catch e: E1 >: E in same method
        // if(!e isInstanceOf E) throw e
        // => rewrite throw to goto
    }
  }

  lazy val eliminateNew = new RewriteRule("eliminateNew") {
    override def apply[A](instance: Instance[A]) = {
      instance.publicMethods.foldLeft(instance) { (instance, method) =>
        instance.rewriteMethod(method) {
          _.rewriteBody {
            case out @ NewInstance(cName, mSig, args) ~ dependers =>
              if(
                // all dependers are invoke out's method
                // all referenced methods should:
                //    - not reference this
                //    - referenced this-methods are recursively inlineable
                //    - inlining result not contain this method reference
                false
              ) {
                // create new data correspond to out's fields
                // expand out.<init>
                // instance-wide inlining and substitute fileld access method call of out
                out
              } else {
                out
              }
          }
        }
      }
    }
  }

  lazy val eliminateBoxing: RewriteRule = RewriteRule.sequence("eliminateBoxing")(
    RewriteRule.eachPublicMethod("valueOfToNew") { method =>
      method.rewriteBody {
        case out @ InvokeStatic("java.lang.Integer", "valueOf(I)Ljava.lang.Integer;", in) =>
          NewInstance("java.lang.Integer", "I", in)
      }
    },
    RewriteRule.eachPublicMethod("eliminateBoxingBody") { method =>
      method.rewriteBody {
        case InvokeVirtual("java.lang.Integer", "intValue()I", NewInstance("java.lang.Integer", "I", _, int)) =>
          int
      }
    },
    RewriteRule.eachPublicMethod("newToValueOf") { method =>
      method.rewriteBody {
        case out @ NewInstance("java.lang.Integer", "I", in) =>
          InvokeStatic("java.lang.Integer", "valueOf(I)Ljava.lang.Integer;", out, in)
      }
    }
  )
}

case class ClassDescriptor()
case class MethodDescriptor()
case class FieldDescriptor()

class DataType
class DataValue {
  def toInstance: Instance[_] = ???
}
sealed abstract class Data {
  def tpe: DataType = ???
  def isConstRef: Boolean = ???
  def constRef: Instance[_] = ???
}

class DataLabel
object DataLabel {
  case class Local(n: Int) extends DataLabel
  case class Stack(n: Int) extends DataLabel
}

class ProcLabel

abstract class Frame {
  def locals: Seq[Data]
  def stack: Seq[Data]
  def apply(l: DataLabel): Data
}

sealed abstract class Proc {
  def firstFrame: Frame
  def lastFrame: Frame
}

abstract class MethodBody(args: Seq[Data]) {
  def start: Proc
  def exits: Seq[Proc]
  def bind(newArgs: Data*): Proc
}

object ProcMatch {
  object ExactRef(classDescriptor: String)
  object ThisRef(value: Data) 
  object InvokeStatic(classDescriptor: String, methodDescriptor: String, args: Data*) 
  object InvokeSpecial(classDescriptor: String, methodDescriptor: String, args: Data*) 
  object NewInstance(classDescriptor: String, constructorSignature: String, args: Data*) 
  object InvokeVirtual(classDescriptor: String, methodDescriptor: String, thisValue: Data, args: Data*) 

  case class ~(src: Data, dependers: Seq[Data]) 
}

sealed abstract class Instance[A] {
  def instance(): A = ???
  def publicMethods: Seq[Method] = ???
  def rewriteMethod(m1: Method, m2: Method): Instance[A] = ???
  def rewriteMethod(orig: Method)(f: Method => Method): Instance[A] = ???
  def method(descriptor: String): Option[Method] = ???
}

object Instance {
  case class Native[A](value: A) extends Instance[A]
  case class Rewritten[A]() extends Instance[A]
  case class Upcast[A](instance: Instance[_ <: A])
}

class Method {
  def rewriteBody(f: PartialFunction[Proc, Proc]): Method = ???
  def hasFieldReference: Boolean = ???
  def hasThisReference: Boolean = ???
  def hasFinalFieldReference: Boolean = ???
  def body: MethodBody = ???
}

trait EventListener

abstract class RewriteRule(val name: String) {
  def apply[A](sc: Instance[A]): Instance[A]
}

object RewriteRule {
  val identity: RewriteRule = new RewriteRule("identity") {
    override def apply[A](i: Instance[A]): Instance[A] = i
  }

  def sequence(name: String)(rules: RewriteRule*): RewriteRule =
    Sequence(name, rules)

  def eachPublicMethod(name: String)( f: Method => Method): RewriteRule =
    EachPublicMethod(name, f)

  case class Sequence(override val name: String, rules: Seq[RewriteRule]) extends RewriteRule(name) {
    override def apply[A](i: Instance[A]): Instance[A] = rules.foldLeft(i) { (i, r) => r(i) }
  }

  case class EachPublicMethod(override val name: String, f: Method => Method) extends RewriteRule(name) {
    override def apply[A](i: Instance[A]): Instance[A] =
      i.publicMethods.foldLeft(i) { (instance, method) => instance.rewriteMethod(method, f(method)) }
  }

}
