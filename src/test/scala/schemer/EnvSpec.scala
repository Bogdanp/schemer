package schemer

import org.specs2.mutable._

class EnvSpec extends Specification {
  "The Env" should {
    "fail to lookup when empty" in {
      Env().lookup(SymbolExpression("foo")) must beNone
    }

    "allow symbols to be set" in {
      val sym = SymbolExpression("foo")
      val num = NumberExpression(42)

      Env().set(sym, num).lookup(sym) must beSome(num)
    }

    "lookup symbols in parent" in {
      val sym = SymbolExpression("foo")
      val num = NumberExpression(42)
      val parent = Env().set(sym, num)

      Env(parent).lookup(sym) must beSome(num)
    }
  }
}
