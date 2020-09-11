object Interpreter {
  sealed abstract class Token
  case class TOpen() extends Token
  case class TClose() extends Token
  case class TSymbol(string: String) extends Token
  case class TInt(value: Int) extends Token

  def isSymbolCharacter(c: Char): Boolean = {
    c.isLetterOrDigit || List('+', '-', '*', '/', '!', '=', '<', '>', '_', '%').contains(c)
  }

  def tokenize(string: String): List[Token] = tokenize(string.toCharArray.toList)

  def tokenize(characters: List[Char]): List[Token] = {
    characters match {
      case List() => List()
      case '(' :: cs => TOpen() +: tokenize(cs)
      case ')' :: cs => TClose() +: tokenize(cs)
      case c :: cs =>
        c match {
          case digit if digit.isDigit =>
            val (remainingDigits, rest) = cs.span(i => i.isDigit)
            TInt(value = (digit +: remainingDigits).mkString.toInt) +: tokenize(rest)
          case symbolCharacter if isSymbolCharacter(symbolCharacter) =>
            val (remainingSymbolCharacters, rest) = cs.span(i => isSymbolCharacter(i))
            TSymbol(string = (symbolCharacter +: remainingSymbolCharacters).mkString) +: tokenize(rest)
          case whitespace if whitespace.isWhitespace =>
            tokenize(cs)
        }

    }
  }

  sealed abstract class Nest
  case class NList(list: List[Nest]) extends Nest
  case class NSymbol(string: String) extends Nest
  case class NInt(value: Int) extends Nest

  def nestOne(tokens: List[Token], ns: List[Nest]): (Nest, List[Token]) = {
    tokens match {
      case List() =>
        (NList(ns), List())
      case t :: ts =>
        t match {
          case TOpen() =>
            val (n, tsp) = nestOne(ts, List())
            nestOne(tsp, ns :+ n)
          case TSymbol(s) =>
            nestOne(ts, ns :+ NSymbol(s))
          case TInt(i) =>
            nestOne(ts, ns :+ NInt(i))
          case TClose() =>
            (NList(ns), ts)
        }
    }
  }

  def nest(tokens: List[Token]): Nest = {
    val (result, remainder) = nestOne(tokens, List())
    if (remainder.nonEmpty) {
      throw new RuntimeException(s"Non empty remainder when nesting: ${remainder}")
    }
    result match {
      case NList(ns) => NList(NSymbol("seq") +: ns)
      case e => throw new RuntimeException(s"Invalid nest result: ${e}")
    }
  }

  sealed abstract class Expression
  case class ESet(identifier: String, value: Expression) extends Expression
  case class EGet(identifier: String) extends Expression
  case class EInt(value: Int) extends Expression
  case class EBinOp(operator: Op, lhs: Expression, rhs: Expression) extends Expression
  case class ENot(e: Expression) extends Expression
  case class EIf(condition: Expression, consequent: Expression, alternative: Expression) extends Expression
  case class EWhile(condition: Expression, body: Expression) extends Expression
  case class ESeq(e1: Expression, e2: Expression) extends Expression
  case class EWriteInt(e: Expression) extends Expression

  sealed abstract class Op
  case class Add() extends Op
  case class Sub() extends Op
  case class Mul() extends Op
  case class Div() extends Op
  case class Mod() extends Op

  case class Eq() extends Op
  case class Neq() extends Op

  case class Lt() extends Op
  case class Lte() extends Op
  case class Gt() extends Op
  case class Gte() extends Op

  case class And() extends Op
  case class Or() extends Op

  def parse(nest: Nest): Expression = {
    nest match {
      case NInt(i) => EInt(i)

      case NList(List(NSymbol("+"), a, b)) => EBinOp(Add(), parse(a), parse(b))
      case NList(List(NSymbol("-"), a, b)) => EBinOp(Sub(), parse(a), parse(b))
      case NList(List(NSymbol("*"), a, b)) => EBinOp(Mul(), parse(a), parse(b))
      case NList(List(NSymbol("/"), a, b)) => EBinOp(Div(), parse(a), parse(b))
      case NList(List(NSymbol("%"), a, b)) => EBinOp(Mod(), parse(a), parse(b))

      case NList(List(NSymbol("="), a, b)) => EBinOp(Eq(), parse(a), parse(b))
      case NList(List(NSymbol("!="), a, b)) => EBinOp(Neq(), parse(a), parse(b))
      case NList(List(NSymbol("<"), a, b)) => EBinOp(Lt(), parse(a), parse(b))
      case NList(List(NSymbol("<="), a, b)) => EBinOp(Lte(), parse(a), parse(b))
      case NList(List(NSymbol(">"), a, b)) => EBinOp(Gt(), parse(a), parse(b))
      case NList(List(NSymbol(">="), a, b)) => EBinOp(Gte(), parse(a), parse(b))

      case NList(List(NSymbol("and"), a, b)) => EBinOp(And(), parse(a), parse(b))
      case NList(List(NSymbol("or"), a, b)) => EBinOp(Or(), parse(a), parse(b))
      case NList(List(NSymbol("not"), a)) => ENot(parse(a))

      case NList(List(NSymbol("get"), NSymbol(a))) => EGet(a)
      case NList(List(NSymbol("set"), NSymbol(a), b)) => ESet(a, parse(b))

      case NList(List(NSymbol("if"), a, b, c)) => EIf(parse(a), parse(b), parse(c))
      case NList(NSymbol("seq") :: xs) => xs.map(parse).reduceRight(ESeq)
      case NList(NSymbol("while") :: a :: bs) => EWhile(parse(a), bs.map(parse).reduceRight(ESeq))
      case NList(List(NSymbol("write"), a)) => EWriteInt(parse(a))

      case NSymbol(a) => EGet(a)
    }
  }

  def evalOp(op: Op, a: Int, b: Int): Int = {
    op match {
      case Add() => a + b
      case Sub() => a - b
      case Mul() => a * b
      case Div() => a / b
      case Mod() => a % b
      case Eq() => if (a == b) 1 else 0
      case Neq() => if (a != b) 1 else 0
      case Lt() => if (a < b) 1 else 0
      case Lte() => if (a <= b) 1 else 0
      case Gt() => if (a > b) 1 else 0
      case Gte() => if (a >= b) 1 else 0
      case And() => if ((a != 0) && (b != 0)) 1 else 0
      case Or() => if ((a != 0) || (b != 0)) 1 else 0
    }
  }

  def eval(vars: Map[String, Int], expression: Expression): (Int, Map[String, Int]) = {
    expression match {
      case EInt(i) => (i, vars)
      case EBinOp(op, e1, e2) =>
        val (val1, varsp) = eval(vars, e1)
        val (val2, varspp) = eval(varsp, e2)
        (evalOp(op, val1, val2), varspp)
      case ENot(e) =>
        val (v, varsp) = eval(vars, e)
        if (v == 0) {
          (1, varsp)
        } else {
          (0, varsp)
        }
      case EGet(variable) =>
        vars.get(variable) match {
          case Some(x) => (x, vars)
          case None => throw new RuntimeException(s"Variable does not exist: ${variable}")
        }
      case ESet(variable, e) =>
        val (value, varsp) = eval(vars, e)
        (value, varsp.updated(variable, value))
      case EIf(c, t, e) =>
        val (cond, varsp) = eval(vars, c)
        if (cond == 0) {
          eval(varsp, e)
        } else {
          eval(varsp, t)
        }
      case EWhile(c, e) =>
        val (cond, varsp) = eval(vars, c)
        if (cond == 0) {
          (0, varsp)
        } else {
          val (_, varspp) = eval(varsp, e)
          eval(varspp, EWhile(c, e))
        }
      case ESeq(e1, e2) =>
        val (_, varsp) = eval(vars, e1)
        eval(varsp, e2)
      case EWriteInt(intE) =>
        val (i, varsp) = eval(vars, intE)
        println(i)
        (i, varsp)
    }
  }

  def eval(string: String): (Int, Map[String, Int]) = {
    val tokenized = tokenize(string)

    val nested = nest(tokenized)

    val parsed = parse(nested)

    eval(Map(), parsed)
  }

  def evalToGlobals(string: String): Map[String, Int] = eval(string)._2

  def evalToResult(string: String): Int = eval(string)._1

}
