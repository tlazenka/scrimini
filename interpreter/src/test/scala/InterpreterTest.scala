import Interpreter._
import org.scalatest.funsuite.AnyFunSuite

class InterpreterTest extends AnyFunSuite {
  test("Interpreter.eval") {
    val result0 = evalToGlobals("(set i 0) (set i (+ i 2))")
    assert(result0.size == 1)
    assert(result0("i") == 2)

    val result1 = evalToGlobals("(set i 0) (if 0 (set i (+ i 2)) (set i (+ i 3)))")
    assert(result1.size == 1)
    assert(result1("i") == 3)

    val result2 = evalToGlobals("(set i 0) (if 1 (set i (+ i 2)) (set i (+ i 3)))")
    assert(result2.size == 1)
    assert(result2("i") == 2)

    val result3 = evalToGlobals("(set i 0) (while (< i 3) (set i (+ i 1)))")
    assert(result3.size == 1)
    assert(result3("i") == 3)

  }

  test("Interpreter.parse") {
    val example: String = """(set i 0)
                            |(while (<= i 3)
                            |  (write i)
                            |  (set j 500)
                            |  (while (<= j 503)
                            |    (write j)
                            |    (set j (+ j 1)))
                            |  (set i (+ i 1)))""".stripMargin

    val tokenized = tokenize(example)

    // format: off
    val expectedTokens = List(TOpen(), TSymbol("set"), TSymbol("i"), TInt(0), TClose(),
      TOpen(), TSymbol("while"), TOpen(), TSymbol("<="), TSymbol("i"), TInt(3), TClose(),
        TOpen(), TSymbol("write"), TSymbol("i"), TClose(),
        TOpen(), TSymbol("set"), TSymbol("j"), TInt(500), TClose(),
        TOpen(), TSymbol("while"),
          TOpen(), TSymbol("<="), TSymbol("j"), TInt(503), TClose(),
          TOpen(), TSymbol("write"), TSymbol("j"), TClose(),
          TOpen(), TSymbol("set"), TSymbol("j"), TOpen(), TSymbol("+"), TSymbol("j"), TInt(1), TClose(), TClose(), TClose(),
        TOpen(), TSymbol("set"), TSymbol("i"), TOpen(), TSymbol("+"), TSymbol("i"), TInt(1), TClose(), TClose(), TClose())
    // format: on
    assert(tokenized == expectedTokens)

    val nested = nest(tokenized)

    // format: off
    val expectedNest = NList(List(NSymbol("seq"),
      NList(List(NSymbol("set"), NSymbol("i"), NInt(0))),
      NList(List(NSymbol("while"),
        NList(List(NSymbol("<="), NSymbol("i"), NInt(3))),
        NList(List(NSymbol("write"), NSymbol("i"))),
        NList(List(NSymbol("set"), NSymbol("j"), NInt(500))),
        NList(List(NSymbol("while"),
          NList(List(NSymbol("<="), NSymbol("j"), NInt(503))),
          NList(List(NSymbol("write"), NSymbol("j"))),
          NList(List(NSymbol("set"), NSymbol("j"), NList(List(NSymbol("+"), NSymbol("j"), NInt(1))))))),
        NList(List(NSymbol("set"), NSymbol("i"), NList(List(NSymbol("+"), NSymbol("i"), NInt(1)))))))))
    // format: on

    assert(nested == expectedNest)

    // format: off
    val expectedParse = ESeq(ESet("i", EInt(0)),
      EWhile(EBinOp(Lte(), EGet("i"), EInt(3)),
        ESeq(EWriteInt(EGet("i")), ESeq(ESet("j", EInt(500)),
          ESeq(EWhile(EBinOp(Lte(), EGet("j"), EInt(503)),
            ESeq(EWriteInt(EGet("j")),
              ESet("j", EBinOp(Add(), EGet("j"), EInt(1))))), ESet("i", EBinOp(Add(), EGet("i"), EInt(1))))))))
    // format: on

    val parsed = parse(nested)

    assert(expectedParse == parsed)

    val result0 = evalToGlobals(example)
    assert(result0.size == 2)
    assert(result0("i") == 4)
    assert(result0("j") == 504)
  }

  test("Interpreter.op") {
    assert(evalToResult("(>= 1 0)") == 1)
    assert(evalToResult("(> 1 0") == 1)
    assert(evalToResult("(<= 1 0)") == 0)
    assert(evalToResult("(< 1 0)") == 0)
    assert(evalToResult("(= 1 0)") == 0)
    assert(evalToResult("(!= 1 0)") == 1)
    assert(evalToResult("(= 1 1)") == 1)
    assert(evalToResult("(!= 1 1)") == 0)

    assert(evalToResult("(+ 6 2)") == 8)
    assert(evalToResult("(- 6 2)") == 4)
    assert(evalToResult("(* 6 2)") == 12)
    assert(evalToResult("(/ 6 2)") == 3)

    assert(evalToResult("(% 20 3)") == 2)

    assert(evalToResult("(and 20 3)") == 1)
    assert(evalToResult("(and 20 0)") == 0)
    assert(evalToResult("(and 0 3)") == 0)
    assert(evalToResult("(and 0 0)") == 0)
    assert(evalToResult("(or 0 0)") == 0)
    assert(evalToResult("(or 20 0)") == 1)
    assert(evalToResult("(or 0 3)") == 1)
    assert(evalToResult("(or 20 3)") == 1)

    assert(evalToResult("(not 20)") == 0)
    assert(evalToResult("(not 0)") == 1)

  }
}
