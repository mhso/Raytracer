namespace Tests

(*
  Some of these tests are from the ones given in the Functional Programming course (test01 - test08)
*)

module ExprToPolyTests =
  open Tracer.ImplicitSurfaces.ExprParse
  open Tracer.ImplicitSurfaces.ExprToPoly
  open Assert
  let allTests =
    printfn "Doing ExprToPolyTests..."
    let test04 =
      let es = List.map (fun n -> FExponent (FAdd (FVar "a", FVar "b"), n)) [1 .. 5]
      let esStr = List.map (fun n -> parseStr ("(a+b)^"+((string)n))) [1 .. 5]
      let es2 = List.map (fun n -> FExponent (FAdd (FVar "a", FAdd(FVar "b", FVar "c")),n)) [1 .. 5]
      let es2Str = List.map (fun n -> parseStr ("(a+b+c)^"+((string)n))) [1 .. 5]
      let rs s = [("TestSimplify01"+s,"a+b");
                  ("TestSimplify02"+s,"2*a*b+a^2+b^2");
                  ("TestSimplify03"+s,"3*a*b^2+3*a^2*b+a^3+b^3");
                  ("TestSimplify04"+s,"4*a*b^3+6*a^2*b^2+4*a^3*b+a^4+b^4");
                  ("TestSimplify05"+s,"5*a*b^4+10*a^2*b^3+10*a^3*b^2+5*a^4*b+a^5+b^5")]
      let rs2 s = [("TestSimplify06"+s,"a+b+c");
                   ("TestSimplify07"+s,"2*a*b+2*a*c+a^2+2*b*c+b^2+c^2");
                   ("TestSimplify08"+s,"6*a*b*c+3*a*b^2+3*a*c^2+3*a^2*b+3*a^2*c+a^3+3*b*c^2+3*b^2*c+b^3+c^3");
                   ("TestSimplify09"+s,"12*a*b*c^2+12*a*b^2*c+4*a*b^3+4*a*c^3+12*a^2*b*c+6*a^2*b^2+6*a^2*c^2+4*a^3*b+4*a^3*c+a^4+4*b*c^3+6*b^2*c^2+4*b^3*c+b^4+c^4");
                   ("TestSimplify10"+s,"20*a*b*c^3+30*a*b^2*c^2+20*a*b^3*c+5*a*b^4+5*a*c^4+30*a^2*b*c^2+30*a^2*b^2*c+10*a^2*b^3+10*a^2*c^3+20*a^3*b*c+10*a^3*b^2+10*a^3*c^2+5*a^4*b+5*a^4*c+a^5+5*b*c^4+10*b^2*c^3+10*b^3*c^2+5*b^4*c+b^5+c^5")]
      let allEs = List.zip (rs "") es
      let allEsStr = List.zip (rs "Parse") esStr
      let allEs2 = List.zip (rs2 "") es2
      let allEs2Str = List.zip (rs2 "Parse") es2Str
      let genChk ((n,r),e) = (n,(exprToSimpleExpr >> ppSimpleExpr) e,r)
      let testlist = List.map genChk (allEs @ allEsStr @ allEs2 @ allEs2Str)
      Assert.EqualMany testlist (parseStr >> exprToSimpleExpr >> ppSimpleExpr)
     
    test04