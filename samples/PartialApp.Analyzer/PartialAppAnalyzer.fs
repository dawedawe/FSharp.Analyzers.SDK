module PartialAppAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open Microsoft.FSharp.Core

module AST =

    let collectExprFromMatchClauses (clauses: SynMatchClause list) =
        clauses
        |> List.map (fun (SynMatchClause(whenExpr = whenExpr; resultExpr = resultExpr)) ->
            [ if Option.isSome whenExpr then
                  whenExpr.Value
              else
                  ()
                  resultExpr ])
        |> List.concat

    let rec visitApp (expr: SynExpr) =
        match expr with
        | SynExpr.App(funcExpr = SynExpr.Paren(expr = expr); argExpr = argExpr) ->
            [ visitApp expr; visitExpr argExpr ] |> List.concat
        | SynExpr.App(funcExpr = SynExpr.Ident i) -> [ (i.idText, i.idRange, 1) ]
        | SynExpr.App(funcExpr = SynExpr.LongIdent(longDotId = longDotId); argExpr = argExpr) ->
            let i = longDotId.IdentsWithTrivia |> Seq.last

            match i with
            | SynIdent.SynIdent(ident = ident) ->
                [ [ (ident.idText, longDotId.Range, 1) ]; visitApp argExpr ] |> List.concat
        | SynExpr.App(funcExpr = SynExpr.App _ as funcExpr; argExpr = argExpr) ->
            let apps = visitApp funcExpr

            [ apps |> List.map (fun (i, r, c) -> (i, r, c + 1)); visitApp argExpr ]
            |> List.concat
        | SynExpr.App(funcExpr = SynExpr.TypeApp(expr = SynExpr.Ident i)) -> [ (i.idText, i.idRange, 1) ]
        | SynExpr.App(funcExpr = SynExpr.TypeApp(expr = SynExpr.LongIdent(longDotId = longDotId))) ->
            let i = longDotId.IdentsWithTrivia |> Seq.last

            match i with
            | SynIdent.SynIdent(ident = ident) -> [ (ident.idText, longDotId.Range, 1) ]
        | SynExpr.IfThenElse(ifExpr = ifExpr; thenExpr = thenExpr; elseExpr = elseExpr) ->
            [ visitApp ifExpr
              visitApp thenExpr
              Option.map visitApp elseExpr |> Option.defaultValue [] ]
            |> List.concat
        | SynExpr.Match(expr = expr; clauses = clauses) ->
            let matchExprs = collectExprFromMatchClauses clauses

            [ visitApp expr; (List.map visitApp matchExprs) |> List.concat ] |> List.concat
        | SynExpr.MatchLambda(matchClauses = matchClauses) ->
            let matchExprs = collectExprFromMatchClauses matchClauses
            List.map visitApp matchExprs |> List.concat
        | SynExpr.App(funcExpr = SynExpr.DotGet _) -> []
        | SynExpr.App(funcExpr = SynExpr.DotIndexedGet _) -> []
        | SynExpr.App(funcExpr = SynExpr.TypeApp(expr = SynExpr.DotIndexedGet _)) -> []
        | SynExpr.App(funcExpr = SynExpr.TypeApp(expr = SynExpr.DotGet _)) -> []
        | SynExpr.Const _
        | SynExpr.LongIdent _ -> []

        | _ ->
            printfn $"visitApp: not supported yet {expr}"
            []

    and visitMatchClause (SynMatchClause(resultExpr = resultExpr; whenExpr = whenExpr)) =
        [ visitExpr resultExpr
          Option.map visitExpr whenExpr |> Option.defaultValue [] ]
        |> List.concat

    and visitRecordField (SynExprRecordField(expr = expr)) =
        Option.map visitExpr expr |> Option.defaultValue []

    and visitAndBang (SynExprAndBang(body = body)) = visitExpr body

    and visitInterpolatedStringParts (part: SynInterpolatedStringPart) =
        match part with
        | SynInterpolatedStringPart.FillExpr(fillExpr = fillExpr) -> visitExpr fillExpr
        | _ -> []

    and visitExpr (expr: SynExpr) =
        match expr with
        | SynExpr.App _ as e -> visitApp e
        | SynExpr.Lambda(body = body) -> visitExpr body
        | SynExpr.LetOrUse(bindings = bindings; body = body) ->
            [ List.map visitBinding bindings |> List.concat; visitExpr body ] |> List.concat
        | SynExpr.Do(expr = expr) -> visitExpr expr
        | SynExpr.Sequential(expr1 = expr1; expr2 = expr2) -> [ visitExpr expr1; visitExpr expr2 ] |> List.concat
        | SynExpr.Typed(expr = expr) -> visitExpr expr
        | SynExpr.Match(expr = expr; clauses = clauses) ->
            [ visitExpr expr; clauses |> List.map visitMatchClause |> List.concat ]
            |> List.concat
        | SynExpr.Assert(expr = expr) -> visitExpr expr
        | SynExpr.Downcast(expr = expr) -> visitExpr expr
        | SynExpr.Dynamic(funcExpr = funcExpr; argExpr = argExpr) ->
            [ visitExpr funcExpr; visitExpr argExpr ] |> List.concat
        | SynExpr.Fixed(expr = expr) -> visitExpr expr
        | SynExpr.For(identBody = identBody; toBody = synExpr; doBody = doBody) ->
            [ visitExpr identBody; visitExpr synExpr; visitExpr doBody ] |> List.concat
        | SynExpr.Lazy(expr = expr) -> visitExpr expr
        | SynExpr.New(expr = expr) -> visitExpr expr
        | SynExpr.Paren(expr = expr) -> visitExpr expr
        | SynExpr.Quote(operator = operator; quotedExpr = quotedExpr) ->
            [ visitExpr operator; visitExpr quotedExpr ] |> List.concat
        | SynExpr.Record(baseInfo = baseInfo; copyInfo = copyInfo; recordFields = recordFields) ->
            [ match baseInfo with
              | Some(_, expr, _, _, _) -> visitExpr expr
              | None -> []
              match copyInfo with
              | Some(expr, _) -> visitExpr expr
              | None -> []
              recordFields |> List.map visitRecordField |> List.concat ]
            |> List.concat
        | SynExpr.Set(targetExpr = targetExpr; rhsExpr = rhsExpr) ->
            [ visitExpr targetExpr; visitExpr rhsExpr ] |> List.concat
        | SynExpr.Tuple(exprs = exprs) -> List.map visitExpr exprs |> List.concat
        | SynExpr.Upcast(expr = expr) -> visitExpr expr
        | SynExpr.While(whileExpr = whileExpr; doExpr = doExpr) ->
            [ visitExpr whileExpr; visitExpr doExpr ] |> List.concat
        | SynExpr.AddressOf(expr = expr) -> visitExpr expr
        | SynExpr.AnonRecd(copyInfo = copyInfo; recordFields = recordFields) ->
            [ match copyInfo with
              | Some(expr, _) -> visitExpr expr
              | None -> []
              recordFields |> List.map (fun (_, _, expr) -> visitExpr expr) |> List.concat ]
            |> List.concat
        | SynExpr.ComputationExpr(expr = expr) -> visitExpr expr
        | SynExpr.DebugPoint(innerExpr = innerExpr) -> visitExpr innerExpr
        | SynExpr.DoBang(expr = expr) -> visitExpr expr
        | SynExpr.DotGet(expr = expr) -> visitExpr expr
        | SynExpr.DotSet(targetExpr = targetExpr; rhsExpr = rhsExpr) ->
            [ visitExpr targetExpr; visitExpr rhsExpr ] |> List.concat
        | SynExpr.ForEach(enumExpr = enumExpr; bodyExpr = bodyExpr) ->
            [ visitExpr enumExpr; visitExpr bodyExpr ] |> List.concat
        | SynExpr.ArrayOrList(exprs = exprs) -> List.map visitExpr exprs |> List.concat
        | SynExpr.ObjExpr(argOptions = argOptions; bindings = bindings; members = members; extraImpls = extraImpls) ->
            [ match argOptions with
              | Some(expr, _) -> visitExpr expr
              | None -> []
              List.map visitBinding bindings |> List.concat
              List.map visitMemberDefn members |> List.concat
              List.map visitInterfaceImpl extraImpls |> List.concat ]
            |> List.concat
        | SynExpr.ArrayOrListComputed(expr = expr) -> visitExpr expr
        | SynExpr.IndexRange(expr1 = expr1; expr2 = expr2) ->
            [ Option.map visitExpr expr1 |> Option.defaultValue []
              Option.map visitExpr expr2 |> Option.defaultValue [] ]
            |> List.concat
        | SynExpr.IndexFromEnd(expr = expr) -> visitExpr expr
        | SynExpr.MatchLambda(matchClauses = matchClauses) -> List.map visitMatchClause matchClauses |> List.concat
        | SynExpr.TypeApp(expr = expr) -> visitExpr expr
        | SynExpr.TryWith(tryExpr = tryExpr; withCases = withCases) ->
            [ visitExpr tryExpr; List.map visitMatchClause withCases |> List.concat ]
            |> List.concat
        | SynExpr.TryFinally(tryExpr = tryExpr; finallyExpr = finallyExpr) ->
            [ visitExpr tryExpr; visitExpr finallyExpr ] |> List.concat
        | SynExpr.IfThenElse(ifExpr = ifExpr; thenExpr = thenExpr; elseExpr = elseExpr) ->
            [ visitExpr ifExpr
              visitExpr thenExpr
              Option.map visitExpr elseExpr |> Option.defaultValue [] ]
            |> List.concat
        | SynExpr.LongIdentSet(expr = expr) -> visitExpr expr
        | SynExpr.DotIndexedGet(objectExpr = objectExpr; indexArgs = indexArgs) ->
            [ visitExpr objectExpr; visitExpr indexArgs ] |> List.concat
        | SynExpr.DotIndexedSet(objectExpr = objectExpr; indexArgs = indexArgs; valueExpr = valueExpr) ->
            [ visitExpr objectExpr; visitExpr indexArgs; visitExpr valueExpr ]
            |> List.concat
        | SynExpr.NamedIndexedPropertySet(expr1 = expr1; expr2 = expr2) ->
            [ visitExpr expr1; visitExpr expr2 ] |> List.concat
        | SynExpr.DotNamedIndexedPropertySet(targetExpr = targetExpr; argExpr = argExpr; rhsExpr = rhsExpr) ->
            [ visitExpr targetExpr; visitExpr argExpr; visitExpr rhsExpr ] |> List.concat
        | SynExpr.TypeTest(expr = expr) -> visitExpr expr
        | SynExpr.InferredUpcast(expr = expr) -> visitExpr expr
        | SynExpr.InferredDowncast(expr = expr) -> visitExpr expr
        | SynExpr.TraitCall(argExpr = argExpr) -> visitExpr argExpr
        | SynExpr.JoinIn(lhsExpr = lhsExpr; rhsExpr = rhsExpr) ->
            [ visitExpr lhsExpr; visitExpr rhsExpr ] |> List.concat
        | SynExpr.SequentialOrImplicitYield(expr1 = expr1; expr2 = expr2; ifNotStmt = ifNotStmt) ->
            [ visitExpr expr1; visitExpr expr2; visitExpr ifNotStmt ] |> List.concat
        | SynExpr.YieldOrReturn(expr = expr) -> visitExpr expr
        | SynExpr.YieldOrReturnFrom(expr = expr) -> visitExpr expr
        | SynExpr.LetOrUseBang(rhs = rhs; andBangs = andBangs; body = body) ->
            [ visitExpr rhs; List.map visitAndBang andBangs |> List.concat; visitExpr body ]
            |> List.concat
        | SynExpr.MatchBang(expr = expr; clauses = clauses) ->
            [ visitExpr expr; List.map visitMatchClause clauses |> List.concat ]
            |> List.concat
        | SynExpr.LibraryOnlyILAssembly(args = args) -> List.map visitExpr args |> List.concat
        | SynExpr.LibraryOnlyStaticOptimization(expr = expr; optimizedExpr = optimizedExpr) ->
            [ visitExpr expr; visitExpr optimizedExpr ] |> List.concat
        | SynExpr.LibraryOnlyUnionCaseFieldGet(expr = expr) -> visitExpr expr
        | SynExpr.LibraryOnlyUnionCaseFieldSet(expr = expr; rhsExpr = rhsExpr) ->
            [ visitExpr expr; visitExpr rhsExpr ] |> List.concat
        | SynExpr.InterpolatedString(contents = contents) ->
            List.map visitInterpolatedStringParts contents |> List.concat
        | _ -> []

    and visitBinding (SynBinding(expr = expr)) = visitExpr expr

    and visitInterfaceImpl (SynInterfaceImpl(bindings = bindings; members = members)) =
        [ List.map visitBinding bindings |> List.concat
          List.map visitMemberDefn members |> List.concat ]
        |> List.concat

    and visitMemberDefn (memberDefn: SynMemberDefn) =
        match memberDefn with
        | SynMemberDefn.LetBindings(bindings = bindings) -> List.map visitBinding bindings |> List.concat
        | SynMemberDefn.Member(memberDefn = memberDefn) -> visitBinding memberDefn
        | SynMemberDefn.GetSetMember(memberDefnForGet = memberDefnForGet; memberDefnForSet = memberDefnForSet) ->
            [ Option.map visitBinding memberDefnForGet |> Option.defaultValue []
              Option.map visitBinding memberDefnForSet |> Option.defaultValue [] ]
            |> List.concat
        | SynMemberDefn.Interface(members = members) ->
            Option.map (List.map visitMemberDefn) members
            |> Option.defaultValue []
            |> List.concat
        | SynMemberDefn.NestedType(typeDefn = typeDefn) -> visitTypeDefn typeDefn
        | SynMemberDefn.AutoProperty(synExpr = synExpr) -> visitExpr synExpr
        | _ -> []

    and visitTypeDefn (synTypeDefn: SynTypeDefn) =
        match synTypeDefn with
        | SynTypeDefn.SynTypeDefn(typeRepr = typeRepr; members = members) ->
            match typeRepr with
            | SynTypeDefnRepr.ObjectModel(members = members) -> List.map visitMemberDefn members |> List.concat
            | SynTypeDefnRepr.Simple _ -> List.map visitMemberDefn members |> List.concat
            | _ -> []

    and visitModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.NestedModule(decls = decls) -> List.map visitModuleDecl decls |> List.concat
        | SynModuleDecl.Let(bindings = bindings) -> List.map visitBinding bindings |> List.concat
        | SynModuleDecl.Expr(expr = expr) -> visitExpr expr
        | SynModuleDecl.Types(typeDefns = typeDefns) -> List.map visitTypeDefn typeDefns |> List.concat
        | SynModuleDecl.Exception(exnDefn = SynExceptionDefn(members = members)) ->
            List.map visitMemberDefn members |> List.concat
        | _ -> []

    and visitModuleDecls (decls: SynModuleDecl list) =
        List.map visitModuleDecl decls |> List.concat

    and visitModuleOrNamespace (SynModuleOrNamespace(decls = decls): SynModuleOrNamespace) = visitModuleDecls decls

    let tryGetParameterCount (symbolUses: FSharpSymbolUse array) r =
        symbolUses
        |> Array.tryFind (fun s -> s.Range = r)
        |> Option.map (fun s ->
            match s.Symbol with
            | :? FSharpMemberOrFunctionOrValue as mfv -> Some mfv.CurriedParameterGroups.Count
            | _ -> None)
        |> Option.flatten

    [<Analyzer "PartialAppAnalyzer AST">]
    let partialAppAnalyzer: Analyzer =
        fun (context: Context) ->

            let applications =
                match context.ParseFileResults.ParseTree with
                | ParsedInput.ImplFile(ParsedImplFileInput.ParsedImplFileInput(contents = contents)) ->
                    contents |> List.collect visitModuleOrNamespace
                | _ -> []

            seq {
                for app in applications do
                    let ident, range, providedArgsCount = app
                    let parameterCount = tryGetParameterCount context.SymbolUsesOfFile range

                    match parameterCount with
                    | Some paramsCount ->
                        if providedArgsCount < paramsCount then // use LESS because of CEs, printf, etc.

                            let msg =
                                { Type = "Partial Application Analyzer"
                                  Message = $"partial application should not be used: {ident} at {range}"
                                  Code = "PA001"
                                  Severity = Warning
                                  Range = range
                                  Fixes = [] }

                            yield msg
                    | None -> ()
            }
            |> Seq.toList
