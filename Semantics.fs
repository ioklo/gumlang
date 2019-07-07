module Semantics

open Syntax
open System
open System.Collections.Immutable

type Continuation = 
    | Terminate
    | Continue of (State -> Continuation * State)

and State = { stack: ImmutableStack<Object> }

let inline (|?) (a: 'a option) b = if a.IsSome then a.Value else b

let PushScope (state: State): State = state // TODO: 임시
let PopScope (state: State): State = state // TODO: 임시

let Push (value: Object) (state: State) = 
    { state with stack = state.stack.Push(value) }

let Pop<'T> (state: State): 'T * State = 
    let value: 'T = downcast state.stack.Peek()    
    (value, { state with stack = state.stack.Pop() } ) 
    
// 현재 state의 next에는 
let rec StepStmt (stmt: Stmt) (cont: Continuation) (state: State) : (Continuation * State) = 
    match stmt with 
    | Stmt.Blank -> (cont, state)
    | Stmt.Block blockStmt             -> StepBlockStmt blockStmt cont state
    | Stmt.VarDecl varDeclStmt         -> StepVarDeclStmt varDeclStmt cont state
    | Stmt.Return returnStmt           -> StepReturnStmt returnStmt cont state
    | Stmt.Continue                    -> StepContinueStmt cont state
    | Stmt.Break                       -> StepBreakStmt cont state
    | Stmt.If ifStmt                   -> StepIfStmt ifStmt cont state
    | Stmt.TypeGuard typeGuardStmt     -> StepTypeGuardStmt typeGuardStmt cont state
    | Stmt.While whileStmt             -> StepWhileStmt whileStmt cont state
    | Stmt.DoWhile doWhileStmt         -> StepDoWhileStmt doWhileStmt cont state
    | Stmt.For forStmt                 -> StepForStmt forStmt cont state
    | Stmt.Foreach foreachStmt         -> StepForeachStmt foreachStmt cont state
    | Stmt.Exp expStmt                 -> StepExpStmt expStmt cont state
    | Stmt.YieldReturn yieldReturnStmt -> StepYieldReturnStmt yieldReturnStmt cont state
    | Stmt.YieldBreak                  -> StepYieldBreakStmt cont state

and StepBlockStmt (blockStmt: BlockStmt) (cont: Continuation) (state: State) : (Continuation * State) =

    let rec StepBlockStmtStart state = 
        printfn "Block Begin"
        (Continue (StepStmts blockStmt.stmts), state)

    and StepStmts stmts state =
        match stmts with
        | hd::tl -> StepStmt hd (Continue (StepStmts tl)) state
        | [] -> StepBlockStmtEnd state

    and StepBlockStmtEnd state = 
        printfn "Block End"
        (cont, state)

    StepBlockStmtStart state 

and StepVarDeclStmt (varDeclStmt: VarDeclStmt) (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepReturnStmt (returnStmt: ReturnStmt) (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepContinueStmt (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepBreakStmt (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepIfStmt (ifStmt: IfStmt) (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepTypeGuardStmt (typeGuardStmt: TypeGuardStmt) (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepWhileStmt (whileStmt: WhileStmt) (cont: Continuation) (state: State) : (Continuation * State) = 
    let rec StepWhileBegin state = 
        let state = PushScope state
        StepCond state

    and StepCond state = 
        StepExp whileStmt.cond (Continue StepCondEval) state

    and StepCondEval state = 
        let (condValue, state) = Pop<bool> state

        if condValue then 
            StepStmt whileStmt.body (Continue StepCond) state
        else 
            StepWhileEnd state

    and StepWhileEnd state =
        let state = PopScope state
        (cont, state)

    StepWhileBegin state
    
and StepDoWhileStmt (doWhileStmt:DoWhileStmt) (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepForStmt (forStmt:ForStmt) (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepForeachStmt (foreachStmt: ForeachStmt) (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepExpStmt (expStmt: ExpStmt) (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepYieldReturnStmt (yieldReturnStmt: YieldReturnStmt) (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())
and StepYieldBreakStmt (cont: Continuation) (state: State) : (Continuation * State) = raise (System.Exception())    

and StepExp (exp:Exp) (cont: Continuation) (state: State) = 
    match exp with
    | Exp.UnaryOperator unaryOperatorExp       -> StepUnaryOperatorExp unaryOperatorExp cont state
    | Exp.BinaryOperator binaryOperatorExp     -> StepBinaryOperatorExp binaryOperatorExp cont state    
    | Exp.ShortcutOperator shortcutOperatorExp -> StepShortcutOperatorExp shortcutOperatorExp cont state
    | Exp.Indexer indexerExp                   -> StepIndexerExp indexerExp cont state
    | Exp.NullLiteral                          -> StepNullLiteralExp cont state
    | Exp.IntLiteral intLiteralExp             -> StepIntLiteralExp intLiteralExp cont state 
    | Exp.BoolLiteral boolLiteralExp           -> StepBoolLiteralExp boolLiteralExp cont state 
    | Exp.StringLiteral stringLiteralExp       -> StepStringLiteralExp stringLiteralExp cont state     
    | Exp.ListConstructor listConstructorExp   -> StepListConstructorExp listConstructorExp cont state   
    | Exp.TupleConstructor tupleConstructorExp -> StepTupleConstructorExp tupleConstructorExp cont state 
    | Exp.ID idExp                             -> StepIDExp idExp cont state 
    | Exp.New newExp                           -> StepNewExp newExp cont state 
    | Exp.Call callExp                         -> StepCallExp callExp cont state 
    | Exp.Assign assignExp                     -> StepAssignExp assignExp cont state  
    | Exp.Member memberExp                     -> StepMemberExp memberExp cont state 
    | Exp.Conditional conditionalExp           -> StepConditionalExp conditionalExp cont state 
    | Exp.NullConditional nullConditionalExp   -> StepNullConditionalExp nullConditionalExp cont state 
    | Exp.Await awaitExp                       -> StepAwaitExp awaitExp cont state 
    | Exp.Launch launchExp                     -> StepLaunchExp launchExp cont state 

and StepUnaryOperatorExp (unaryOperatorExp: UnaryOperatorExp) (cont: Continuation) (state: State) = raise (System.Exception())
and StepBinaryOperatorExp (binaryOperatorExp: BinaryOperatorExp) (cont: Continuation) (state: State) = raise (System.Exception())    
and StepShortcutOperatorExp (shortcutOperatorExp: ShortcutOperatorExp) (cont: Continuation) (state: State) = raise (System.Exception())
and StepIndexerExp (indexerExp: IndexerExp) (cont: Continuation) (state: State) = raise (System.Exception())

and StepNullLiteralExp (cont: Continuation) (state: State) = 
    (cont, Push null state)

and StepIntLiteralExp (intLiteralExp: IntLiteralExp) (cont: Continuation) (state: State) = 
    (cont, Push intLiteralExp.value state)
    
and StepBoolLiteralExp (boolLiteralExp: BoolLiteralExp) (cont: Continuation) (state: State) = 
    (cont, Push boolLiteralExp.value state)

and StepStringLiteralExp (stringLiteralExp: StringLiteralExp) (cont: Continuation) (state: State) = 
    (cont, Push stringLiteralExp.value state)

and StepListConstructorExp (listConstructorExp: ListConstructorExp) (cont: Continuation) (state: State) = 

    let mutable result = new ResizeArray<Object>()

    let rec StepListElems elems state =        
        match elems with
        | hd::tl -> StepExp hd (Continue (StepCollectAndProceed tl)) state
        | [] -> (cont, state)

    and StepCollectAndProceed elems state = 
        match elems with
        | [] -> 
            (cont, Push result state)

        | _ ->
            let (value, newState) = Pop state
            result.Add(value)
            ((Continue (StepListElems elems)), newState)

    StepListElems listConstructorExp.elems state

and StepTupleConstructorExp (tupleConstructorExp: TupleConstructorExp) (cont: Continuation) (state: State) = 

    let mutable result = new ResizeArray<Tuple<string, Object>>()
    let mutable count = 0

    let rec StepTupleElems (elems: TupleElem list) state =
        match elems with
        | hd::tl -> StepExp hd.exp (Continue (StepCollectAndProceed hd.name tl)) state
        | [] -> (cont, state)

    and StepCollectAndProceed name elems state = 

        if elems = [] then 
            (cont, Push result state)
        else         
            let elemName = name |? sprintf "Item%d" count
            count <- count + 1

            let (value, newState) = Pop state

            result.Add((elemName, value))
            ((Continue (StepTupleElems elems)), newState)

    StepTupleElems tupleConstructorExp.elems state

and StepIDExp (idExp: IDExp) (cont: Continuation) (state: State) = raise (System.Exception()) 
and StepNewExp (newExp: NewExp) (cont: Continuation) (state: State) = raise (System.Exception()) 
and StepCallExp (callExp: CallExp) (cont: Continuation) (state: State) = raise (System.Exception()) 
and StepAssignExp (assignExp: AssignExp) (cont: Continuation) (state: State) = raise (System.Exception())    
and StepMemberExp (memberExp: MemberExp) (cont: Continuation) (state: State) = raise (System.Exception()) 
and StepConditionalExp (conditionalExp: ConditionalExp) (cont: Continuation) (state: State) = raise (System.Exception()) 
and StepNullConditionalExp (nullConditionalExp: NullConditionalExp) (cont: Continuation) (state: State) = raise (System.Exception()) 
and StepAwaitExp (awaitExp: AwaitExp) (cont: Continuation) (state: State) = raise (System.Exception()) 
and StepLaunchExp (launchExp: LaunchExp) (cont: Continuation) (state: State) = raise (System.Exception()) 

// 끝 신호가 날때까지 계속 
let Run (fileUnit: FileUnit) = 

    // fileUnit중에서 
    let initCont = 
        let stmts =
            fileUnit.elems
            |> List.filter (fun elem ->
                match elem with 
                | Stmt _ -> true
                | _ -> false)

            |> List.map (fun elem -> 
                match elem with 
                | Stmt s -> s 
                | _ ->  raise (System.Exception ()))

        let blockStmt = { stmts = stmts }

        (Continue (StepBlockStmt blockStmt Terminate))
    
    let initState = { stack = ImmutableStack<Object>.Empty }

    let rec RunInner cont state = 
        match cont with 
        | Terminate -> ()
        | Continue cont -> 
            let newCont, newState = cont state
            RunInner newCont newState

    RunInner initCont initState
