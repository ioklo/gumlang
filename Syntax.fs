module Syntax

type Operator = 
    | Plus
    | Minus
    | Star
    | Slash
    | PlusPlus             // ++
    | MinusMinus           // --
    | EqualEqual           // ==
    | Less 
    | Excl                 // !
    | Indexer

type Argument = { bOut: bool; bParams: bool; exp: Exp }

and Exp = 
    | OperatorExp of OperatorExpContext
    | NullLiteral
    | IntLiteral of IntLiteralContext
    | BoolLiteral of BoolLiteralContext
    | StringLiteral of StringLiteralContext
    | InterfaceConstructor of InterfaceConstructorContext
    | ListConstructor of ListConstructorContext
    | TupleConstructor of TupleConstructorContext
    | UnionConstructor of UnionConstructorContext
    | FuncConstructor of FuncConstructorContext
    | IDExp of IDExpContext
    | NewExp of NewExpContext
    | CallExp of CallExpContext
    | AssignExp of AssignExpContext
    | MemberExp of MemberExpContext
    | ConditionalExp of ConditionalExpContext
    | NullConditionalExp of NullConditionalExpContext
    | AwaitExp of AwaitExpContext
    | RefExp of RefExpContext
    | DerefExp of DerefExpContext

and Stmt = 
    | BlankStmt
    | BlockStmt of BlockStmtContext
    | VarDeclStmt of VarDeclStmtContext
    | ReturnStmt of ReturnStmtContext
    | ContinueStmt
    | BreakStmt
    | IfStmt of IfStmtContext
    | TypeGuardStmt of TypeGuardStmtContext
    | WhileStmt of WhileStmtContext
    | DoWhileStmt of DoWhileStmtContext
    | ForStmt of ForStmtContext
    | ForeachStmt of ForeachStmtContext
    | ExpStmt of ExpStmtContext
    | YieldReturnStmt of YieldReturnStmtContext
    | YieldBreakStmt 
    | TaskStmt of TaskStmtContext
    | QueueStmt of QueueStmtContext
    | AsyncStmt of AsyncStmtContext
    | CallConstructorStmt of CallConstructorStmtContext

and OperatorExpContext = { 
    operator: Operator; 
    operands: Argument list; 
    operatorSearchTargetCount: int;
    funcInst: Core.FuncInst // funcInst들은 타입 체킹 후에 세팅된다
}
and IntLiteralContext = { value: int }
and BoolLiteralContext = { value: bool }
and StringLiteralContext = { value : string }
and InterfaceConstructorContext = { interfaceType: Core.Type; exp: Exp }
and ListConstructorContext = { elems: Exp list; elemType: Core.Type }
and TupleItem = { name: string; exp: Exp }
and TupleConstructorContext = { items: TupleItem list }
and UnionConstructorContext = { unionType: Core.Type; exp: Exp; }
and FuncConstructorContext = { thisType: Core.Type; funcInst: Core.FuncInst }
and IDExpContext = {name: string; typeArgs: Core.Type list }
and NewExpContext = { objectType: Core.Type; args: Argument list; funcInst: Core.FuncInst }
and CallExpContext = { 
    funcExp: Exp; 
    args: Argument list;

    // 타입 체킹 후에 세팅된다
    ContextExp: Exp;
    funcInst: Core.FuncInst;
    virtualFuncIndex: int
}
and AssignExpContext = { leftExp: Exp; rightExp: Exp }    
and MemberExpContext = { exp: Exp; id: IDExpContext } 
and ConditionalExpContext = { cond: Exp; trueExp: Exp; falseExp: Exp } 
and NullConditionalExpContext = { exp: Exp; nullExp: Exp }
and AwaitExpContext = { callExp: CallExpContext }
and RefExpContext = { exp: Exp } 
and DerefExpContext = { exp: Exp } 
and VarDeclItem = { name: string; exp: Exp option }
and BlockStmtContext = { stmts: Stmt list }
and VarDeclStmtContext = { bImplicitType: bool; varType: Core.Type; items: VarDeclItem list }

and ReturnStmtContext = { exp: Exp option; }

and IfStmtContext = { cond: Exp; trueBody: Stmt; falseBody: Stmt }    
and TypeGuardStmtContext = { idExp: IDExpContext; typeGuardType: Core.Type; trueBody : Stmt; falseBody: Stmt }
and WhileStmtContext = {cond : Exp; body: Stmt; }
and DoWhileStmtContext = { body: Stmt; cond: Exp }
and ForStmtContext = { initializer: Stmt; cond: Exp; condExp: Exp; body: Stmt }
and ForeachStmtContext = {
    typeParams: Core.TypeParamInfoContext list; 
    bImplicitType: bool;
    elemType: Core.Type;
    elemName: string;        
    enumExp: Exp;
    body : Stmt;
    rewritedStmt: Stmt // TypeChecking 시에 덮어 씌워진다
    // TODO: ASTRewriter 등으로 
} 
and ExpStmtContext = { exp: Exp; }
and YieldReturnStmtContext = { exp:Exp; }
and TaskStmtContext = { body: BlockStmtContext }
and QueueStmtContext = { callExp: CallExpContext }
and AsyncStmtContext = { callExp: CallExpContext } 
and CallConstructorStmtContext = { constructorType: Core.Type; args: Argument list; funcInst: Core.FuncInst } // ASTBuilder가 만들어 내는 Stmt, 코드에서 만들 수 없음
