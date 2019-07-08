module Syntax

type UnaryOperatorKind = 
    | PlusPlus             // ++, unary, postfix
    | MinusMinus           // --, unary, postfix
    | Excl                 // !, unary

type BinaryOperatorKind = 
    | Plus
    | Minus
    | Star
    | Slash
    | EqualEqual           // ==
    | ExclEqual            // !=
    | Less                 
    | LessEqual
    | Greater              // GT, GTEqual은 Less, LessEqual로 변환된다
    | GreaterEqual         


type ShortcutOperatorKind = 
    | LogicalOr 
    | LogicalAnd

type Accessibility = 
    | Private
    | Protected
    | PublicGet
    | Public
    
type MemberFuncKind = 
    | Normal
    | Virtual
    | Override
    | Static    

type LiteralTypeID = {
    name: string
}

and TypeInstTypeID = {
    typeID: LiteralTypeID
    typeArgs: TypeID list
}

and TupleTypeElem = {
    elemType: TypeID
    name: string option
}

and TupleTypeID = {
    elems: TupleTypeElem list
}

and UnionTypeID = {
    elems: TypeID list 
}

and NullableTypeID = {
    typeID: TypeID // default, tuple, typeInst
}

and TypeID = 
    | Literal of LiteralTypeID // A
    | TypeInst of TypeInstTypeID // A<B?>
    | Nullable of NullableTypeID // A?
    | Tuple of TupleTypeID // (A, B?, C)
    | Union of UnionTypeID // (A | B)

type TypeParam = { bParams: bool; name: string }

type FuncParam = { 
    bParams: bool
    bOut: bool
    bRef: bool
    typeID: TypeID
    name: string 
}

type TypeParamConstraint = {
    typeVar: string
    constraintTypes: TypeID list
}

and ClassDecl = { 
    name: string
    typeParams: TypeParam list
    typeParamConstraints: TypeParamConstraint list
    baseTypes: TypeID list
    memberDecls: MemberDecl list 
}

and StructDecl = { 
    name: string
    typeParams: TypeParam list
    typeParamConstraints: TypeParamConstraint list
    baseTypes: TypeID list
    memberDecls: MemberDecl list 
}

and InterfaceMemberDecl =
    | VarDecl of InterfaceMemberVarDecl
    | FuncDecl of InterfaceMemberFuncDecl 

and InterfaceMemberVarDecl = {
    bStatic: bool
    varType: TypeID
    varNames: string list
}

and InterfaceMemberFuncDecl = {
    bStatic: bool
    bAsync: bool
    retType: TypeID
    name: string
    funcParams: FuncParam list    
}

and InterfaceDecl = { 
    name: string
    typeParams: TypeParam list
    typeParamConstraints: TypeParamConstraint list
    baseTypes: TypeID list
    memberDecls: InterfaceMemberDecl list
}

and MemberDecl = 
    | Constructor of MemberConstructorDecl
    | Var of MemberVarDecl
    | Func of MemberFuncDecl

and MemberVarDecl = {
    accessibility: Accessibility
    varType: TypeID
    names: string list
}

and MemberFuncDecl = {
    accessibility: Accessibility
    funcKind: MemberFuncKind
    bAsync: bool

    retTypeID: TypeID
    name: string
    typeParams: TypeParam list
    funcParams: FuncParam list
    
    stmts: Stmt list
}

and MemberConstructorDecl = {
    accessibility: Accessibility // Only Public, Protected allowed
    bAsync: bool

    retTypeID: TypeID
    name: string
    typeParams: TypeParam list
    funcParams: FuncParam list

    baseConstructorArguments: Arguments

    stmts: Stmt list
}

and VarDecl = {
    varType: TypeID option // implicit 인 경우 type이 존재하지 않는다
    varDeclElems: VarDeclElem list
}

and GlobalFuncDecl = {    

    bAsync: bool

    retTypeID: TypeID
    name: string
    typeParams: TypeParam list
    typeParamConstraints: TypeParamConstraint list
    funcParams: FuncParam list

    stmts: Stmt list
}

and GlobalVarDecl = VarDecl    

and Argument = { bOut: bool; exp: Exp }
and Arguments = { args: Argument list; paramsExp: Exp option }

and VarDeclElem = { name: string; exp: Exp option }
    
and Exp = 
    | UnaryOperator of UnaryOperatorExp
    | BinaryOperator of BinaryOperatorExp    
    | ShortcutOperator of ShortcutOperatorExp
    | Indexer of IndexerExp
    | NullLiteral 
    | IntLiteral of IntLiteralExp 
    | BoolLiteral of BoolLiteralExp 
    | StringLiteral of StringLiteralExp     
    | ListConstructor of ListConstructorExp   
    | TupleConstructor of TupleConstructorExp 
    | ID of IDExp 
    | New of NewExp 
    | Call of CallExp 
    | Assign of AssignExp  
    | Member of MemberExp 
    | Conditional of ConditionalExp 
    | NullConditional of NullConditionalExp 
    | Await of AwaitExp 
    | Launch of LaunchExp 

and Stmt = 
    | Blank
    | Block of BlockStmt
    | VarDecl of VarDeclStmt
    | Return of ReturnStmt
    | Continue
    | Break
    | If of IfStmt
    | TypeGuard of TypeGuardStmt
    | While of WhileStmt
    | DoWhile of DoWhileStmt
    | For of ForStmt
    | Foreach of ForeachStmt
    | Exp of ExpStmt
    | YieldReturn of YieldReturnStmt
    | YieldBreak 
    
and UnaryOperatorExp = { kind: UnaryOperatorKind; operand: Exp }
and BinaryOperatorExp = { kind: BinaryOperatorKind; leftOperand: Exp; rightOperand: Exp }
and ShortcutOperatorExp = { kind: ShortcutOperatorKind; leftOperand: Exp; rightOperand: Exp }
and IndexerExp = { operands: Exp list }
and IntLiteralExp = { value: int }
and BoolLiteralExp = { value: bool }
and StringLiteralExp = { value : string }
and ListConstructorExp = { elems: Exp list }
and TupleElem = { name: string option; exp: Exp }
and TupleConstructorExp = { elems: TupleElem list }
and IDExp = { name: string; typeArgs: TypeID list }
and NewExp = { objectType: TypeID; args: Arguments }

and CallExp = { funcExp: Exp; args: Arguments }
and AssignExp = { leftExp: Exp; rightExp: Exp }    
and MemberExp = { exp: Exp; id: IDExp } 
and ConditionalExp = { cond: Exp; trueExp: Exp; falseExp: Exp } 
and NullConditionalExp = { exp: Exp; nullExp: Exp }
and AwaitExp = { exp: Exp }
and RefExp = { exp: Exp } 
and DerefExp = { exp: Exp } 
and LaunchExp = { exp: Exp option; parallelStmts: Stmt }

and BlockStmt = { stmts: Stmt list }
and VarDeclStmt = VarDecl
and ReturnStmt = { exp: Exp option; }
and IfStmt = { cond: Exp; trueBody: Stmt; falseBody: Stmt option }    
and TypeGuardStmt = { idExp: IDExp; typeGuardType: TypeID; trueBody: Stmt; falseBody: Stmt option }
and WhileStmt = { cond : Exp; body: Stmt; }
and DoWhileStmt = { body: Stmt; cond: Exp }
and ForStmt = { initializer: Stmt option; cond: Exp option; condExp: Exp option; body: Stmt }
and ForeachStmt = { typeParams: TypeParam list; elemType: TypeID option; elemName: string; enumExp: Exp; body: Stmt; } 
and ExpStmt = { exp: Exp; }
and YieldReturnStmt = { exp:Exp; }

type FileUnitElem = 
    | Class of ClassDecl
    | Struct of StructDecl
    | Interface of InterfaceDecl
    | Func of GlobalFuncDecl
    | Stmt of Stmt

type FileUnit = {
    elems: FileUnitElem list
}

