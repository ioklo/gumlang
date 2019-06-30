module Core

type SyncOption = Sync | Async

type FuncKind = 
    | Global
    | MemberNormal
    | MemberVirtual
    | MemberOverride
    | MemberStatic

type Accessibility = 
    | Private
    | Protected
    | PublicGet
    | Public

and ObjectInfoRuntimeContext = {
    mutable totalTypeParamCount: int

    mutable memberVars: VarInfoContext list
    mutable memberFuncs: FuncInfoContext list

    // ObjectInfo가 만들어 지고 난 다음에 계산되는 것
    mutable baseType: Type
    mutable InterfaceTypes: Type list

    mutable virtualFuncTable: FuncInfoContext list
    mutable interfaceVirtualFuncTables: FuncInfoContext list list
}

and ObjectInfoContext = {
    name: string
    typeParams: TypeParamInfoContext list
    extraSize: int

    mutable runtimeData: ObjectInfoRuntimeContext option
}

and FuncInfoRuntimeContext = {
    mutable totalTypeParamCount: int

    // Link 이후에 사용가능해지는 것들
    mutable virtualFuncIndex: int
}

and FuncInfoContext = {
    name: string;

    accessibility: Accessibility;
    funcKind: FuncKind;
    syncOption: SyncOption;

    contextObj: ObjectInfoContext;
    TypeParams : TypeParamInfoContext list;
    FuncType: FuncTypeContext;    

    // 계산 되는 것들
    mutable runtimeData: FuncInfoRuntimeContext option
}

and VarInfoContext = {    
    accessibility: Accessibility;
    contextObj: ObjectInfoContext
    varType: Type;
    name: string
}

and TypeParamInfoContext = {
    bParams: bool;
    name: string
}

and Type = 
    | ObjectType of ObjectTypeContext
    | TupleType of TupleTypeContext
    | UnionType of UnionTypeContext
    | FuncType of FuncTypeContext
    | RefType of RefTypeContext
    | TypeVar of TypeVarContext

and ObjectTypeContext = { objectInfo: ObjectInfoContext; typeArgs: Type list }
and TupleTypeItem = { bParams: bool; itemType: Type; name: string }
and TupleTypeContext = { itemTypes: TupleTypeItem list }
and UnionTypeContext = { types: Type list }
and FuncTypeContext = { retType: Type; paramType: Type }
and RefTypeContext = { bOut: bool; targetType: Type }
and TypeVarContext = { typeParamInfo: TypeParamInfoContext }


type FuncInst = { funcInfo: FuncInfoContext; typeArgs: Type list}

