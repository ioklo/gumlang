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

and RuntimeObjectInfo = {
    mutable totalTypeParamCount: int

    mutable memberVars: VarInfoContext list
    mutable memberFuncs: FuncInfo list

    // ObjectInfo가 만들어 지고 난 다음에 계산되는 것
    mutable baseType: Type
    mutable InterfaceTypes: Type list

    mutable virtualFuncTable: FuncInfo list
    mutable interfaceVirtualFuncTables: FuncInfo list list
}

and ObjectInfo = {
    name: string
    typeParams: TypeParamInfo list
    extraSize: int

    mutable runtimeData: RuntimeObjectInfo option
}

and RuntimeFuncInfo = {
    mutable totalTypeParamCount: int

    // Link 이후에 사용가능해지는 것들
    mutable virtualFuncIndex: int
}

and FuncInfo = {
    name: string;

    accessibility: Accessibility;
    funcKind: FuncKind;
    syncOption: SyncOption;

    contextObj: ObjectInfo;
    TypeParams : TypeParamInfo list;
    FuncType: FuncType;    

    // 계산 되는 것들
    mutable runtimeData: RuntimeFuncInfo option
}

and VarInfoContext = {    
    accessibility: Accessibility;
    contextObj: ObjectInfo
    varType: Type;
    name: string
}

and TypeParamInfo = {
    bParams: bool;
    name: string
}

and Type = 
    | Object of ObjectType
    | Tuple of TupleType
    | Union of UnionType
    | Func of FuncType
    | Ref of RefType
    | Var of TypeVar

and ObjectType = { objectInfo: ObjectInfo; typeArgs: Type list }
and TupleTypeItem = { bParams: bool; itemType: Type; name: string }
and TupleType = { itemTypes: TupleTypeItem list }
and UnionType = { types: Type list }
and FuncType = { retType: Type; paramType: Type }
and RefType = { bOut: bool; targetType: Type }
and TypeVar = { typeParamInfo: TypeParamInfo }


type FuncInst = { funcInfo: FuncInfo; typeArgs: Type list}

