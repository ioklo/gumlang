// Learn more about F# at http://fsharp.org

open System
open Syntax

let Execute (stmts: Stmt list) = ()

[<EntryPoint>]
let main argv =    
    let intInfo: Core.ObjectInfoContext = { name = "int"; typeParams = []; extraSize = 0; runtimeData = None }
    let intType = Core.Type.ObjectType { objectInfo = intInfo; typeArgs = [] }
    let stmts = [Stmt.VarDeclStmt { bImplicitType = false; varType = intType;  items = [ { name = "a"; exp = None } ] } ]
    Execute stmts; 0

