// Learn more about F# at http://fsharp.org

open System
open Syntax
open Semantics

open FSharp.Text.Lexing

let parse str = 
    let rec printList list =
        match list with
        | hd::tl -> printfn "%d " hd ; printList tl
        | [] -> ()

    let lexbuf = LexBuffer<char>.FromString str
    let lst = Parser.start Lexer.token lexbuf
    0

[<EntryPoint>]
let main argv =    
    let str = "
    interface A 
    {
        (A | B) S(out int& x);
    }
"
    parse str
    

