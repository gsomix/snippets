// Original:
// https://github.com/true-grue/Compiler-Development/wiki/Кратчайшее-введение-в-создание-компилятора-(Python)

// SSA: 
// https://ru.wikipedia.org/wiki/SSA

open System
open System.Text

let [<Literal>] C_CODE = """#include <stdio.h>
int main(int argc, char** argv) {{
{0}
printf("%d\n", {1});
return 0;
}}"""

type IR =
    | Push of int
    | Op of string

type Env = {
    stack: string list
    name_cnt: int
}

let emptyEnv = { stack = []; name_cnt = 0 }

let scan (source: string) =
    let tokens = source.Split ' '
    [ for x in tokens -> 
        if Char.IsDigit x.[0] then
            Push (int(x))
        else
            Op x 
    ]

let trans (ir: IR list) =
    let transInstr (env: Env, code: StringBuilder) = function
    | Push value -> 
        let code = code.AppendLine (sprintf "int t%d = %d;" env.name_cnt value)
        let stack = (sprintf "t%d" env.name_cnt) :: env.stack
        { env with stack = stack; name_cnt = env.name_cnt + 1}, code
    | Op op -> 
        let (a :: b :: stack) = env.stack
        let code = code.AppendLine (sprintf "int t%d = %s %s %s;" env.name_cnt b op a)
        let stack = (sprintf "t%d" env.name_cnt) :: stack
        { env with stack = stack; name_cnt = env.name_cnt + 1}, code

    let env, code = ir |> List.fold transInstr (emptyEnv, StringBuilder())
    code, List.head env.stack

let rpnToC (source: string) = 
    let code, lastVar = source |> scan |> trans
    String.Format(C_CODE, code, lastVar)

printfn "%s" (rpnToC "2 2 + 3 -") // 2 + 2 - 3 = 1

(* Output:

#include <stdio.h>
int main(int argc, char** argv) {
    int t0 = 2;
    int t1 = 2;
    int t2 = t0 + t1;
    int t3 = 3;
    int t4 = t2 - t3;

    printf("%d\n", t4);
    return 0;
}

*)