// Original:
// https://github.com/true-grue/Compiler-Development/wiki/Кратчайшее-введение-в-создание-компилятора-(Python)

open System

let [<Literal>] ST_SIZE = 100
let [<Literal>] C_CODE = """#include <stdio.h>
int main(int argc, char** argv) {{ 
int st[{0}], sp = 0;
{1}
printf("%d\n", st[sp - 1]);
return 0;
}}"""

type IR =
    | Push of int
    | Op of string

let scan (source: string) =
    let tokens = source.Split ' '
    [ for x in tokens -> 
        if Char.IsDigit x.[0] then
            Push (int(x))
        else
            Op x 
    ]

let trans ir =
    [ for instr in ir do
        match instr with
        | Push value -> 
            yield sprintf "st[sp] = %d;" value
            yield "sp += 1;"
        | Op op -> 
            yield sprintf "st[sp - 2] = st[sp - 2] %s st[sp - 1];" op
            yield "sp -= 1;" 
    ] |> String.concat "\n"

let rpnToC source = 
    let code = source |> scan |> trans
    String.Format(C_CODE, ST_SIZE, code)

printfn "%s" (rpnToC "2 2 + 3 -") // 2 + 2 - 3 = 1

(* Output:

#include <stdio.h>
int main(int argc, char** argv) {
    int st[100], sp = 0;
    st[sp] = 2;
    sp += 1;
    st[sp] = 2;
    sp += 1;
    st[sp - 2] = st[sp - 2] + st[sp - 1];
    sp -= 1;
    st[sp] = 3;
    sp += 1;
    st[sp - 2] = st[sp - 2] - st[sp - 1];
    sp -= 1;
    printf("%d\n", st[sp - 1]);
    return 0;
}

*)