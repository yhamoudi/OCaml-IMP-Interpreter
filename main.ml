open Execute


let lexbuf = Lexing.from_channel stdin

let parse () = Parser.main Lexer.token lexbuf

let imp () =
    let result = parse () in
        execute result;
        print_newline();
        flush stdout

let _ = imp()
