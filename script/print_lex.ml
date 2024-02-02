open Parser

let print_token result= 
  match result with
  NUM _  -> Printf.printf "NUM "
  | IF -> Printf.printf "IF "
  | ELSE  -> Printf.printf "ELSE "                  
  | WHILE  -> Printf.printf "WHILE "                 
  | SCAN  -> Printf.printf "SCAN "                  
  | SPRINT  -> Printf.printf "SPRINT "                
  | IPRINT  -> Printf.printf "IPRINT "                
  | INT  -> Printf.printf "INT "                   
  | RETURN  -> Printf.printf "RETURN "                
  | TYPE  -> Printf.printf "TYPE "                  
  | VOID  -> Printf.printf "VOID "                  
  | ID _  -> Printf.printf "ID "
  | STR _ -> Printf.printf "STR "
  | ASSIGN  -> Printf.printf "= "                     
  | EQ  -> Printf.printf "== "                    
  | NEQ  -> Printf.printf "!= "                    
  | GT  -> Printf.printf "> "                     
  | LT  -> Printf.printf "< "                     
  | GE  -> Printf.printf ">= "                    
  | LE  -> Printf.printf "<= "                    
  | PLUS  -> Printf.printf "+ "                     
  | MINUS  -> Printf.printf "- "                     
  | TIMES  -> Printf.printf "* "                     
  | DIV  -> Printf.printf "/ "                     
  | LB  -> Printf.printf "{ "                     
  | RB  -> Printf.printf "} "                     
  | LS  -> Printf.printf "[ "                     
  | RS  -> Printf.printf "] "                     
  | LP  -> Printf.printf "( "                     
  | RP  -> Printf.printf ") "                     
  | COMMA  -> Printf.printf ", "                     
  | SEMI  -> Printf.printf "; "                     
  | _ -> Printf.printf "Error: invalid str for token\n"
  
    
      
      

let main () = 
  let cin = 
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin in
  let lexbuf = Lexing.from_channel cin in
  try 
    while true do
      let result = Lexer.lexer lexbuf in
      print_token result;
    done
  with
  End_of_file -> Printf.printf "Lexer finished successfully\n"; exit 0
  | _ -> Printf.printf "There something went wrong \n"; exit 1

  

        
let () = try main () with
  Parsing.Parse_error -> print_string "syntax error\n"
