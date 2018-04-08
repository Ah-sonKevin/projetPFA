{
  open Objet
  open Camera
  exception Erreur_de_syntaxe
}
  
let digit = ['0'-'9']
let espaces = [' ' '\t']*
                let coupleDigit = '(' (digit+ as x1) ',' (digit+ as x2) ')'
let float = '-'? digit+ '.' digit+
              let coupleFloat = '(' (float as x1) ',' (float as x2) ')'
let id = ['a'-'z' 'A'-'Z' '_']+
           let mot = (id | ['.' ',' ';' ':' '(' ')'] | digit | float |espaces)+
                       let nomFichier = (['a'-'z' 'A'-'Z' '_' '.' '/'] | digit)+
                                          let nomFichierBMP = nomFichier '.' "bmp" 
let nomFichierTXT = nomFichier '.' "txt" 

                      rule sceneText r = parse
                  |"(*" mot "*)" {sceneText r lexbuf  }
                   |"Scene" espaces digit espaces "\n\n" {print_string "scene";sceneGrav r lexbuf }
                   |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
                   |eof {raise Erreur_de_syntaxe}

and sceneGrav r = parse
              |"Gravity" espaces (float as f) "\n\n" {(float_of_string f, entities r lexbuf )}
              |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
              |eof {raise Erreur_de_syntaxe}


and entities r = parse
             |"Genre" espaces {
let (genre,pos,speed,maxSpeed,pv,(g,m,d,s), y) = (genre r lexbuf  ) in 
List.cons (Objet.create genre pos speed maxSpeed pv (g,m,d,s) r ) y }
             |'\n' {entities r lexbuf }
             |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
             |eof {[]}

and genre r = parse
          |"Personnage" espaces '\n' {
let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos r lexbuf ) in ((Objet.Personnage),pos, speed, maxSpeed, pv, (g,m,d,s),  y )}
          |"Ennemi\n"
{ let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos r lexbuf ) in ((Objet.Ennemi),pos, speed, maxSpeed, pv, (g,m,d,s),  y )}
          |"Wall\n" 
{ let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos r lexbuf ) in ((Objet.Wall),pos, speed, maxSpeed, pv, (g,m,d,s),  y ) }
          |"Background\n" 
{ let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos r lexbuf ) in ((Objet.Background),pos, speed, maxSpeed, pv, (g,m,d,s),  y ) }
	  |"Door" espaces (nomFichierTXT as t) espaces "\n" 
{ let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos r lexbuf ) in ((Objet.Door t ),pos, speed, maxSpeed, pv, (g,m,d,s),  y ) } 
          |"Projectile\n" 
{ let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos r lexbuf ) in ((Objet.Projectile),pos, speed, maxSpeed, pv, (g,m,d,s),  y ) }
          |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
          |eof {raise Erreur_de_syntaxe}

and pos r = parse
        |"Position" espaces (coupleDigit as c) '\n'{
let (speed,maxSpeed,pv,(g,m,d,s), y) = (vit r lexbuf ) in ((int_of_string x1,int_of_string x2), speed, maxSpeed, pv, (g,m,d,s),  y )
}
        |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
        |eof {raise Erreur_de_syntaxe}

and animM l r = parse
            |(nomFichierBMP as c) '\n' {animM (c::l) r lexbuf  }
            |"\n" 
{let ((d,s),y) = (animD [] r lexbuf  ) in ((l,d,s),  y )}
            |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
            |eof {raise Erreur_de_syntaxe}

and animG l r = parse
            |(nomFichierBMP as c) '\n' {(animG (c::l) r lexbuf )}
            |"\n"
{ let ((m,d,s), y) = (animM [] r lexbuf  ) in ((l,m,d,s),  y )}
            |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
            |eof {raise Erreur_de_syntaxe}

and animD l r = parse
            |(nomFichierBMP as c) '\n' {(animD (c::l) r lexbuf )}
            |"\n" 
{ let (s, y) = (animS [] r lexbuf  ) in ((l,s),  y )}
            |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
 |eof {raise Erreur_de_syntaxe}

and animS l r = parse
            |(nomFichierBMP as c) '\n' {animS (c::l) r lexbuf  } 
            |"\n"
{let y = (entities r lexbuf ) in (l,  y )}
            |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
            |eof {let y = (entities r lexbuf ) in (l,  y )}

and anim r = parse
         |"Animation" espaces '\n' espaces {animG [] r lexbuf  }
         |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
         |eof {raise Erreur_de_syntaxe}

and pv r = parse
       |"PV" espaces (digit+ as c) '\n' 
{let ((g,m,d,s), y) = (anim r lexbuf ) in (c, (g,m,d,s),  y )} 
       |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
       |eof {raise Erreur_de_syntaxe}

and vit r = parse
        |"Vitesse" espaces (coupleFloat as c) '\n' 
{let (maxSpeed,pv,(g,m,d,s), y) = ((vitMax r lexbuf )) in ((float_of_string x1, float_of_string x2), maxSpeed, pv, (g,m,d,s),  y ) }
        |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
        |eof {raise Erreur_de_syntaxe}

and vitMax r = parse
           |"VitesseMax" espaces (coupleFloat as c) '\n'
{let (pv,(g,m,d,s), y) = (pv r lexbuf ) in ((float_of_string x1, float_of_string x2) , int_of_string pv, (Array.of_list g,Array.of_list m,Array.of_list d,Array.of_list s), y ) }
           |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
           |eof {raise Erreur_de_syntaxe}



{
  let lex file r = 
    let f = open_in file
    in let s = sceneText r (Lexing.from_channel f) in close_in f; s 
                  
}
