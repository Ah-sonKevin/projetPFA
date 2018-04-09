{
  open Objet
  open Camera
  exception Erreur_de_syntaxe
  exception ErreurScene
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

rule sceneText perso r = parse
   |"(*" (mot | espaces)+  "*)" {sceneText perso r lexbuf  }
   |"Scene" espaces digit espaces "\n\n" {sceneGrav perso r lexbuf }
   |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
   |eof {raise Erreur_de_syntaxe}

and sceneGrav perso r = parse
              |"Gravity" espaces (float as f) "\n\n" {(float_of_string f, entities perso r lexbuf )}
              |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
              |eof {raise Erreur_de_syntaxe}


and entities perso r = parse
             |"Genre" espaces {
    let (genre,pos,speed,maxSpeed,pv,(g,m,d,s), y) = (genre perso r lexbuf  ) in 
    List.cons (Objet.create genre pos speed maxSpeed pv (g,m,d,s) r ) y }
             |'\n' {entities perso r lexbuf }
             |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
             |eof {[]}

and genre perso r = parse
          |"Personnage" espaces '\n' 
              {let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in ((Objet.Personnage),pos, (Objet.getSpeed perso), (Objet.getMaxSpeed perso), (Objet.getPV perso), (Objet.getAnim perso), y)}
          |"Plateforme" espaces '\n' 
              {let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in ((Objet.Plateforme),pos, speed, maxSpeed, pv, (g,m,d,s),  y)} 
          |"Ennemi\n"
              {let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in ((Objet.Ennemi),pos, speed, maxSpeed, pv, (g,m,d,s), y)}
          |"Wall\n" 
              {let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in ((Objet.Wall),pos, speed, maxSpeed, pv, (g,m,d,s),  y)}
          |"Background\n" 
              {let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in ((Objet.Background),pos, speed, maxSpeed, pv, (g,m,d,s), y )}
	  |"Door" espaces (nomFichierTXT as t) '\n' 
              {let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in ((Objet.Door t ),pos, speed, maxSpeed, pv, (g,m,d,s), y )} 
          |"Projectile\n" 
              {let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in ((Objet.Projectile),pos, speed, maxSpeed, pv, (g,m,d,s), y )}
          |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
          |eof {raise Erreur_de_syntaxe}

and pos perso r = parse
        |"Position" espaces (coupleDigit ) '\n'
              {let (speed,maxSpeed,pv,(g,m,d,s), y) = (vit perso r lexbuf ) in  ((int_of_string x1,int_of_string x2), speed, maxSpeed, pv, (g,m,d,s),  y )}
        |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
        |eof {raise Erreur_de_syntaxe}

and animM l perso r = parse
            |(nomFichierBMP as c) '\n' {animM (c::l) perso r lexbuf  }            
            |("\n" | "no\n")
              {let ((d,s),y) = (animD [] perso r lexbuf  ) in ((l,d,s),  y )}
            |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
            |eof {raise Erreur_de_syntaxe}

and animG l perso r = parse
            |(nomFichierBMP as c) '\n' {(animG (c::l) perso r lexbuf )}
            |("\n" | "no\n")
              {let ((m,d,s), y) = (animM [] perso r lexbuf  ) in ((l,m,d,s),  y )}
            |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
            |eof {raise Erreur_de_syntaxe}

and animD l perso r = parse
            |(nomFichierBMP as c) '\n' {(animD (c::l) perso r lexbuf )}
            |("\n" | "no\n")
              {let (s, y) = (animS [] perso r lexbuf  ) in ((l,s),  y )}
            |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
            |eof {raise Erreur_de_syntaxe}

and animS l perso r = parse
            |(nomFichierBMP as c) '\n' {animS (c::l) perso r lexbuf  } 
            |("\n" | "no\n\n"|"no\n")
              {let y = (entities perso r lexbuf ) in (l,  y )}
            |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
            |eof {let y = (entities perso r lexbuf ) in (l,  y )}

and anim perso r = parse
         |"Animation" espaces '\n' espaces {animG [] perso r lexbuf  }
         |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
         |eof {raise Erreur_de_syntaxe}

and pv perso r = parse
       |"PV" espaces (digit+ as c) '\n' 
{let ((g,m,d,s), y) = (anim perso r lexbuf ) in (c, (g,m,d,s),  y )} 
       |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
       |eof {raise Erreur_de_syntaxe}

and vit perso r = parse
        |"Vitesse" espaces (coupleFloat) '\n' 
              {let (maxSpeed,pv,(g,m,d,s), y) = ((vitMax perso r lexbuf )) in ((float_of_string x1, float_of_string x2), maxSpeed, pv, (g,m,d,s),  y ) }
        |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
        |eof {raise Erreur_de_syntaxe}

and vitMax perso r = parse
           |"VitesseMax" espaces (coupleFloat) '\n'
              {let (pv,(g,m,d,s), y) = (pv perso r lexbuf ) in ((float_of_string x1, float_of_string x2) , int_of_string pv, (Array.of_list g,Array.of_list m,Array.of_list d,Array.of_list s), y ) }
           |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
           |eof {raise Erreur_de_syntaxe}



{
  let lex file pers r = 
    let rec sub3 l acc = 
      match l with 
      |[]->acc
      |x::s when (Objet.getGenre x) = Objet.Background -> sub3 s acc
      |x::s -> sub3 s (x::acc)
    in
    let rec sub1 l = 
      match l with 
      |[] -> print_string "no back";raise ErreurScene 
      |x::s when (Objet.getGenre x) = Background -> x 
      |x::s -> sub1 s
    in
    (*recuperer perso *)
    let rec sub2 l = 
      match l with 
      |[] -> print_string "noPerso";raise ErreurScene 
      |x::s when (Objet.getGenre x) = Personnage -> x 
      |x::s -> sub2 s
    in
      let f = open_in file
      in let (g,list) = sceneText perso r (Lexing.from_channel f) in 
         let () = close_in f in  
         let b = sub1 list in
         let p = sub2 list in
         let l = sub3 list [] in          
         (l,g,b,p)           
}
