{
  open Objet
  open Camera
  open Anim
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
    |"(*" (mot | espaces)+ "*)" {sceneText perso r lexbuf }
    |"Scene" espaces digit espaces "\n\n" {sceneGrav perso r lexbuf }
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
       
and sceneGrav perso r = parse
    |"Gravity" espaces (float as f) "\n\n"{
      let x = (background r lexbuf) in
      let y = (entities perso r lexbuf ) in
      (float_of_string f, x ,y )
    }
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
	
and background r = parse
    |"Background" espaces (nomFichierBMP as c) "\n\n"{Objet.create Background (0,0) (0.0,0.0) (0.0,0.0) 1000 (Anim.create [||] [|c|] [||] [||] r) r}
    |_ as c {Printf.printf "ErreurAA : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}	
	
and entities perso r = parse
    |"Genre" espaces{
      let (genre,pos,speed,maxSpeed,pv,t, y) = (genre perso r lexbuf ) in 
      List.cons (Objet.create genre pos speed maxSpeed pv t r) y
    }
    |'\n' {entities perso r lexbuf }
    |_ as c {Printf.printf "ErreurBB : %c" c;raise Erreur_de_syntaxe}
    |eof {[]}
	
and genre perso r = parse
    |"Personnage" espaces '\n' {
      match perso with
      |None -> let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in ((Objet.Personnage),pos, speed , maxSpeed , pv , (Anim.create g m d s r), y)
      |Some p -> let (pos,y) = (pos2 perso r lexbuf ) in ((Objet.Personnage),pos, (Objet.getSpeed p), (Objet.getMaxSpeed p), (Objet.getPV p), (Objet.getAnim p), y)
    }
    |"Plateforme" espaces '\n' {
      let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in
      ((Objet.Plateforme),pos, speed, maxSpeed, pv, (Anim.create g m d s r), y)
    } 
    |"Ennemi\n" {
      let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in
      ((Objet.Ennemi),pos, speed, maxSpeed, pv, (Anim.create g m d s r), y)
    }
    |"Wall\n" {
      let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in
      ((Objet.Wall),pos, speed, maxSpeed, pv, (Anim.create g m d s r), y)
    }
    |"Door" espaces (nomFichierTXT as t) '\n' {
      let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in
      ((Objet.Door t ),pos, speed, maxSpeed, pv, (Anim.create g m d s r), y
      )} 
    |"Projectile\n" {
      let (pos,speed,maxSpeed,pv,(g,m,d,s), y) = (pos perso r lexbuf ) in
      ((Objet.Projectile),pos, speed, maxSpeed, pv, (Anim.create g m d s r), y)
    }
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
	
and pos2 perso r = parse
    |"Position" espaces (coupleDigit ) '\n' {
      let y = (entities perso r lexbuf ) in
      ((int_of_string x1,int_of_string x2),y)
    }
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
	
and pos perso r = parse
    |"Position" espaces (coupleDigit ) '\n' {
      let (speed,maxSpeed,pv,(g,m,d,s), y) = (vit perso r lexbuf ) in
      ((int_of_string x1,int_of_string x2), speed, maxSpeed, pv, (g,m,d,s), y)
    }
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
	
and animM l perso r = parse
    |(nomFichierBMP as c) '\n' {animM (c::l) perso r lexbuf } 
    |("\n" | "no\n") {let ((d,s),y) = (animD [] perso r lexbuf ) in ((l,d,s), y )}
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
	
and animG l perso r = parse
    |(nomFichierBMP as c) '\n' {(animG (c::l) perso r lexbuf )}
    |("\n" | "no\n") {let ((m,d,s), y) = (animM [] perso r lexbuf ) in ((l,m,d,s), y )}
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
	
and animD l perso r = parse
    |(nomFichierBMP as c) '\n' {(animD (c::l) perso r lexbuf )}
    |("\n" | "no\n") {let (s, y) = (animS [] perso r lexbuf ) in ((l,s), y )}
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
     
and animS l perso r = parse
    |(nomFichierBMP as c) '\n' {animS (c::l) perso r lexbuf } 
    |("\n" | "no\n\n"|"no\n")
	{let y = (entities perso r lexbuf ) in (l, y )}
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {let y = (entities perso r lexbuf ) in (l, y )}
	
and anim perso r = parse
    |"Animation" espaces '\n' espaces {animG [] perso r lexbuf }
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
	
and pv perso r = parse
    |"PV" espaces (digit+ as c) '\n' {let ((g,m,d,s), y) = (anim perso r lexbuf ) in (c, (g,m,d,s), y )} 
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
	
and vit perso r = parse
    |"Vitesse" espaces (coupleFloat) '\n' {
      let (maxSpeed,pv,(g,m,d,s), y) = ((vitMax perso r lexbuf )) in
      ((float_of_string x1, float_of_string x2), maxSpeed, pv, (g,m,d,s), y )
    }
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
	
and vitMax perso r = parse
    |"VitesseMax" espaces (coupleFloat) '\n' {
      let (pv,(g,m,d,s), y) = (pv perso r lexbuf ) in ((float_of_string x1, float_of_string x2) , int_of_string pv, (Array.of_list g,Array.of_list m,Array.of_list d,Array.of_list s), y )
    }
    |_ as c {Printf.printf "Erreur : %c" c;raise Erreur_de_syntaxe}
    |eof {raise Erreur_de_syntaxe}
	
{
let lex file pers r = 
    let rec sub2 l = 
      match l with 
          |[] -> print_string "noPerso";raise ErreurScene 
          |x::s when (Objet.getGenre x) = Personnage -> x 
	  |x::s -> sub2 s
    in
    let f = open_in file
    in let (grav,back,list) = sceneText pers r (Lexing.from_channel f) in 
       let () = close_in f in 
       let p = sub2 list in 
       (list,grav,back,p) 
}
	
