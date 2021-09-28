(* EXERCICE 4 *)

let trycode a =
  if String.length (string_of_int a) = 4 then
    if a > 0 then
  if a /   1000 = (a mod 10)*2 then
    if (a/100)mod 10 = (a/10)mod 10 * 3 then
      if ((a/1000)+(a mod 10)+((1234/100)mod 10)+((a/10)mod 10))=13 then
           true
          else
            failwith "Il faut que la somme de ces 4 chiffres soit egal a 13"
          else
            failwith "Le chiffre des 100aines 3* celui des 10aines"
          else
            failwith "Le chiffre des 1000e 2* celui des unites"
    else
      failwith "Le nombre doit etre positif"
  else
    failwith "Il faut 4 chiffres entiers";;

a = (n/1000) and
b = ((n/100)mod 10) and
c = ((n/10)mod 10) and
d = n mod 10 in
if (a)=2*(d) then
      if (b)=3*(c) then
        if (a+b+c+d)=13 then ;;

(* EXERCICE 5 *)

let plate a =
  if String.length (string_of_int a) = 4 then
    if a > 0 then
      if a/1000 = (a/100)mod 10 then
        if (a/10)mod 10 = a mod 10 then
          if (sqrt(float_of_int a))**2.=float_of_int a then
                                         true
             else
               false
             else
               false
             else
               false
             else
               failwith "plate : parameter is not a 4-digit natural"
             else
               failwith "plate: parameter is not a 4-digit natural";;

(* EXERCICE 6 *)

let surface a =
  let b = a - 5000 in
  let c = (b*100)+b in
  let d = (c/7)-(7*(c mod 7)) in
  let e = (d/11)-(11*(d mod 11)) in
  let f = (e/13)-(13*(e mod 13)) in
    if a>0 then
                 if f = 555 then
                   true
                 else
                   false
    else failwith "surface:negative parameter, cannot be a surface area";;

(* EXERCICE 7 *)

let tomorrow a =
  let jour = String.sub a 0 2 in
  let mois = String.sub a 3 2 in
  let an = String.sub a 6 4 in
  match mois with
  |1|3|5|7|8|10|12 -> match jour with
                      |31 -> jour-30
                      |_ -> jour+1
   |4|6|9|11 -> match jour with
                 |30 -> jour-29
                 |_ -> jour+1
   |2 -> match an with
         |((int_of_string a) mod 4) = 0 -> match jour with
                           |29 -> jour-28
                           |_ -> jour+1
       |_ -> match jour with
               |28 -> jour-27
               |_ -> jour+1
         match mois with
         |12 -> an+1
         |_ -> failwith "ERREUR INTERNE";;
