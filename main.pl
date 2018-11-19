/* Init Game */
/*:-dynamic(location/1).*/
:-dynamic(map_element/4).
:-dynamic(position/2).
:-dynamic(countMove/1).

health(player,100).
armor(player,0).
weapon(player, barehand).
inInventory(player,i).
position(player, 0, 0).

isWeapon(keris).
isWeapon(kujang).
isWeapon(bambuRuncing).
isWeapon(panah).
isWeapon(sumpit).
isWeapon(barehand). /*Default one */

isArmor(tameng).
isArmor(zirah).
isArmor(helm).
isArmor(jimat).
isArmor(batuAkik).

isMedicine(panadol).
isMedicine(obhCombi).
isMedicine(minyakKayuPutih).
isMedicine(jamu).

isNPC(tentaraBelanda).
isNPC(tentaraJepang).
isNPC(antekPKI).
isNPC(koruptor).

damage(barehand, 5).
damage(keris, 20).
damage(kujang, 20).
damage(bambuRuncing, 30).
damage(panah, 50).
damage(sumpit, 40).

ammo(keris, none).
ammo(kujang, none).
ammo(bambuRuncing, none).
ammo(panah, anakpanah).
ammo(sumpit, anaksumpit).
ammo(barehand, none).

medicine(panadol, 20).
medicine(obhCombi, 30).
medicine(minyakKayuPutih, 40).
medicine(jamu, 100).

armorAmmount(zirah, 20).
armorAmmount(tameng, 15).
armorAmmount(helm, 10).
armorAmmount(jimat, 10).
armorAmmount(batuAkik, 50).



start:-
  game_start,!,
  write('##          ###    ##      ##    ###    ##    ##         ##      ##    ###    ##    ##  ######   ##    ## ##    ## ####'),nl,
  write('##         ## ##   ##  ##  ##   ## ##   ###   ##         ##  ##  ##   ## ##   ###   ## ##    ##  ##   ##   ##  ##  ####'),nl,
  write('##        ##   ##  ##  ##  ##  ##   ##  ####  ##         ##  ##  ##  ##   ##  ####  ## ##        ##  ##     ####   ####'),nl,
  write('##       ##     ## ##  ##  ## ##     ## ## ## ## ####    ##  ##  ## ##     ## ## ## ## ##   #### #####       ##     ## '),nl,
  write('##       ######### ##  ##  ## ######### ##  #### ####    ##  ##  ## ######### ##  #### ##    ##  ##  ##      ##         '),nl,
  write('##       ##     ## ##  ##  ## ##     ## ##   ###  ##     ##  ##  ## ##     ## ##   ### ##    ##  ##   ##     ##    #### '),nl,
  write('######## ##     ##  ###  ###  ##     ## ##    ## ##       ###  ###  ##     ## ##    ##  ######   ##    ##    ##    #### '),nl,nl,
  write('Selamat datang ke Gim yang mungkin menarik ini!                            '),nl,
  write('Wangky adalah seorang pemuda nasionalis yang masih muda,tapi               '),nl,
  write('sudah ndewo kalau urusan melawan penjajah.                                 '),nl,
  write('Dengan keadaan Indonesia yang dijajah oleh tentara luar,                   '),nl,
  write('Wangky dengan berani melawan para penjajah itu!                            '),nl,
  write('Eits, jangan lupa. Ada juga musuh orang Indonesia loh!                     '),nl,
  write('Alias koruptor yang ingin juga merusak citra Indonesia!                    '),nl,nl,
  write('Pilihan ada di tangan Wangky sekarang.                                     '),nl,
  write('Melawan musuh, atau membiarkan Indonesia... kalah.                         '),nl,
  write('Mari bantu Wangky melawan musuh-musuh Indonesia!                           '),nl,nl,
  help,
  write('Mari lawan para musuh Indonesia sekarang!!!                                '),nl,nl,
  repeat,
  write('>'), read(A),
  do(A),nl,
  (A == quit ; gameoverZonaMati).

help :-
  write('Perintah yang dapat Anda jalankan:                                         '),nl,
  write('   start. -- Mulai gim                                                     '),nl,
  write('   help. -- Memunculkan perintah-perintah yang diperlukan                  '),nl,
  write('   quit. -- Keluar dari gim                                                '),nl,
  write('   look. -- Lihat sekeliling Anda!                                         '),nl,
  write('   n. s. e. w. --Bergerak ke utara,selatan,timur,barat!                    '),nl,
  write('   map. -- Melihat peta!                                                   '),nl,
  write('   take(Object). -- Mengambil objek pada petak sekarang.                   '),nl,
  write('   drop(Object), -- Menjatuhkan objek pada petak sekarang.                 '),nl,
  write('   use(Object), -- Memakai objek yang Anda sudah ambil!                    '),nl,
  write('   attack. -- Menyerang musuh!                                             '),nl,
  write('   status. -- Menunjukkan status gim Anda sekarang!                        '),nl,
  write('   save(namafile.txt). -- Menyimpan file gim Anda!                         '),nl,
  write('   load(namafile.txt). -- Memasukkan data dari file ke gim.                '),nl,nl,
  write('Beberapa simbol yang Anda perlu tahu:                                      '),nl,
  write('   S = Senjata                                                             '),nl,
  write('   L = Ammo/Logistik(nama lainnya pelor lah)                               '),nl,
  write('   A = Armor/Perlengkapan Perang                                           '),nl,
  write('   O = Obat                                                                '),nl,
  write('   P = Pemain                                                              '),nl,
  write('   M = Musuh                                                               '),nl,
  write('   - = Petak yang bisa diakses                                             '),nl,
  write('   X = Zona mati. Jangan bergerak ke Zona ini!                             '),nl,nl.


bacaFakta(end_of_file):- !.
bacaFakta(X):- asserta(X),!,fail.

game_start:-
  /*
  open('map.txt',read,S),
  read_file(S,Lines,1,1),
  close(S).

  read_file(S,H,A,B):-
      at_end_of_stream(S),!.

  read_file(S,[H|T],A,B):-
        \+ at_end_of_stream(S),!,
        read(S,H),
        retract(map_element(_,_,A,B)),
        asserta(map_element(H,' - ',A,B)),
        write(H),nl, Bx is B+1,
        read_file(S,T,A,Bx).
  */

      open('mulaigim.txt',read,In),
      (
        repeat,
        read_term(In, X, []),
        bacaFakta(X), !
      ),
      close(In),

      retract(map_element(_,_,2,2)),
      asserta(map_element('P','-',2,2)).


quit :-
  write(' Wangky gagal karena kamu :( ').

map1(N):-
  (map_element(A,_,N,1) -> write(A)),
  (map_element(B,_,N,2) -> write(B)),
  (map_element(C,_,N,3) -> write(C)),
  (map_element(D,_,N,4) -> write(D)),
  (map_element(E,_,N,5) -> write(E)),
  (map_element(F,_,N,6) -> write(F)),
  (map_element(G,_,N,7) -> write(G)),
  (map_element(H,_,N,8) -> write(H)),
  (map_element(I,_,N,9) -> write(I)),
  (map_element(J,_,N,10) -> write(J)),
  (map_element(K,_,N,11) -> write(K)),
  (map_element(L,_,N,12) -> write(L)),nl.

call_map(N) :- N == 13, !.
call_map(N) :- map1(N), N1 is N+1, call_map(N1).

map :- call_map(1).

moveFromTo(A1,B1,A2,B2) :- map_element('X',_,A2,B2), retract(position(A1,B1)),asserta(position(A2,B2)),
     retract(countMove(C)),Cx is C+1, asserta(countMove(Cx)).

moveFromTo(A1,B1,A2,B2) :- retract(position(A1,B1)),retract(map_element(_,_,A1,B1)), asserta(map_element('-','-',A1,B1)),
       asserta(position(A2,B2)),
       retract(map_element(_,_,A2,B2)),asserta(map_element('P','-',A2,B2)),
       retract(countMove(C)),Cx is C+1, asserta(countMove(Cx)), tambahDeadZone.


s :- position(A,B), Ax is (A+1), moveFromTo(A,B,Ax,B).
n :- position(A,B), Ax is (A-1), moveFromTo(A,B,Ax,B).
e :- position(A,B), Bx is (B+1), moveFromTo(A,B,A,Bx).
w :- position(A,B), Bx is (B-1), moveFromTo(A,B,A,Bx).

look_pos(X,Y) :- map_element(A,B,X,Y), A == 'X' -> write(A).
look_pos(X,Y) :- map_element(A,B,X,Y) -> write(B).

look_rek(A,B,C) :- C == 10, !.
look_rek(A,B,C) :-  0 is mod(C,3), !, look_pos(A,B), nl, 
                    A1 is A+1, B1 is B-2, C1 is C+1, look_rek(A1,B1,C1).
look_rek(A,B,C) :- look_pos(A,B), B1 is B+1, C1 is C+1, look_rek(A,B1,C1).

look :- position(A,B),
        A1 is A-1,
        B1 is B-1,

        look_rek(A1,B1,1).

updatemapbaris(N) :-
    retract(map_element(_,_,N,1)), asserta(map_element('X','-',N,1)),
    retract(map_element(_,_,N,2)), asserta(map_element('X','-',N,2)),
    retract(map_element(_,_,N,3)), asserta(map_element('X','-',N,3)),
    retract(map_element(_,_,N,4)), asserta(map_element('X','-',N,4)),
    retract(map_element(_,_,N,5)), asserta(map_element('X','-',N,5)),
    retract(map_element(_,_,N,6)), asserta(map_element('X','-',N,6)),
    retract(map_element(_,_,N,7)), asserta(map_element('X','-',N,7)),
    retract(map_element(_,_,N,8)), asserta(map_element('X','-',N,8)),
    retract(map_element(_,_,N,9)), asserta(map_element('X','-',N,9)),
    retract(map_element(_,_,N,10)), asserta(map_element('X','-',N,10)),
    retract(map_element(_,_,N,11)), asserta(map_element('X','-',N,11)),
    retract(map_element(_,_,N,12)), asserta(map_element('X','-',N,12)).

updatemapkolom(N) :-
  retract(map_element(_,_,1,N)), asserta(map_element('X','-',1,N)),
  retract(map_element(_,_,2,N)), asserta(map_element('X','-',2,N)),
  retract(map_element(_,_,3,N)), asserta(map_element('X','-',3,N)),
  retract(map_element(_,_,4,N)), asserta(map_element('X','-',4,N)),
  retract(map_element(_,_,5,N)), asserta(map_element('X','-',5,N)),
  retract(map_element(_,_,6,N)), asserta(map_element('X','-',6,N)),
  retract(map_element(_,_,7,N)), asserta(map_element('X','-',7,N)),
  retract(map_element(_,_,8,N)), asserta(map_element('X','-',8,N)),
  retract(map_element(_,_,9,N)), asserta(map_element('X','-',9,N)),
  retract(map_element(_,_,10,N)), asserta(map_element('X','-',10,N)),
  retract(map_element(_,_,11,N)), asserta(map_element('X','-',11,N)),
  retract(map_element(_,_,12,N)), asserta(map_element('X','-',12,N)).

tambahDeadZone :-
    countMove(A), A == 3,!,updatemapbaris(2).
tambahDeadZone :-
    countMove(A), A == 6,!,updatemapbaris(3).
tambahDeadZone :-
    countMove(A), A == 9,!,updatemapkolom(2).
tambahDeadZone :-
    countMove(A), A == 12,!,updatemapbaris(4),updatemapkolom(3).
tambahDeadZone :-
    countMove(A), A == 15,!,updatemapbaris(5),updatemapkolom(4).
tambahDeadZone :-
    countMove(A), A == 19,!,updatemapbaris(6),updatemapbaris(7),updatemapkolom(5).
tambahDeadZone :-
    countMove(A), A == 23,!,updatemapbaris(8),updatemapkolom(6),updatemapkolom(7),updatemapkolom(8).
tambahDeadZone :-
    countMove(A), A == 27,!,updatemapbaris(9),updatemapkolom(9),updatemapkolom(10),updatemapbaris(10).
tambahDeadZone.

do(help):- help,!.
do(map):-map,!.
do(s) :- s,!.
do(n) :- n,!.
do(e) :- e,!.
do(w) :- w,!.
do(quit) :-quit,!.
do(gameover) :-gameover,!.
do(look) :- look,!.
do(tambahDeadZone) :- tambahDeadZone,!.
do(countMove(A)) :- countMove(A),!.
do(updatemapbaris(A)) :- updatemapbaris(A),!.
do(updatemapkolom(A)) :- updatemapkolom(A),!.
do(_) :- write('Perintah tidak valid!'),nl.

gameoverZonaMati :-
    position(A,B),
    map_element(C,_,A,B), C == 'X',
    write('KAMU KALAH! Lain Kali Jangan Mengenai Zona Mati ya!'),nl.

menang :-
  write('                               ___________________________________________'),nl,
  write('                                        Persembahan kami, untuk Anda.     '),nl,
  write('                               ___________________________________________'),nl.
