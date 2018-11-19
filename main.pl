/* Init Game */
/*:-dynamic(location/1).*/
:-dynamic(map_element/4).
:-dynamic(position/2).
:-dynamic(countMove/1).
:-dynamic(positionNPC/3).
:-dynamic(armor/2).
:-dynamic(health/2).
:-dynamic(weapon/2).
:-dynamic(inInventory/2).

health(player,100).
armor(player,0).
weapon(player, barehand).

isWeapon(keris).
isWeapon(kujang).
isWeapon(bambuRuncing).
isWeapon(senapan).
isWeapon(sumpit).
isWeapon(duit).
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

positionNPC(tentaraBelanda, 0, 0).
positionNPC(tentaraJepang, 0, 0).
positionNPC(antekPKI, 0, 0).
positionNPC(koruptor, 0, 0).

inInventory(player, []).
inInventory(tentaraBelanda, [senapan]).
inInventory(tentaraJepang, [sumpit]).
inInventory(antekPKI, [kujang]).
inInventory(koruptor, [duit]).

damage(barehand, 5).
damage(keris, 20).
damage(kujang, 20).
damage(bambuRuncing, 30).
damage(senapan, 50).
damage(sumpit, 40).
damage(duit, 30).

isAmmo(peluru).
isAmmo(anaksumpit).

weaponAmmo(keris, none).
weaponAmmo(kujang, none).
weaponAmmo(bambuRuncing, none).
weaponAmmo(senapan, peluru).
weaponAmmo(sumpit, anaksumpit).
weaponAmmo(barehand, none).
weaponAmmo(duit, none).

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
  game_retractstart,!,
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
retractFakta(end_of_file):- !.
retractFakta(X):- retract(X),!,fail.

game_retractstart :-
  open('retractmulaigim.txt',read,In),
  (
    repeat,
    read_term(In, X, []),
    retractFakta(X), !
  ),
  close(In).

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
    asserta(map_element('P',['P'],2,2)),

    initEnemy.


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

initEnemy :-
    forall(isNPC(A), initEnemy(A)).

initEnemy(A) :-
    positionNPC(A, X, Y),
    retract(positionNPC(A, X, Y)),
    random(2, 11, NewX),
    random(2, 11, NewY),
    asserta(positionNPC(A, NewX, NewY)),
    map_element(S, L, NewX, NewY),
    retract(map_element(S, L, NewX, NewY)),
    append(L, [A], NewList),
    asserta(map_element(S, NewList, NewX, NewY)).

moveEnemy(A) :-
    positionNPC(A, X, Y),
    write(A), nl,
    retract(positionNPC(A, X, Y)),
    write(X), write('spasi'), write(Y), nl,
    map_element(S, L, X, Y),
    write(L), nl,
    retract(map_element(S, L, X, Y)),
    delete(L, A, NL),
    write(NL), nl,
    asserta(map_element(S, NL, X, Y)),
    random(-1, 1, NewDX),
    random(-1, 1, NewDY),
    NewX is (X + NewDX),
    NewY is (Y + NewDY),
    write(NewX), write('spasi'), write(NewY), nl,
    normalizePosition(NewX, NewY, NNewX, NNewY),
    write(NNewX), write('spasi'), write(NNewY), nl,
    asserta(positionNPC(A, NNewX, NNewY)),
    append(NL, [A], NewList),
    write(NewList), nl,
    asserta(map_element(S, NewList, X, Y)).

normalizePosition(X, Y, XN, YN) :-
    X < 1,
    XN is 1,
    normalizePosition(XN, Y, XN, YN).

normalizePosition(X, Y, XN, YN) :-
    Y < 1,
    YN is 1,
    normalizePosition(X, YN, XN, YN).

normalizePosition(X, Y, XN, YN) :-
    X > 12,
    XN is 12,
    normalizePosition(XN, Y, XN, YN).

normalizePosition(X, Y, XN, YN) :-
    Y > 12,
    YN is 12,
    normalizePosition(X, YN, XN, YN).

normalizePosition(X, Y, XN, YN) :-
    XN is X, YN is Y.

moveAllEnemy :-
    forall(isNPC(A), moveEnemy(A)).

moveFromTo(A1,B1,A2,B2) :-
    map_element('X',_,A2,B2),
    retract(position(A1,B1)),
    asserta(position(A2,B2)),
    retract(countMove(C)),
    Cx is C+1,
    asserta(countMove(Cx)),!.

moveFromTo(A1,B1,A2,B2) :-
    retract(position(A1,B1)),
    map_element(S, L, A1, B1),
    delete(L,'P',LX),
    retract(map_element(S,L,A1,B1)),
    asserta(map_element('-',LX,A1,B1)),
    asserta(position(A2,B2)),
    map_element(S1,L1,A2,B2),
    retract(map_element(S1,L1,A2,B2)),
    append(L1,['P'],L1X),
    asserta(map_element('P',L1X,A2,B2)),
    retract(countMove(C)),
    Cx is C+1,
    asserta(countMove(Cx)),
    tambahDeadZone.

s :- position(A,B), Ax is (A+1), moveFromTo(A,B,Ax,B).
n :- position(A,B), Ax is (A-1), moveFromTo(A,B,Ax,B).
e :- position(A,B), Bx is (B+1), moveFromTo(A,B,A,Bx).
w :- position(A,B), Bx is (B-1), moveFromTo(A,B,A,Bx).

look_pos(X,Y) :- map_element(A,_,X,Y), A == 'X', !, write(A).
look_pos(X,Y) :- map_element(_,_B,X,Y), _B == [], !, write('-').
look_pos(X,Y) :- map_element(_,_B,X,Y), member('E',[_B]), !, write('E').
look_pos(X,Y) :- map_element(_,_B,X,Y), member('M',[_B]), !, write('M').
look_pos(X,Y) :- map_element(_,_B,X,Y), member('W',[_B]), !, write('W').
look_pos(X,Y) :- map_element(_,_B,X,Y), member('A',[_B]), !, write('A').
look_pos(X,Y) :- map_element(_,_B,X,Y), member('O',[_B]), !, write('O').
look_pos(X,Y) :- map_element(_,_B,X,Y), member('P',[_B]), !, write('P').
look_pos(X,Y) :- map_element(_,_B,X,Y), write('P').


look_rek(_,_,C) :- C == 10, !.
look_rek(A,B,C) :-  0 is mod(C,3), !, look_pos(A,B), nl,
                    A1 is A+1, B1 is B-2, C1 is C+1, look_rek(A1,B1,C1).
look_rek(A,B,C) :- look_pos(A,B), B1 is B+1, C1 is C+1, look_rek(A,B1,C1).

look :- position(A,B),
        A1 is A-1,
        B1 is B-1,
        look_rek(A1,B1,1).

status :-
    inInventory(player,LI),
    health(player,H),
    armor(player,A),
    weapon(player,W),
    write('Health : '), write(H), nl,
    write('Armor  : '), write(A), nl,
    write('Weapon : '), write(W), nl,
    write('Isi Inventori : '),nl, tulisInventory(LI).

tulisInventory([])     :- write(' -Kosong-'), nl, !.
tulisInventory([H|[]]) :- write(' -'),write(H), nl,!.
tulisInventory([H|T])  :- write(' -'),write(H),nl, tulisInventory(T).

use(Item) :-
  inInventory(player,LI),
  member(Item, LI),
  isArmor(Item),
  armor(player,X),
  armorAmmount(Item,ArmorAdded),
  delete(LI,Item,NewLI),
  NewArmor is X + ArmorAdded,
  retract(armor(player,X)),
  asserta(armor(player,NewArmor)),
  retract(inInventory(player,LI)),
  asserta(inInventory(player,NewLI)),
  write('Armor anda bertambah sebesar '), write(ArmorAdded), nl.

use(Item) :-
  inInventory(player,LI),
  member(Item, LI),
  isMedicine(Item),
  health(player,HPAwal),
  medicine(Item,HealthAdded),
  delete(LI,Item,NewLI),
  NewHealth is (HPAwal + HealthAdded),
  NewHealth > 100, !,
  retract(health(player,HPAwal)),
  asserta(health(player,100)),
  retract(inInventory(player,LI)),
  asserta(inInventory(player,NewLI)),
  write('Health anda bertambah sebesar '), write(HealthAdded), nl.

use(Item) :-
  inInventory(player,LI),
  member(Item, LI),
  isMedicine(Item),
  health(player,X),
  medicine(Item,HealthAdded),
  delete(LI,Item,NewLI),
  NewHealth is X + HealthAdded,
  retract(health(player,X)),
  asserta(health(player,NewHealth)),
  retract(inInventory(player,LI)),
  asserta(inInventory(player,NewLI)),
  write('Health anda bertambah sebesar '), write(HealthAdded), nl.

use(Item) :-
  inInventory(player,LI),
  member(Item, LI),
  isWeapon(Item),
  weapon(player,Old),
  delete(LI,Item,NewLI),
  Old == barehand,!,
  retract(weapon(player,Old)),
  asserta(weapon(player,Item)),
  retract(inInventory(player,LI)),
  asserta(inInventory(player,NewLI)),
  write('Anda kini menggunakan '), write(Item), write(' sebagai senjata anda'), nl.

use(Item) :-
  inInventory(player,LI),
  member(Item, LI),
  isWeapon(Item),
  weapon(player,Old),
  delete(LI,Item,TempLI),
  append(TempLI,[Old],NewLI),
  retract(weapon(player,Old)),
  asserta(weapon(player,Item)),
  retract(inInventory(player,LI)),
  asserta(inInventory(player,NewLI)),
  write('Anda kini menggunakan '), write(Item), write(' sebagai senjata anda'), nl.

use(Item) :-
  inInventory(player,LI),
  member(Item, LI),
  isAmmo(Item).

use(Item) :-
  inInventory(player,LI),
  \+member(Item, LI),
  write('Tidak bisa menggunakan barang tersebut'),nl, write('karena barang tersebut tidak ada dalam inventori'), nl.

drop(Item) :-
  inInventory(player,LI),
  \+member(Item, LI),!,
  write('Tidak bisa menjatuhkan barang tersebut'),nl, write('karena barang tersebut tidak ada dalam inventori'), nl.

drop(Item) :-
  inInventory(player,LI),
  member(Item,LI),isWeapon(Item),!,write('Anda menjatuhkan senjata '),write(LI),nl,
  retract(inInventory(player,LI)),
  delete(LI,Item,NewLI),
  asserta(inInventory(player,NewLI)),
  position(A,B),
  retract(map_element(_,LPetak,A,B)),
  append(LPetak,['W'],NewLPetak),
  assert(map_element(_,NewLPetak,A,B)).

drop(Item) :-
  inInventory(player,LI),
  member(Item,LI),isMedicine(Item),!,write('Anda menjatuhkan obat '),write(LI),nl,
  retract(inInventory(player,LI)),
  delete(LI,Item,NewLI),
  asserta(inInventory(player,NewLI)),
  position(A,B),
  retract(map_element(_,LPetak,A,B)),
  append(LPetak,['M'],NewLPetak),
  assert(map_element(_,NewLPetak,A,B)).

drop(Item) :-
  inInventory(player,LI),
  member(Item,LI),isArmor(Item),!,write('Anda menjatuhkan pelindung '),write(LI),nl,
  retract(inInventory(player,LI)),
  delete(LI,Item,NewLI),
  asserta(inInventory(player,NewLI)),
  position(A,B),
  retract(map_element(_,LPetak,A,B)),
  append(LPetak,['A'],NewLPetak),
  assert(map_element(_,NewLPetak,A,B)).

drop(Item) :-
  inInventory(player,LI),
  member(Item,LI),isAmmo(Item),!,write('Anda menjatuhkan pelor '),write(LI),nl,
  retract(inInventory(player,LI)),
  delete(LI,Item,NewLI),
  asserta(inInventory(player,NewLI)),
  position(A,B),
  retract(map_element(_,LPetak,A,B)),
  append(LPetak,['A'],NewLPetak),
  assert(map_element(_,NewLPetak,A,B)).


updatemapbaris(N) :-
    retract(map_element(_,X1,N,1)), asserta(map_element('X',X1,N,1)),
    retract(map_element(_,X2,N,2)), asserta(map_element('X',X2,N,2)),
    retract(map_element(_,X3,N,3)), asserta(map_element('X',X3,N,3)),
    retract(map_element(_,X4,N,4)), asserta(map_element('X',X4,N,4)),
    retract(map_element(_,X5,N,5)), asserta(map_element('X',X5,N,5)),
    retract(map_element(_,X6,N,6)), asserta(map_element('X',X6,N,6)),
    retract(map_element(_,X7,N,7)), asserta(map_element('X',X7,N,7)),
    retract(map_element(_,X8,N,8)), asserta(map_element('X',X8,N,8)),
    retract(map_element(_,X9,N,9)), asserta(map_element('X',X9,N,9)),
    retract(map_element(_,X10,N,10)), asserta(map_element('X',X10,N,10)),
    retract(map_element(_,X11,N,11)), asserta(map_element('X',X11,N,11)),
    retract(map_element(_,X12,N,12)), asserta(map_element('X',X12,N,12)).

updatemapkolom(N) :-
  retract(map_element(_,X1,1,N)), asserta(map_element('X',X1,1,N)),
  retract(map_element(_,X2,2,N)), asserta(map_element('X',X2,2,N)),
  retract(map_element(_,X3,3,N)), asserta(map_element('X',X3,3,N)),
  retract(map_element(_,X4,4,N)), asserta(map_element('X',X4,4,N)),
  retract(map_element(_,X5,5,N)), asserta(map_element('X',X5,5,N)),
  retract(map_element(_,X6,6,N)), asserta(map_element('X',X6,6,N)),
  retract(map_element(_,X7,7,N)), asserta(map_element('X',X7,7,N)),
  retract(map_element(_,X8,8,N)), asserta(map_element('X',X8,8,N)),
  retract(map_element(_,X9,9,N)), asserta(map_element('X',X9,9,N)),
  retract(map_element(_,X10,10,N)), asserta(map_element('X',X10,10,N)),
  retract(map_element(_,X11,11,N)), asserta(map_element('X',X11,11,N)),
  retract(map_element(_,X12,12,N)), asserta(map_element('X',X12,12,N)).

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
do(s) :- s, moveAllEnemy,!.
do(n) :- n, moveAllEnemy,!.
do(e) :- e, moveAllEnemy,!.
do(w) :- w, moveAllEnemy,!.
do(quit) :-quit,!.
do(gameover) :-gameover,!.
do(look) :- look,!.
do(drop) :- drop,!.
do(tambahDeadZone) :- tambahDeadZone,!.
do(countMove(A)) :- countMove(A),!.
do(updatemapbaris(A)) :- updatemapbaris(A),!.
do(updatemapkolom(A)) :- updatemapkolom(A),!.
do(status) :- status,!.
do(use(Item)) :- use(Item),!.
do(_) :- write('Perintah tidak valid!'),nl.

gameoverZonaMati :-
    position(A,B),
    map_element(C,_,A,B), C == 'X',
    write('KAMU KALAH! Lain Kali Jangan Mengenai Zona Mati ya!'),nl.

menang :-
  write('                               ___________________________________________'),nl,
  write('                                        Persembahan kami, untuk Anda.     '),nl,
  write('                               ___________________________________________'),nl.
