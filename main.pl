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

armor(player,0).
weapon(player, none).

isWeapon(keris).
isWeapon(kujang).
isWeapon(bambuRuncing).
isWeapon(senapan).
isWeapon(sumpit).
isWeapon(duit).

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

health(player,100).
health(tentaraBelanda, 100).
health(tentaraJepang, 100).
health(antekPKI, 100).
health(koruptor, 100).

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
  (A == quit ; gameoverZonaMati; checkPlayerLife).

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
    write('initposition'), write(NewX), write(NewY),nl,
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

    moveEnemyHelper(X, Y, NNewX, NNewY),
    print(NNewX), print('spasi'), print(NNewY), nl,
    map_element(NextS, NextL, NNewX, NNewY),
    write(NNewX,NNewY),nl,
    retract(map_element(NextS, NextL, NNewX, NNewY)),
    asserta(positionNPC(A, NNewX, NNewY)),
    append(NextL, [A], NewList),
    write(NewList), nl,
    asserta(map_element(NextS, NewList, NNewX, NNewY)).

moveEnemyHelper(X, Y, NewX, NewY) :-
    X = 1, Y = 1,
    isEnemyHere(X, Y + 1),
    isEnemyHere(X + 1, Y),
    isEnemyHere(X + 1, Y + 1),
    NewX is X, NewY is Y.

moveEnemyHelper(X, Y, NewX, NewY) :-
    X = 1, Y = 12,
    isEnemyHere(X, Y - 1),
    isEnemyHere(X + 1, Y),
    isEnemyHere(X + 1, Y - 1),
    NewX is X, NewY is Y.

moveEnemyHelper(X, Y, NewX, NewY) :-
    X = 12, Y = 1,
    isEnemyHere(X, Y + 1),
    isEnemyHere(X - 1, Y),
    isEnemyHere(X - 1, Y + 1),
    NewX is X, NewY is Y.

moveEnemyHelper(X, Y, NewX, NewY) :-
    X = 12, Y = 12,
    isEnemyHere(X, Y - 1),
    isEnemyHere(X - 1, Y),
    isEnemyHere(X - 1, Y - 1),
    NewX is X, NewY is Y.

moveEnemyHelper(X, Y, NewX, NewY) :-
    random(-1, 1, NewDX),
    random(-1, 1, NewDY),
    NX is X + NewDX, NY is Y + NewDY,
    normalizePosition(NX, NY, NNewX, NNewY),
    \+isEnemyHere(NNewX, NNewY),
    NewX is NNewX, NewY is NNewY.

moveEnemyHelper(X, Y, NewX, NewY) :-
    random(-1, 1, NewDX),
    random(-1, 1, NewDY),
    NX is X + NewDX, NY is Y + NewDY,
    normalizePosition(NX, NY, NNewX, NNewY),
    isEnemyHere(NNewX, NNewY),
    moveEnemyHelper(X, Y, NewX, NewY).

isEnemyHere(X, Y) :-
    map_element(_, L, X, Y),
    member(tentaraBelanda, L).

isEnemyHere(X, Y) :-
    map_element(_, L, X, Y),
    member(tentaraJepang, L).

isEnemyHere(X, Y) :-
    map_element(_, L, X, Y),
    member(antekPKI, L).

isEnemyHere(X, Y) :-
    map_element(_, L, X, Y),
    member(koruptor, L).

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
look_pos(X,Y) :- map_element(_,_B,X,Y), member('E',_B), !, write('E').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(tentaraBelanda,_B), !, write('M').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(tentaraJepang,_B), !, write('M').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(antekPKI,_B), !, write('M').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(koruptor,_B), !, write('M').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(keris,_B), !, write('W').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(kujang,_B), !, write('W').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(bambuRuncing,_B), !, write('W').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(senapan,_B), !, write('W').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(sumpit,_B), !, write('W').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(duit,_B), !, write('W').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(tameng,_B), !, write('A').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(zirah,_B), !, write('A').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(helm,_B), !, write('A').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(jimat,_B), !, write('A').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(batuAkik,_B), !, write('A').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(panadol,_B), !, write('O').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(obhCombi,_B), !, write('O').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(minyakKayuPutih,_B), !, write('O').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(jamu,_B), !, write('O').
look_pos(X,Y) :- map_element(_,_B,X,Y), member('P',_B), !, write('P').
look_pos(X,Y) :- map_element(_,_B,X,Y), write('P').

look_rek(_,_,C) :- C == 10, !.
look_rek(A,B,C) :-  0 is mod(C,3), !, look_pos(A,B), nl,
                    A1 is A+1, B1 is B-2, C1 is C+1, look_rek(A1,B1,C1).
look_rek(A,B,C) :- look_pos(A,B), B1 is B+1, C1 is C+1, look_rek(A,B1,C1).

look_desc(X,Y) :- map_element(_,_B,X,Y), _B == ['P'], !, write('Kamu sedang berada di lapangan terbuka, tidak ada apa-apa disini'),nl.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(tentaraBelanda,_B), write('Terdapat tentara belanda disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(tentaraJepang,_B), write('Terdapat tentara jepang disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(antekPKI,_B), write('Terdapat antek PKI disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(koruptor,_B), write('Terdapat koruptor disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(keris,_B), write('Terdapat keris disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(kujang,_B), write('Terdapat kujang disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(bambuRuncing,_B), write('Terdapat bambu runcing disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(senapan,_B), write('Terdapat senapan disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(sumpit,_B), write('Terdapat sumpit disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(duit,_B), write('Terdapat duit disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(tameng,_B), write('Terdapat tameng disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(zirah,_B), write('Terdapat zirah disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(helm,_B), write('Terdapat helm disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(jimat,_B), write('Terdapat jimat disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(batuAkik,_B), write('Terdapat batu akik disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(panadol,_B), write('Terdapat panadol disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(obhCombi,_B), write('Terdapat OBH Combi disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(minyakKayuPutih,_B), write('Terdapat minyak kayu putih disini. ').
look_desc(X,Y) :- map_element(_,_B,X,Y), member(jamu,_B), write('Terdapat jamu disini. ').
look_desc(X,Y) :- nl.

look :- position(A,B),
        A1 is A-1,
        B1 is B-1,
        look_rek(A1,B1,1),
        look_desc(A,B).
        

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

take(Item) :-
    position(X, Y),
    write(X), write(' '), write(Y), nl,
    map_element(S, L, X, Y),
    member(Item, L), !,
    write(Item), write(' '), write(L), nl,
    inInventory(player, LI),
    write(LI), nl,
    retract(inInventory(player, LI)),
    write(LI), nl,
    append(LI, [Item], NLI),
    write(NLI), nl,
    asserta(inInventory(player, NLI)),
    retract(map_element(S, L, X, Y)),
    delete(L, Item, LNEW),
    asserta(map_element(S, LNEW, X, Y)),
    write(Item), write(' sudah diambil! Hebat Kamu!'), nl.

take(Item) :-
    position(X, Y),
    write(X), write(' '), write(Y), nl,
    map_element(_, L, X, Y),
    write(Item), write(' '), write(L), nl,
    \+member(Item, L), !,
    write('Tidak ada '), write(Item), write(' di sini! Miskin!'), nl.

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
  Old == none,!,
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

checkEnemy :-
    position(X, Y),
    map_element(_, L, X, Y),
    member(tentaraBelanda, L).

checkEnemy :-
    position(X, Y),
    map_element(_, L, X, Y),
    member(tentaraJepang, L).

checkEnemy :-
    position(X, Y),
    map_element(_, L, X, Y),
    member(antekPKI, L).

checkEnemy :-
    position(X, Y),
    map_element(_, L, X, Y),
    member(koruptor, L).

attack :-
    position(X, Y),
    map_element(S, L, X, Y),
    checkEnemy,
    attackHelper1,
    write('ATTACK SEQUENCE 1'), nl.

attack :-
    position(X, Y),
    map_element(S, L, X, Y),
    \+checkEnemy,
    write('Tidak ada musuh disini!'), nl.

attackHelper1 :-
    weapon(player, WEAPON_PLAYER),
    \+WEAPON_PLAYER = none,
    attackHelper2(WEAPON_PLAYER).

attackHelper1 :-
    weapon(player, WEAPON_PLAYER),
    WEAPON_PLAYER = none,
    write('Kamu tidak memegang senjata! Cari mati kamu?'), nl.

attackHelper2(WEAPON_PLAYER) :-
    weaponAmmo(WEAPON_PLAYER, AMMO_TYPE),
    AMMO_TYPE = none,
    position(X, Y),
    positionNPC(NPC, X, Y),
    write('Kamu menyerang '), write(NPC), write('! Kasihan dia!'), nl,
    health(NPC, CURRENT_HEALTH),
    retract(health(NPC, CURRENT_HEALTH)),
    damage(WEAPON_PLAYER, DAMAGE_PLAYER),
    NEW_HEALTH is CURRENT_HEALTH - DAMAGE_PLAYER,
    asserta(health(NPC, CURRENT_HEALTH)),
    write('Nyawa '), write(NPC), write(' berkurang '), write(DAMAGE_PLAYER), write('!'), nl,
    attackHelper3.

attackHelper2(WEAPON_PLAYER) :-
    weaponAmmo(WEAPON_PLAYER, AMMO_TYPE),
    \+AMMO_TYPE = none,
    inInventory(player, INVENTORY_LIST),
    member(AMMO_TYPE, INVENTORY_LIST),
    retract(inInventory(player, INVENTORY_LIST)),
    delete(INVENTORY_LIST, AMMO_TYPE, INVENTORY_LIST_NEW),
    asserta(inInventory(player, INVENTORY_LIST_NEW)),
    write(AMMO_TYPE), write(' berkurang 1!'), nl,
    position(X, Y),
    positionNPC(NPC, X, Y),
    write('Kamu menyerang '), write(NPC), write('! Kasihan dia!'), nl,
    health(NPC, CURRENT_HEALTH),
    retract(health(NPC, CURRENT_HEALTH)),
    damage(WEAPON_PLAYER, DAMAGE_PLAYER),
    NEW_HEALTH is CURRENT_HEALTH - DAMAGE_PLAYER,
    asserta(health(NPC, CURRENT_HEALTH)),
    write('Nyawa '), write(NPC), write(' berkurang '), write(DAMAGE_PLAYER), write('!'), nl,
    attackHelper3.

attackHelper2(WEAPON_PLAYER) :-
    weaponAmmo(WEAPON_PLAYER, AMMO_TYPE),
    \+AMMO_TYPE = none,
    inInventory(player, INVENTORY_LIST),
    \+member(AMMO_TYPE, INVENTORY_LIST),
    write('Kamu tidak memiliki '), write(AMMO_TYPE), write('! Rasain!'),
    attackHelper3.

attackHelper3 :-
    position(X, Y),
    positionNPC(NPC, X, Y),
    health(NPC, HEALTH_NPC),
    HEALTH_NPC > 0,
    inInventory(NPC, [WEAPON_NPC|TAIL]),
    damage(WEAPON_NPC, DAMAGE_NPC),
    write(NPC), write(' menyerang kamu! Nyawa kamu berkurang '), write(DAMAGE_NPC), write('!'), nl,
    retract(health(player, CURRENT_HEALTH)),
    NEW_HEALTH is CURRENT_HEALTH - DAMAGE_NPC,
    asserta(health(player, NEW_HEALTH)).

attackHelper3 :-
    position(X, Y),
    positionNPC(NPC, X, Y),
    health(NPC, HEALTH_NPC),
    \+HEALTH_NPC > 0,
    write(NPC), write(' telah kamu bunuh! Dia menjatuhkan hal berikut!'), nl,
    inInventory(NPC, INVENTORY_LIST),
    write(INVENTORY_LIST),
    drop(NPC).

dropNPC(NPC) :-
    inInventory(NPC, INVENTORY_LIST),
    retract(NPC, INVENTORY_LIST),
    positionNPC(NPC, X, Y),
    map_element(S, L, X, Y),
    retract(map_element(S, L, X, Y)),
    append(L, INVENTORY_LIST, NL),
    asserta(map_element(S, NL, X, Y)).

checkPlayerLife :-
    health(player, CURRENT_HEALTH),
    CURRENT_HEALTH < 0,
    write('Kamu mati! Permainan selesai!'), nl.

dropMedicine :-
    random(1, 4, RanNum),
    dropMedicineHelper(RanNum).

dropMedicineHelper(RanNum) :-
    RanNum = 1,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [panadol], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropMedicineHelper(RanNum) :-
    RanNum = 2,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [obhCombi], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropMedicineHelper(RanNum) :-
    RanNum = 3,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [minyakKayuPutih], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropMedicineHelper(RanNum) :-
    RanNum = 4,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [jamu], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropArmor :-
    random(1, 5, RanNum),
    dropArmorHelper(RanNum).

dropArmorHelper(RanNum) :-
    RanNum = 1,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [tameng], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropArmorHelper(RanNum) :-
    RanNum = 2,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [zirah], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropArmorHelper(RanNum) :-
    RanNum = 3,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [helm], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropArmorHelper(RanNum) :-
    RanNum = 4,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [jimat], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropArmorHelper(RanNum) :-
    RanNum = 5,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [batuAkik], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeapon :-
    random(1, 6, RanNum),
    dropWeaponHelper(RanNum).

dropWeaponHelper(RanNum) :-
    RanNum = 1,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [keris], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeaponHelper(RanNum) :-
    RanNum = 2,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [kujang], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeaponHelper(RanNum) :-
    RanNum = 3,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [bambuRuncing], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeaponHelper(RanNum) :-
    RanNum = 4,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [senapan], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeaponHelper(RanNum) :-
    RanNum = 5,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [sumpit], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeaponHelper(RanNum) :-
    RanNum = 6,
    random(1, 12, RX),
    random(1, 12, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [duit], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropRandomizer :-
    random(1, 3, RanNum),
    dropRandomizerHelper(RanNum).

dropRandomizerHelper(RanNum) :-
    RanNum = 1,
    dropArmor.

dropRandomizerHelper(RanNum) :-
    RanNum = 2,
    dropWeapon.

dropRandomizerHelper(RanNum) :-
    RanNum = 3,
    dropMedicine.

goodRandomizer(X, Y, XN, YN) :-
    map_element(S, _, X, Y),
    S = 'X',
    random(1, 12, XR),
    random(1, 12, YR),
    goodRandomizer(XR, YR, XN, YN).

goodRandomizer(X, Y, XN, YN) :-
    map_element(S, _, X, Y),
    \+S = 'X',
    XN = X, YN = Y.

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
do(s) :- s, moveAllEnemy, dropRandomizer, !.
do(n) :- n, moveAllEnemy, dropRandomizer, !.
do(e) :- e, moveAllEnemy, dropRandomizer, !.
do(w) :- w, moveAllEnemy, dropRandomizer, !.
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
do(take(Item)) :- take(Item),!.
do(attack) :- attack,!.
do(_) :- write('Perintah tidak valid!'),nl.

gameoverZonaMati :-
    position(A,B),
    map_element(C,_,A,B), C == 'X',
    write('KAMU KALAH! Lain Kali Jangan Mengenai Zona Mati ya!'),nl.

menang :-
  write('                               ___________________________________________'),nl,
  write('                                        Persembahan kami, untuk Anda.     '),nl,
  write('                               ___________________________________________'),nl.
