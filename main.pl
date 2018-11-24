/* Init Game */
/*:-dynamic(location/1).*/
:-dynamic(map_element/4).
:-dynamic(position/2).
:-dynamic(countMove/1).
:-dynamic(positionNPC/3).
:-dynamic(armor/2).
:-dynamic(health/2).
:-dynamic(weapon/3).
:-dynamic(inInventory/2).
:-dynamic(isNPC/1).
:-dynamic(isCheat/1).
:-dynamic(bag/2).


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



isBag(carrierBag).
isBag(ransel).
isBag(toteBag).

bagAmmout(carrierBag, 25).
bagAmmout(ransel, 15).
bagAmmout(toteBag,10).
bagAmmout(none, 5).

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

armorAmmount(zirah, 10).
armorAmmount(tameng,6).
armorAmmount(helm, 2).
armorAmmount(jimat, 2).
armorAmmount(batuAkik, 20).


start:-
    game_retractstart,!,
    game_start,!,
	write('      ##      ##    ###    ##    ##  ######   ##    ## ##    ## ####  ######          '),nl,
	write('      ##  ##  ##   ## ##   ###   ## ##    ##  ##   ##   ##  ##  #### ##    ##         '),nl,
	write('      ##  ##  ##  ##   ##  ####  ## ##        ##  ##     ####    ##  ##               '),nl,
	write('      ##  ##  ## ##     ## ## ## ## ##   #### #####       ##    ##    ######          '),nl,
	write('      ##  ##  ## ######### ##  #### ##    ##  ##  ##      ##               ##         '),nl,
	write('      ##  ##  ## ##     ## ##   ### ##    ##  ##   ##     ##         ##    ##         '),nl,
	write('       ###  ###  ##     ## ##    ##  ######   ##    ##    ##          ######          '),nl,
	write('                                                                                      '),nl,
	write('   ###    ########  ##     ## ######## ##    ## ######## ##     ## ########  ######## '),nl,
	write('  ## ##   ##     ## ##     ## ##       ###   ##    ##    ##     ## ##     ## ##       '),nl,
	write(' ##   ##  ##     ## ##     ## ##       ####  ##    ##    ##     ## ##     ## ##       '),nl,
	write('##     ## ##     ## ##     ## ######   ## ## ##    ##    ##     ## ########  ######   '),nl,
	write('######### ##     ##  ##   ##  ##       ##  ####    ##    ##     ## ##   ##   ##       '),nl,
	write('##     ## ##     ##   ## ##   ##       ##   ###    ##    ##     ## ##    ##  ##       '),nl,
	write('##     ## ########     ###    ######## ##    ##    ##     #######  ##     ## ######## '),nl,nl,

    write('  "Selamat Datang di Medan Pertempuran!"                                  '),nl,
    write('  Suara aneh yang seketika membangunkanmu dari tidurmu.                   '),nl,
    write('  Kamu mendapati dirimu terbangun di sebuah tempat antah berantah         '),nl,
    write('  kamu menemukan sebuah pesan di sebelah tempatmu terbangun,              '),nl,
    write('  Pesan itu bertulisankan:                                                '),nl,
    write('  "Wahai kamu pemuda yang senang bermalasan - malasan. Kini               '),nl,
    write('   kami leluhurmu, memanggilmu kesini, ke Medan Pertempuran ini,          '),nl,
    write('   agar kau tahu betapa beratnya melawan musuh- musuh bangsa,             '),nl,
	write('   untuk kembali ke asalmu, kamu harus menjadi satu - satunya yang selamat'),nl,
    write('   Semoga Sukses PEMUDA"                                                  '),nl,
    write('  selain pesan itu, kamu juga menemukan sebuah peta                       '),nl,nl,
	write('     X X X X X X X X X X X X X X X    '),nl,
	write('     X              |            X    '),nl,
	write('     X              |            X    '),nl,
	write('     X    PADANG    |   SAVANA   X    '),nl,
	write('     X    PASIR     |            X    '),nl,
	write('     X              |            X    '),nl,
	write('     X______________|            X    '),nl,
	write('     X            |P|___________ X    '),nl,
	write('     X            |              X    '),nl,
	write('     X            |              X    '),nl,
	write('     X   TUNDRA   |    HUTAN     X    '),nl,
	write('     X            |              X    '),nl,
	write('     X            |              X    '),nl,
	write('     X            |              X    '),nl,
	write('     X X X X X X X X X X X X X X X    '),nl,nl,
    write(' Berjuanlah, Demi Dirimu, Keluargamu, Teman - Teman mu, dan Bangsamu, !!!'),nl,nl,
	write('  Gunakan Command >help. untuk melihat apa saja yang bisa kamu lakukan! '),nl,
    repeat,
    write('>'), read(A),
    do(A),nl,
    (A == quit ; gameoverZonaMati; checkPlayerLife;menang).

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
  write('   T = Tas                                                                 '),nl,
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

    open('mulaigim.txt',read,In),
    (
      repeat,
      read_term(In, X, []),
      bacaFakta(X), !
    ),
    close(In),

    retract(map_element(_,_,8,8)),
    asserta(map_element('P',['player'],8,8)),

    initEnemy.


quit :-
  write(' Wangky gagal karena kamu :( ').

map1(N):-
  (map_element(A,_,N,1) -> write(A)),write(' '),
  (map_element(B,_,N,2) -> write(B)),write(' '),
  (map_element(C,_,N,3) -> write(C)),write(' '),
  (map_element(D,_,N,4) -> write(D)),write(' '),
  (map_element(E,_,N,5) -> write(E)),write(' '),
  (map_element(F,_,N,6) -> write(F)),write(' '),
  (map_element(G,_,N,7) -> write(G)),write(' '),
  (map_element(H,_,N,8) -> write(H)),write(' '),
  (map_element(I,_,N,9) -> write(I)),write(' '),
  (map_element(J,_,N,10) -> write(J)),write(' '),
  (map_element(K,_,N,11) -> write(K)),write(' '),
  (map_element(L,_,N,12) -> write(L)),write(' '),
  (map_element(M,_,N,13) -> write(M)),write(' '),
  (map_element(O,_,N,14) -> write(O)),write(' '),
  (map_element(P,_,N,15) -> write(P)),nl.

call_map(N) :- N == 16, !.
call_map(N) :- map1(N), N1 is N+1, call_map(N1).

map :- call_map(1).

initEnemy :-
    isNPC(ALL_ENEMY),
    forall(member(A,ALL_ENEMY), initEnemy(A)).

initEnemy(A) :-
    positionNPC(A, X, Y),
    retract(positionNPC(A, X, Y)),
    random(2, 14, NewX),
    random(2, 14, NewY),
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
    deleteOne(L, A, NL),
    write(NL), nl,
    asserta(map_element(S, NL, X, Y)),
    moveEnemyHelper(X, Y, NNewX, NNewY, 1),
    print(NNewX), print('spasi'), print(NNewY), nl,
    map_element(NextS, NextL, NNewX, NNewY),
    retract(map_element(NextS, NextL, NNewX, NNewY)),
    asserta(positionNPC(A, NNewX, NNewY)),
    append(NextL, [A], NewList),
    write(NewList), nl,
    asserta(map_element(NextS, NewList, NNewX, NNewY)).

moveEnemyHelper(X, Y, NewX, NewY, It) :-
    (It > 9),
    random(-1, 2, NewDX),
    random(-1, 2, NewDY),
    NX is X + NewDX, NY is Y + NewDY,
    normalizePosition(NX, NY, NNewX, NNewY),
    \+isEnemyHere(NNewX, NNewY),
    !,
    NewX is NNewX, NewY is NNewY.

moveEnemyHelper(X, Y, NewX, NewY, It) :-
    (It < 10),
    random(-1, 2, NewDX),
    random(-1, 2, NewDY),
    NX is X + NewDX, NY is Y + NewDY,
    normalizePosition(NX, NY, NNewX, NNewY),
    \+isEnemyHere(NNewX, NNewY),
    map_element(L,_,NNewX,NNewY),
    \+(L == 'X'),
    !,
    NewX is NNewX, NewY is NNewY.

moveEnemyHelper(X, Y, NewX, NewY, It) :-
    Itx is It+1,
    moveEnemyHelper(X, Y, NNewX, NNewY, Itx),
    NewX is NNewX, NewY is NNewY.

isEnemyHere(X, Y) :-
    map_element(_, L, X, Y),
    member(tentaraBelanda, L),!.

isEnemyHere(X, Y) :-
    map_element(_, L, X, Y),
    member(tentaraJepang, L),!.

isEnemyHere(X, Y) :-
    map_element(_, L, X, Y),
    member(antekPKI, L),!.

isEnemyHere(X, Y) :-
    map_element(_, L, X, Y),
    member(koruptor, L),!.

normalizePosition(X, Y, XN, YN) :-
    X < 1,
    !,
    XN is 1,
    normalizePosition(XN, Y, XN, YN).

normalizePosition(X, Y, XN, YN) :-
    Y < 1,
    !,
    YN is 1,
    normalizePosition(X, YN, XN, YN).

normalizePosition(X, Y, XN, YN) :-
    X > 15,
    !,
    XN is 15,
    normalizePosition(XN, Y, XN, YN).

normalizePosition(X, Y, XN, YN) :-
    Y > 15,
    !,
    YN is 15,
    normalizePosition(X, YN, XN, YN).

normalizePosition(X, Y, XN, YN) :-
    XN is X, YN is Y.

moveAllEnemy :- isCheat(X),X,!.

moveAllEnemy :-
    isNPC(ALL_ENEMY),
    forall(member(A,ALL_ENEMY), moveEnemy(A)).

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
    deleteOne(L,player,LX),
    retract(map_element(S,L,A1,B1)),
    asserta(map_element('-',LX,A1,B1)),
    asserta(position(A2,B2)),
    map_element(S1,L1,A2,B2),
    retract(map_element(S1,L1,A2,B2)),
    append(L1,[player],L1X),
    asserta(map_element('P',L1X,A2,B2)),
    retract(countMove(C)),
    Cx is C+1,
    asserta(countMove(Cx)),
    tambahDeadZone.

s :- position(A,B), Ax is (A+1), moveFromTo(A,B,Ax,B), narasi_lokasi(Ax,B).
n :- position(A,B), Ax is (A-1), moveFromTo(A,B,Ax,B), narasi_lokasi(Ax,B).
e :- position(A,B), Bx is (B+1), moveFromTo(A,B,A,Bx), narasi_lokasi(A,Bx).
w :- position(A,B), Bx is (B-1), moveFromTo(A,B,A,Bx), narasi_lokasi(A,Bx).

narasi_lokasi(X,Y) :-
    NS is X+1, NN is X-1, NE is Y+1, NW is Y-1,
    write('s'),narasiHelper(NS,Y),
    write('n'),narasiHelper(NN,Y),
    write('e'),narasiHelper(X,NE),
    write('w'),narasiHelper(X,NW).

narasiHelper(A,B):-
    A > 1, A < 8, B > 1, B < 9,!,
    write(' selanjutnya menuju ke Padang Pasir Terbuka'),nl.

narasiHelper(A,B):-
    A > 1, A < 9, B > 8, B < 15,!,
    write(' selanjutnya menuju ke Savana Terbuka'),nl.

narasiHelper(A,B):-
    A > 7, A < 15, B > 1, B < 8,!,
    write(' selanjutnya menuju ke Tundra'),nl.

narasiHelper(A,B):-
    A > 8, A < 15, B > 7, B < 15,!,
    write(' selanjutnya menuju ke Hutan'),nl.
narasiHelper(A,B):-
    A = 8, B = 8,!,
    write(' selanjutnya menuju ke Titik Tengah Arena'),nl.

narasiHelper(_,_):-
    write(' Menuju ke Deathzone').

look_pos(X,Y) :- map_element(A,_,X,Y), A == 'X', !, write(A).
look_pos(X,Y) :- map_element(_,_B,X,Y), _B == [], !, write('-').
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
look_pos(X,Y) :- map_element(_,_B,X,Y), member(toteBag,_B), !, write('T').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(carrierBag,_B), !, write('T').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(ransel,_B), !, write('T').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(panadol,_B), !, write('O').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(obhCombi,_B), !, write('O').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(minyakKayuPutih,_B), !, write('O').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(jamu,_B), !, write('O').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(peluru,_B), !, write('L').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(anaksumpit,_B), !, write('L').
look_pos(X,Y) :- map_element(_,_B,X,Y), member(player,_B), !, write('P').
look_pos(X,Y) :- map_element(_,_B,X,Y), write('P').

look_rek(_,_,C) :- C == 10, !.
look_rek(A,B,C) :-  0 is mod(C,3), !, look_pos(A,B), nl,
                    A1 is A+1, B1 is B-2, C1 is C+1, look_rek(A1,B1,C1).
look_rek(A,B,C) :- look_pos(A,B), B1 is B+1, C1 is C+1, look_rek(A,B1,C1).

look_desc(X,Y) :- map_element(_,_B,X,Y), _B == [player], !, write('Nyari apa kamu? gak ada apapun disini'),nl.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(tentaraBelanda,_B),
    write('Kamu melihat ada Tentara Belanda yang membawa senjata'),nl,
    write('Gunakan command >attack. untuk menyerangnya'), nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(tentaraJepang,_B),
    write('Kamu melihat seorang tentara yang berteriak Banzai, Banzai, Banzai, '), nl,
    write('kamu sudah tahu artinya.'), nl,
    write('Gunakan command >attack. untuk menyerang tentara wibu tersebut'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(antekPKI,_B),
    write('Penghianat yang ingin mengubah ideologi bangsa ini terdapat di depan matamu,'), nl,
    write(' dialah Antek PKI.'), nl,
    write('Gunakan command >attack. untuk menyadarkan pentingnya pancasila padanya.'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(koruptor,_B),
    write('Wow, kamu menemukan Koruptor, musuh tulen bangsa ini.'), nl,
    write('Gunakan command >attack. untuk menghapuskan eksistensinya.'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(keris,_B),
    write('Terdapat sebuah keris tergelatak disini. Diduga peninggalan dari jaman Majapahit.'),nl,
    write('Ambil dengan Command >take(keris). dan gunakan keris tersebut untuk '), nl,
    write('menuntaskan semua musuh bangsa.'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(kujang,_B),
    write('Sejata khas barudak sunda, kujang tergelatak didepanmu'), nl,
    write('Gunakan commad >take(kujang). untuk mewarisi semangat orang sunda '), nl,
    write('menghabisi semua musuh Indonesia.'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(bambuRuncing,_B),
    write('Terdapat bambu runcing disini, senjata legendaris yang dikisahkan dalam '), nl,
    write('buku buku sejarah digunakan oleh pejuang sebelumnya.'),nl,
    write('Gunakan command >take(bambuRuncing). untuk mewarisi semangat pejuang '), nl,
    write('terdahulu dan basmi mereka yang tersisa.'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(senapan,_B),
    write('WOW, kamu menemuka semua senapan kosong yang ditinggalkan oleh salah seorang tentara musuh'), nl,
    write('Ambil dengan command >take(senapan). dan buat pemilik senapan itu merasakan senjatanya senditi'), nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(sumpit,_B),
    write('Kamu melihat sebuah sumpit kosong disini, eits, itu bukan sumpit yang '), nl,
    write('digunakan orang orang china untuk makan, itu adalah sumpit yang digunakan '), nl,
    write('para pejuang dari timur.'), nl,
    write('Ambil dengan command >take(sumpit). dan warisi semangat orang timur '), nl,
    write('untuk memperbaiki negeri ini.'), nl,
    write('Jangan lupa mengisinya dengan anaksumpit terlebih dahulu.'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(duit,_B),
    write('WOW BOEY, kamu menemukan setumpuk duit yang ditinggalkan oleh anonimus.'), nl,
    write('Kamu bisa menggunakannya sebagai senjata, namun apakah kamu tega melakukan '), nl,
    write('hal sama dengan para koruptor itu?'),nl,
    write('Gunakan command >take(duit). untuk menyimpan uang tersebut.'), nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(tameng,_B),
    write('Hebat! Kamu menemukan tameng peninggalan kesatria jaman kerajaan.'), nl,
    write('Ambil dengan command >take(tameng). dan gunakan untuk melindungi dirimu '), nl,
    write('dan negeri ini dari mereka yang ingin menghancurkannya.'), nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(zirah,_B),
    write('SEBUAH ZIRAH YANG BERKILAU!!??? Tidak ada baju pelindung yang lebih kuat dari benda ini.'), nl,
    write('Ambil dengan command >take(zirah). dan gunakan untuk melindungi dirimu dari senjata musuh.'), nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(helm,_B),
    write('Kelihatannya ada yang menjatuhkan helmnya disini, aku merasa kasihan '), nl,
    write('jika dia sampai kena tilang karena helmnya tertinggal disini, tapi tenang, '), nl,
    write('ini medan pertempuran, tidak ada polisi lalu lintas disini.'), nl,
    write('Ambil dengan command >take(helm). dan lindungi kepalamu dari serangan musuh.'), nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(jimat,_B),
    write('Kamu menemukan sebuah jimat dengan kekuatan perlindungan magis yang sangat kuat.'), nl ,
    write('Konon katanya, seorang dukun yang sakti mandraguna membuat jimat tersebut.'), nl,
    write('Ambil dengan command >take(jimat). untuk mengambil jimat tersebut.'), nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(batuAkik,_B),
    write('Artifak peninggalan zaman pra-sejarah, yang dimuat dalam sebuah cincin, '), nl,
    write('kamu menemukan sebuah batu Akik disini.'),nl,
    write('Konon katanya batu akik tersebut memiliki kekuatan magis yang dapat '), nl,
    write('membuat penggunanya kebal terhadap senjata tajam'),nl,
    write('Ambil dengan command >take(batuAkik). dan gunakan kekuatan tersebut '), nl,
    write('agar dirimu bisa bertahan dalam neraka ini.'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(panadol,_B),
    write('Siapa orang Indonesia yang tidak tahu obat tersebut, Yap, kamu menemukan satu strip Panadol.'),nl,
    write('Mungkin aneh, tapi kamu bisa menggunakan panadol untuk mengobati luka lukamu.'),nl,
    write('Gunakan command >take(panadol). untuk menyimpan obat sakti tersebut agar kamu '), nl,
    write('bisa menggunakannya kelak disaat saat genting.'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(obhCombi,_B),
    write('BATUK PAK HAJI? tiba tiba kata tersebut terlintas di pikiranmu'), nl,
    write('karena kamu baru saja menemukan sebotol OBH Combi.'),nl,
    write('Eits tapi, di dunia ini segala jenis obat bisa digunakan untuk '), nl,
    write('mengobati luka apapun itu, keren banget emang.'),nl,
    write('Gunakan command >take(obhCombi). untuk menyimpannya kedalam tasmu'), nl,
    write('agar kamu makin terlihat seperti anak medis OSKM, eh agar kamu bisa mengobati luka lukamu nanti'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(minyakKayuPutih,_B),
    write('Apapun penyakit yang di derita kerokan adalah solusi utama orang Indonesia'),nl,
    write('dan WOW, kamu baru saja menemukan minyak kayu putih yang seringkali digunakan'),nl,
    write('untuk kerokan.'),nl,
    write('Gunakan command >take(minyakKayuPutih). untuk menyimpannya dalam tas kamu,'),nl,
    write('agar bisa digunakan kelak jika saja kamu masuk angin atau terkena tembakan'),nl,
    write('dari tentara musuh, Who Knows?.'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(jamu,_B),
    write('Kamu menemukan sebuah jamu segala penyakit di depan matamu. Yap, itu adalah jamu all in one.'),nl,
    write('Gunakan command >take(jamu). untuk mengambil dan menyimpan jamu tersebut ke dalam tas kamu'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(peluru,_B),
    write('Kamu menemukan 1 buah peluru senapan tergelatak tanpa pemilik disini.'),nl,
    write('Ambil dengan command >take(peluru). untuk menyimpan peluru tersebut.'),nl, fail.
look_desc(X,Y) :- map_element(_,_B,X,Y), member(anaksumpit,_B),
    write('Terdapat sebuah anaksumpit yang sudah dilumuri dengan racun yang mematikan di depan matamu.'),nl,
    write('ambil dengan command >take(anaksumpit). untuk menyimpannya di dalam tas kamu, '), nl,
    write('dan INGAT, pastikan mengambil dengan sangat hati hati.'),nl, fail.
look_desc(_,_) :- nl.

look :- position(A,B),
        A1 is A-1,
        B1 is B-1,
        look_rek(A1,B1,1),nl,
        look_desc(A,B),
        map_element(_,X,A,B),
        write(X).


cheathelper(N):-
      look_pos(N,1),
      look_pos(N,2),
      look_pos(N,3),
      look_pos(N,4),
      look_pos(N,5),
      look_pos(N,6),
      look_pos(N,7),
      look_pos(N,8),
      look_pos(N,9),
      look_pos(N,10),
      look_pos(N,11),
      look_pos(N,12),
	  look_pos(N,13),
      look_pos(N,14),
      look_pos(N,15),nl.

cheat :-
    cheathelper(1),cheathelper(2),cheathelper(3),cheathelper(4),cheathelper(5),cheathelper(6),cheathelper(7),
    cheathelper(8),cheathelper(9),cheathelper(10),cheathelper(11),cheathelper(12),cheathelper(13),cheathelper(14),cheathelper(15).

status :-
    inInventory(player,LI),
    health(player,H),
    armor(player,A),
    weapon(player,W,X),
    bag(player,B),
    bagAmmout(B,BA),
    length(LI,IA),
    write('Health : '), write(H), nl,
    write('Armor  : '), write(A), nl,
    write('Weapon : '), write(W), nl,
    write('Ammo   : '), write(X), nl,
    write('Bag    : '), write(B), nl,
    write('Isi Inventori : '), write('('), write(IA), write('/'),write(BA),write(')'), nl,
    tulisInventory(LI).

tulisInventory([])     :- write(' -Kosong-'), nl, !.
tulisInventory([H|[]]) :- write(' -'),write(H), nl,!.
tulisInventory([H|T])  :- write(' -'),write(H),nl, tulisInventory(T).

take(Item) :-
    position(X, Y),
    write(X), write(' '), write(Y), nl,
    map_element(_, L, X, Y),
    member(Item, L),
    bag(player, B),
    bagAmmout(B, BA),
    write(Item), write(' '), write(L), nl,
    inInventory(player, LI),
    length(LI, BNow),
    BNow < BA,
    write(LI), nl,
    retract(inInventory(player, LI)),
    write(LI), nl,
    append(LI, [Item], NLI),
    write(NLI), nl,
    asserta(inInventory(player, NLI)),
    retract(map_element(_, L, X, Y)),
    deleteOne(L, Item, LNEW),
    asserta(map_element(_, LNEW, X, Y)),
    retract(countMove(C)),
	    Cx is C+1,
    asserta(countMove(Cx)),
    write(Item), write(' sudah diambil! Kamu kini bertambah Kuat'), nl.

take(Item) :-
    write("DEBUG"),
    position(X, Y),
    write(X), write(' '), write(Y), nl,
    map_element(_, L, X, Y),
    member(Item, L),
    bag(player, B),
    bagAmmout(B, BA),
    write(Item), write(' '), write(L), nl,
    inInventory(player, LI),
    length(LI, BNow),
    BA =:= BNow,
    retract(countMove(C)),
	    Cx is C+1,
    asserta(countMove(Cx)),
    write(Item), write(' tidak bisa diambil, Tas kamu penuh.'), nl,
    write('Cari Item Bag yang lebih besar ukurannya atau buang beberapa Item kamu'), nl.

take(Item) :-
    position(X, Y),
    write(X), write(' '), write(Y), nl,
    map_element(_, L, X, Y),
    write(Item), write(' '), write(L), nl,
    \+member(Item, L), !,
    retract(countMove(C)),
	    Cx is C+1,
    asserta(countMove(Cx)),
    write('Kamu Bego ya?, Ndak ada '), write(Item), write(' di sini!, sering2 look makanya.'), nl.

/*ARMOR ITEM*/
use(Item) :-
    inInventory(player,LI),
    member(Item, LI),
    isArmor(Item),
    armor(player,X),
    armorAmmount(Item,ArmorAdded),
    deleteOne(LI,Item,NewLI),
    NewArmor is X + ArmorAdded,
    retract(armor(player,X)),
    asserta(armor(player,NewArmor)),
    retract(inInventory(player,LI)),
    asserta(inInventory(player,NewLI)),
    retract(countMove(C)),
    Cx is C+1,
    asserta(countMove(Cx)),
    write('Armor anda bertambah sebesar '), write(ArmorAdded), nl.

/*MEDICINE ITEM*/
use(Item) :-
    inInventory(player,LI),
    member(Item, LI),
    isMedicine(Item),
    health(player,HPAwal),
    medicine(Item,HealthAdded),
    deleteOne(LI,Item,NewLI),
    NewHealth is (HPAwal + HealthAdded),
    NewHealth > 100, !,
    retract(health(player,HPAwal)),
    asserta(health(player,100)),
    retract(inInventory(player,LI)),
    asserta(inInventory(player,NewLI)),
	retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    write('Health anda bertambah sebesar '), write(HealthAdded), nl.


use(Item) :-
    inInventory(player,LI),
    member(Item, LI),
    isMedicine(Item),
    health(player,X),
    medicine(Item,HealthAdded),
    deleteOne(LI,Item,NewLI),
    NewHealth is X + HealthAdded,
    retract(health(player,X)),
    asserta(health(player,NewHealth)),
    retract(inInventory(player,LI)),
    asserta(inInventory(player,NewLI)),
	retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    write('Health anda bertambah sebesar '), write(HealthAdded), nl.

/*WEAPON ITEM*/
/*Kasus none -> NewWeapon */
use(Item) :-
    inInventory(player,LI),
    member(Item, LI),
    isWeapon(Item),
    weapon(player,Old,_),
    deleteOne(LI,Item,NewLI),
    Old == none,!,
    retract(weapon(player,Old,_)),
    asserta(weapon(player,Item,0)),
    retract(inInventory(player,LI)),
    asserta(inInventory(player,NewLI)),
    retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    write('Kamu kini menggunakan '), write(Item), write(' sebagai senjata kamu'), nl.

/*Kasus RangeWeapon with Ammo -> NewWeapon*/
use(Item) :-
    inInventory(player,LI),
    member(Item, LI),
    isWeapon(Item),
    weapon(player,Old,TotalAmmo),
    \+Old == none,
    \+TotalAmmo == 0,!,
    weaponAmmo(Old,Ammo),
    deleteOne(LI,Item,TempLI),
    append(TempLI,[Old],NNLI),
    appendNElmt(Ammo, TotalAmmo, NNLI, NewLI),
    retract(weapon(player,Old, TotalAmmo)),
    asserta(weapon(player,Item, 0)),
    retract(inInventory(player,LI)),
    asserta(inInventory(player,NewLI)),
    retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    write('kamu kini menggunakan '), write(Item), write(' sebagai senjatamu'), nl,
    write('Senjata lamamu beserta ammonya kini disimpan dalam inventorimu'), nl.

/*Kasus HandWepon or RangeWeapon(Without Ammo) -> NewWeapon*/
use(Item) :-
    inInventory(player,LI),
    member(Item, LI),
    isWeapon(Item),
    weapon(player,Old,0),
    \+Old == none,
    deleteOne(LI,Item,TempLI),
    append(TempLI,[Old],NewLI),
    retract(weapon(player,Old, 0)),
    asserta(weapon(player,Item, 0)),
    retract(inInventory(player,LI)),
    asserta(inInventory(player,NewLI)),
    retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    write('Kamu kini menggunakan '), write(Item), write(' sebagai senjatamu'), nl.

/*AMMO ITEM*/
use(Item) :-
    inInventory(player,LI),
    member(Item, LI),
    isAmmo(Item),
    weaponAmmo(Weapon,Item),
    weapon(player, HandWeapon, AmmoNow),
    HandWeapon == Weapon,
    NewAmmo is AmmoNow +1,
    deleteOne(LI,Item,NewLI),
    retract(weapon(player,HandWeapon, AmmoNow)),
    asserta(weapon(player,HandWeapon, NewAmmo)),
    retract(inInventory(player,LI)),
    asserta(inInventory(player,NewLI)),
    retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    write('kamu mengisi kembali'), write(Weapon), write(' dengan 1 buah ammo'),nl.

/*BAG ITEM*/
use(Item) :-
    inInventory(player,LI),
    member(Item,LI),
    isBag(Item),
    bag(player, B),
    bagAmmout(B,BA_Old),
    bagAmmout(Item,BA_New),
    BA_Old > BA_New + 1,!,
    retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    write('Rugi kamu kalau menggunakan tas ini, karena inventorymu akan mengecil'),nl,
    write('Mending kamu buang dengan perintah >drop('), write(Item),write(')'), nl.

use(Item) :-
    inInventory(player,LI),
    member(Item,LI),
    isBag(Item),
    bag(player, B),
    bagAmmout(B,BA_Old),
    bagAmmout(Item,BA_New),
    BA_Old < BA_New,!,
    deleteOne(LI,Item,NewLI),
    retract(bag(player,B)),
    asserta(bag(player,Item)),
    retract(inInventory(player,LI)),
    asserta(inInventory(player,NewLI)),
    retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    write('Kamu sekararang menggunakan '), write(Item), write(' sebagai tas kamu.'),nl,
    write('MaxInventory bertambah sebesar '), Add is BA_New - BA_Old, write(Add),nl.

use(Item) :-
    inInventory(player,LI),
    \+member(Item, LI),
    retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    write('Tidak bisa menggunakan barang tersebut'),nl, write('karena barang tersebut tidak ada dalam inventori'), nl.

deleteOne([], _ , []).
deleteOne([Item|Tail],Item, Tail) :- !.
deleteOne([Head|Tail],Item, [Head|Result]) :-
  deleteOne(Tail,Item, Result).

appendNElmt(_,0,List,Result) :- Result = List, !.
appendNElmt(X,Count,List,Result) :- Count == 1, append(List,[X],Result),!.
appendNElmt(X,Count,List,Result) :- CountTemp is Count - 1, append(List,[X],NewLi), appendNElmt(X,CountTemp,NewLi,Result).

drop(Item) :-
    inInventory(player,LI),
    \+member(Item, LI),!,
    retract(countMove(C)),
	    Cx is C+1,
    asserta(countMove(Cx)),
    write('Orang kamu gapunya barang itu, gimana mau di drop bapak,'),nl.

drop(Item) :-
    inInventory(player,LI),
    member(Item,LI),isWeapon(Item),!,write('Anda menjatuhkan senjata '),write(LI),nl,
    retract(inInventory(player,LI)),
    deleteOne(LI,Item,NewLI),
    asserta(inInventory(player,NewLI)),
    position(A,B),
    retract(countMove(C)),
	    Cx is C+1,
    asserta(countMove(Cx)),
    retract(map_element(_,LPetak,A,B)),
    append(LPetak,[Item],NewLPetak),
    asserta(map_element(_,NewLPetak,A,B)).

drop(Item) :-
    inInventory(player,LI),
    member(Item,LI),isMedicine(Item),!,write('Anda menjatuhkan obat '),write(LI),nl,
    retract(inInventory(player,LI)),
    deleteOne(LI,Item,NewLI),
    asserta(inInventory(player,NewLI)),
    position(A,B),
    retract(countMove(C)),
	    Cx is C+1,
    asserta(countMove(Cx)),
    retract(map_element(_,LPetak,A,B)),
    append(LPetak,[Item],NewLPetak),
    asserta(map_element(_,NewLPetak,A,B)).

drop(Item) :-
    inInventory(player,LI),
    member(Item,LI),isArmor(Item),!,write('Anda menjatuhkan pelindung '),write(LI),nl,
    retract(inInventory(player,LI)),
    deleteOne(LI,Item,NewLI),
    asserta(inInventory(player,NewLI)),
    position(A,B),
    retract(countMove(C)),
	    Cx is C+1,
    asserta(countMove(Cx)),
    retract(map_element(_,LPetak,A,B)),
    append(LPetak,[Item],NewLPetak),
    asserta(map_element(_,NewLPetak,A,B)).

drop(Item) :-
    inInventory(player,LI),
    member(Item,LI),isAmmo(Item),!,write('Anda menjatuhkan Ammo '),write(LI),nl,
    retract(inInventory(player,LI)),
    deleteOne(LI,Item,NewLI),
    asserta(inInventory(player,NewLI)),
    position(A,B),
    retract(countMove(C)),
	    Cx is C+1,
    asserta(countMove(Cx)),
    retract(map_element(_,LPetak,A,B)),
    append(LPetak,[Item],NewLPetak),
    asserta(map_element(_,NewLPetak,A,B)).

drop(Item) :-
    inInventory(player,LI),
    member(Item,LI),isBag(Item),!,write('Anda menjatuhkan Tas '),write(LI),nl,
    retract(inInventory(player,LI)),
    deleteOne(LI,Item,NewLI),
    asserta(inInventory(player,NewLI)),
    position(A,B),
    retract(countMove(C)),
    Cx is C+1,
    asserta(countMove(Cx)),
    retract(map_element(_,LPetak,A,B)),
    append(LPetak,[Item],NewLPetak),
    asserta(map_element(_,NewLPetak,A,B)).

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
    map_element(_, _, X, Y),
    retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    checkEnemy,
    attackHelper1,!,
    write('ATTACK SEQUENCE 1'), nl.

attack :-
    position(X, Y),
    map_element(_, _, X, Y),
    retract(countMove(C)),
	Cx is C+1,
    asserta(countMove(Cx)),
    \+checkEnemy,
    write('Tidak ada musuh disini!'), nl.

%Weapon_Check
attackHelper1 :-
    weapon(player, WEAPON_PLAYER,_),
    \+WEAPON_PLAYER = none,
    attackHelper2(WEAPON_PLAYER),!.

attackHelper1 :-
    weapon(player, WEAPON_PLAYER,_),
    WEAPON_PLAYER = none,
    write('Kamu tidak memegang senjata! Cari mati kamu?'), nl.

%Player_Attack_Enemy
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
    asserta(health(NPC, NEW_HEALTH)),
    write('Nyawa '), write(NPC), write(' berkurang '), write(DAMAGE_PLAYER), write('!'), nl,
    write('Nyawa '), write(NPC), write(' sekarang tinggal '), write(NEW_HEALTH),nl,
    attackHelper3,!.

attackHelper2(WEAPON_PLAYER) :-
    weaponAmmo(WEAPON_PLAYER, AMMO_TYPE),
    weapon(player,WEAPON_PLAYER,AMMO_AMMOUNT),
    \+AMMO_TYPE = none,
    AMMO_AMMOUNT == 0,!,
    inInventory(player,INVENTORY_LIST),
    member(AMMO_TYPE,INVENTORY_LIST),
    write('Ammo kamu sedang kosong reload dulu mz'),nl.

attackHelper2(WEAPON_PLAYER) :-
    weaponAmmo(WEAPON_PLAYER, AMMO_TYPE),
    weapon(player,WEAPON_PLAYER,AMMO_AMMOUNT),
    \+AMMO_TYPE = none,
    AMMO_AMMOUNT == 0,
    inInventory(player,INVENTORY_LIST),
    \+member(AMMO_TYPE,INVENTORY_LIST),
    write('Ammo kamu sedang kosong, dan di inventori mu tidak tersedia'), write(AMMO_TYPE),nl.

attackHelper2(WEAPON_PLAYER) :-
    weaponAmmo(WEAPON_PLAYER, AMMO_TYPE),
    weapon(player,WEAPON_PLAYER,AMMO_AMMOUNT),
    \+AMMO_TYPE = none,
    AMMO_AMMOUNT > 0,!,
    NEW_AMMO is AMMO_AMMOUNT -1,
    retract(weapon(player,WEAPON_PLAYER,AMMO_AMMOUNT)),
    asserta(weapon(player,WEAPON_PLAYER,NEW_AMMO)),
    write(AMMO_TYPE), write(' berkurang 1!'), nl,
    position(X, Y),
    positionNPC(NPC, X, Y),
    write('Kamu menyerang '), write(NPC), write('!'), nl,
    health(NPC, CURRENT_HEALTH),
    retract(health(NPC, CURRENT_HEALTH)),
    damage(WEAPON_PLAYER, DAMAGE_PLAYER),
    NEW_HEALTH is CURRENT_HEALTH - DAMAGE_PLAYER,
    asserta(health(NPC, NEW_HEALTH)),
    write('Nyawa '), write(NPC), write(' berkurang '), write(DAMAGE_PLAYER), write('!'), nl,
    write('Nyawa '), write(NPC), write(' sekarang tinggal '), write(NEW_HEALTH),nl,
    attackHelper3,!.

%Enemy_Attacking_Player
attackHelper3 :-
    position(X, Y),
    positionNPC(NPC, X, Y),
    health(NPC, HEALTH_NPC),
    HEALTH_NPC > 0,
    inInventory(NPC, [WEAPON_NPC|_]),
    armor(player,ARMOR_AMMOUNT),
    damage(WEAPON_NPC, DAMAGE_NPC),
    DIVIDE_DAMAGE is ARMOR_AMMOUNT,
    DAMAGE_NOW is DAMAGE_NPC - DIVIDE_DAMAGE,
    DAMAGE_NOW>0,!,
    write(NPC), write(' menyerang kamu! Nyawa kamu berkurang '),
    write(DAMAGE_NOW), write('!'), nl,
    retract(health(player, CURRENT_HEALTH)),
    NEW_HEALTH is CURRENT_HEALTH - DAMAGE_NOW,
    asserta(health(player, NEW_HEALTH)).

attackHelper3 :-
    position(X, Y),
    positionNPC(NPC, X, Y),
    health(NPC, HEALTH_NPC),
    HEALTH_NPC > 0,
    inInventory(NPC, [WEAPON_NPC|_]),
    armor(player,ARMOR_AMMOUNT),
    damage(WEAPON_NPC, DAMAGE_NPC),
    DIVIDE_DAMAGE is ARMOR_AMMOUNT,
    DAMAGE_NOW is DAMAGE_NPC - DIVIDE_DAMAGE,
    \+DAMAGE_NOW>0,!,
    write(NPC), write(' menyerang kamu! Nyawa kamu berkurang sebanyak 0!'),
    nl,
    write('Armor kamu terlalu kuat!'),
    nl.


attackHelper3 :-
    position(X, Y),
    positionNPC(NPC, X, Y),
    health(NPC, HEALTH_NPC),
    \+HEALTH_NPC > 0,
    write(NPC), write(' telah kamu bunuh! Dia menjatuhkan hal berikut!'), nl,
    inInventory(NPC, INVENTORY_LIST),
    write(INVENTORY_LIST),
    dropNPC(NPC),
    retract(map_element(S, L, X, Y)),
    deleteOne(L,NPC,NL),
    asserta(map_element(S, NL, X, Y)),
    isNPC(ALL_ENEMY),
    member(NPC,ALL_ENEMY),
    retract(isNPC(ALL_ENEMY)),
    deleteOne(ALL_ENEMY,NPC,NEW_RECUCED_ENEMY),
    asserta(isNPC(NEW_RECUCED_ENEMY)).

dropNPC(NPC) :-
    retract(inInventory(NPC, INVENTORY_LIST)),
    positionNPC(NPC, X, Y),
    map_element(S, L, X, Y),
    retract(map_element(S, L, X, Y)),
    append(L, INVENTORY_LIST, NL),
    asserta(map_element(S, NL, X, Y)).

checkPlayerLife :-
    health(player, CURRENT_HEALTH),
    CURRENT_HEALTH < 1,
    write('Kamu mati! Permainan selesai!'), nl.

dropMedicine :-
    random(1, 5, RanNum),
    dropMedicineHelper(RanNum).

dropMedicineHelper(RanNum) :-
    RanNum = 1,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [panadol], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropMedicineHelper(RanNum) :-
    RanNum = 2,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [obhCombi], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropMedicineHelper(RanNum) :-
    RanNum = 3,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [minyakKayuPutih], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropMedicineHelper(RanNum) :-
    RanNum = 4,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [jamu], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropArmor :-
    random(1, 6, RanNum),
    dropArmorHelper(RanNum).

dropArmorHelper(RanNum) :-
    RanNum = 1,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [tameng], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropArmorHelper(RanNum) :-
    RanNum = 2,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [zirah], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropArmorHelper(RanNum) :-
    RanNum = 3,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [helm], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropArmorHelper(RanNum) :-
    RanNum = 4,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [jimat], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropArmorHelper(RanNum) :-
    RanNum = 5,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [batuAkik], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeapon :-
    random(1, 7, RanNum),
    dropWeaponHelper(RanNum).

dropWeaponHelper(RanNum) :-
    RanNum = 1,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [keris], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeaponHelper(RanNum) :-
    RanNum = 2,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [kujang], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeaponHelper(RanNum) :-
    RanNum = 3,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [bambuRuncing], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeaponHelper(RanNum) :-
    RanNum = 4,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [senapan], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeaponHelper(RanNum) :-
    RanNum = 5,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [sumpit], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropWeaponHelper(RanNum) :-
    RanNum = 6,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [duit], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropAmmo :-
    random(1, 3, RanNum),
    dropAmmoHelper(RanNum).

dropAmmoHelper(RanNum) :-
    RanNum = 1,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [peluru], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropAmmoHelper(RanNum) :-
    RanNum = 2,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [anaksumpit], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropBag :-
    random(1,6, RanNumber),
    RanNum is RanNumber mod 3,
    dropBagHelper(RanNum).

dropBagHelper(RanNum) :-
    RanNum = 1,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [toteBag], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropBagHelper(RanNum) :-
    RanNum = 2,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [ransel], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropBagHelper(RanNum) :-
    RanNum = 0,
    random(1, 15, RX),
    random(1, 15, RY),
    goodRandomizer(RX, RY, RanX, RanY),
    map_element(S, L, RanX, RanY),
    retract(map_element(S, L, RanX, RanY)),
    append(L, [carrierBag], NewL),
    write(NewL), write(' di ('), write(RanX), write(', '), write(RanY), write(')'), nl,
    asserta(map_element(S, NewL, RanX, RanY)).

dropRandomizer :-
	countMove(CM),
	TotalRandom is CM div 5 + 1,
	forall(between(1,TotalRandom,_),
		(
			random(1, 10, RanNumber),
            RanNum is RanNumber mod 5,
			dropRandomizerHelper(RanNum)
		)
	).

dropRandomizerHelper(RanNum) :-
    RanNum = 1,
    dropMedicine.

dropRandomizerHelper(RanNum) :-
    RanNum = 2,
    dropWeapon.

dropRandomizerHelper(RanNum) :-
    RanNum = 3,
    dropArmor.

dropRandomizerHelper(RanNum) :-
    RanNum = 4,
    dropAmmo.

dropRandomizerHelper(RanNum) :-
    RanNum = 0,
    dropBag.

goodRandomizer(X, Y, XN, YN) :-
    map_element(S, _, X, Y),
    S = 'X',
    random(1, 15, XR),
    random(1, 15, YR),
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
    retract(map_element(_,X12,N,12)), asserta(map_element('X',X12,N,12)),
	retract(map_element(_,X13,N,13)), asserta(map_element('X',X13,N,13)),
    retract(map_element(_,X14,N,14)), asserta(map_element('X',X14,N,14)),
    retract(map_element(_,X15,N,15)), asserta(map_element('X',X15,N,15)).

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
	retract(map_element(_,X12,12,N)), asserta(map_element('X',X12,12,N)),
	retract(map_element(_,X13,13,N)), asserta(map_element('X',X13,13,N)),
	retract(map_element(_,X14,14,N)), asserta(map_element('X',X14,14,N)),
	retract(map_element(_,X15,15,N)), asserta(map_element('X',X15,15,N)).

tambahDeadZone :- isCheat(X),X,!.
tambahDeadZone :-
    countMove(A), A == 5,!,updatemapbaris(2),updatemapbaris(14), updatemapkolom(2),updatemapkolom(14).
tambahDeadZone :-
    countMove(A), A == 12,!,updatemapbaris(3),updatemapbaris(13), updatemapkolom(3),updatemapkolom(13).
tambahDeadZone :-
    countMove(A), A == 21,!,updatemapbaris(4),updatemapbaris(12), updatemapkolom(4),updatemapkolom(12).
tambahDeadZone :-
    countMove(A), A == 32,!,updatemapbaris(5),updatemapbaris(11), updatemapkolom(5),updatemapkolom(11).
tambahDeadZone :-
    countMove(A), A == 45,!,updatemapbaris(6),updatemapbaris(10), updatemapkolom(6),updatemapkolom(10).
tambahDeadZone :-
    countMove(A), A == 50,!,updatemapbaris(7),updatemapbaris(9), updatemapkolom(7),updatemapkolom(9).
tambahDeadZone.

save(FileName) :-
    open(FileName,write,Save),
    %countMove
    countMove(CM),
    write(Save,CM),
    write(Save,'.'),nl(Save),

    %Posisi
    %Player
    position(PX,PY),
    write(Save,PX),
    write(Save,'. '),
    write(Save,PY),
    write(Save,'.'), nl(Save),

    %NPC
    positionNPC(tentaraBelanda,TBX,TBY),
    write(Save,TBX),
    write(Save,'. '),
    write(Save,TBY),
    write(Save,'.'),nl(Save),

    positionNPC(tentaraJepang,TJX,TJY),
    write(Save,TJX),
    write(Save,'. '),
    write(Save,TJY),
    write(Save,'.'),nl(Save),

    positionNPC(koruptor,KX,KY),
    write(Save,KX),
    write(Save,'. '),
    write(Save,KY),
    write(Save,'.'),nl(Save),

    positionNPC(antekPKI,PKIX,PKIY),
    write(Save,PKIX),
    write(Save,'. '),
    write(Save,PKIY),
    write(Save,'.'),nl(Save),

    %Status_Health
    %Player
    health(player,HP),
    write(Save,HP),
    write(Save,'.'),nl(Save),

    %NPC
    health(tentaraBelanda,TBHP),
    write(Save,TBHP),
    write(Save,'.'),nl(Save),

    health(tentaraJepang,TJHP),
    write(Save,TJHP),
    write(Save,'.'),nl(Save),

    health(koruptor,KHP),
    write(Save,KHP),
    write(Save,'.'),nl(Save),

    health(antekPKI,PKIHP),
    write(Save,PKIHP),
    write(Save,'.'),nl(Save),

    %Status_Armor
    %Player
    armor(player,DEF),
    write(Save,DEF),
    write(Save,'.'), nl(Save),

    %Status_Weapon
    %Player
    weapon(player,Weapon, AmmoCount),
    atom_concat(Weapon,'. ', WPN),
    write(Save, WPN),
    write(Save, AmmoCount),
    write(Save, '.'), nl(Save),

    %Status_Inventory
    %Player
    inInventory(player,LI),
    write(Save,LI),
    write(Save,'.'),nl(Save),

    %Status_Bag
    bag(player,BAG),
    write(Save,BAG),
    write(Save,'.'), nl(Save),

    %NPC
    inInventory(tentaraBelanda,TBLI),
    write(Save,TBLI),
    write(Save,'.'),nl(Save),

    inInventory(tentaraJepang,TJLI),
    write(Save,TJLI),
    write(Save,'.'),nl(Save),

    inInventory(koruptor,KLI),
    write(Save,KLI),
    write(Save,'.'),nl(Save),

    inInventory(antekPKI,PKILI),
    write(Save,PKILI),
    write(Save,'.'),nl(Save),

    %Map_Element
    forall(between(1,15,I),
        (
            forall(between(1,15,J),
                (
                    map_element(ME1,ME2,I,J),
                    write(Save,'('),
                    writeq(Save,ME1),
                    write(Save,'). '),
                    write(Save,ME2),write(Save,'.'),nl(Save)
                )
            )
        )
    ),

    close(Save),
    write('Save Berhasil').

loadf(FileName):-
    open(FileName,read,Baca),
    %countMove
    retract(countMove(_)),
    read(Baca,CM),
    asserta(countMove(CM)),

    %Posisi
    %Player
    retract(position(_,_)),
    read(Baca,PX),
    read(Baca,PY),
    asserta(position(PX,PY)),

    %NPC
    retract(positionNPC(tentaraBelanda,_,_)),
    read(Baca,TBPX),
    read(Baca,TBPY),
    asserta(positionNPC(tentaraBelanda,TBPX,TBPY)),

    retract(positionNPC(tentaraJepang,_,_)),
    read(Baca,TJPX),
    read(Baca,TJPY),
    asserta(positionNPC(tentaraJepang,TJPX,TJPY)),

    retract(positionNPC(koruptor,_,_)),
    read(Baca,KPX),
    read(Baca,KPY),
    asserta(positionNPC(koruptor,KPX,KPY)),

    retract(positionNPC(antekPKI,_,_)),
    read(Baca,PKIPX),
    read(Baca,PKIPY),
    asserta(positionNPC(antekPKI,PKIPX,PKIPY)),

    %Status_Health
    %Player
    retract(health(player,_)),
    read(Baca,HP),
    asserta(health(player,HP)),

    %NPC
    retract(health(tentaraBelanda,_)),
    read(Baca,TBHP),
    asserta(health(tentaraBelanda,TBHP)),

    retract(health(tentaraJepang,_)),
    read(Baca,TJHP),
    asserta(health(tentaraJepang,TJHP)),

    retract(health(koruptor,_)),
    read(Baca,KHP),
    asserta(health(koruptor,KHP)),

    retract(health(antekPKI,_)),
    read(Baca, PKIHP),
    asserta(health(antekPKI,PKIHP)),

    %Status_Armor
    %Player
    retract(armor(player,_)),
    read(Baca, DEF),
    asserta(armor(player,DEF)),

    %Status_Weapon
    %Player

    retract(weapon(player,_,_)),
    read(Baca,WPN),
    read(Baca,AmmoCount),
    asserta(weapon(player,WPN,AmmoCount)),

    %Status_Inventory
    %Player
    retract(inInventory(player,_)),
    read(Baca,LI),
    asserta(inInventory(player,LI)),

    %Player_Bag
    retract(bag(player,_)),
    read(Baca,BAG),
    asserta(bag(player,BAG)),

    %NPC
    retract(inInventory(tentaraBelanda,_)),
    read(Baca,TBLI),
    asserta(inInventory(tentaraBelanda,TBLI)),

    retract(inInventory(tentaraJepang,_)),
    read(Baca,TJLI),
    asserta(inInventory(tentaraJepang,TJLI)),

    retract(inInventory(koruptor,_)),
    read(Baca,KLI),
    asserta(inInventory(koruptor,KLI)),

    retract(inInventory(antekPKI,_)),
    read(Baca,PKILI),
    asserta(inInventory(antekPKI,PKILI)),
    %Map_Element
    forall(between(1,15,I),
        (
            forall(between(1,15,J),
                (
                    retract(map_element(_,_,I,J)),
                    read(Baca,Type),
                    read(Baca,Isi),
                    asserta(map_element(Type,Isi,I,J))
                )
            )
        )
    ),
    close(Baca),
    write('Load Berhasil'),nl.

do(help):- help,!.
do(map):-map,!.
do(s) :- s, moveAllEnemy, dropRandomizer, !.
do(n) :- n, moveAllEnemy, dropRandomizer, !.
do(e) :- e, moveAllEnemy, dropRandomizer, !.
do(w) :- w, moveAllEnemy, dropRandomizer, !.
do(quit) :-quit,!.
do(gameover) :-gameover,!.
do(look) :- look,!.
do(drop(A)) :- drop(A),!.
do(tambahDeadZone) :- tambahDeadZone,!.
do(countMove(A)) :- countMove(A),!.
do(updatemapbaris(A)) :- updatemapbaris(A),!.
do(updatemapkolom(A)) :- updatemapkolom(A),!.
do(status) :- status,!.
do(use(Item)) :- use(Item),!.
do(take(Item)) :- take(Item),!.
do(attack) :- attack,!.
do(cheat) :- asserta(isCheat(true)), cheat.
do(save(FileName)) :- save(FileName),!.
do(load(FileName)) :- loadf(FileName),!.
do(_) :- write('Perintah tidak valid!'),nl.

gameoverZonaMati :-
    position(A,B),
    map_element(C,_,A,B), C == 'X',
    write('KAMU KALAH! Lain Kali Jangan Mengenai Zona Mati ya!'),nl.

menang :-
  isNPC([]),!,
  write('                               ___________________________________________'),nl,
  write('                                        Persembahan kami, untuk Anda.     '),nl,
  write('                               ___________________________________________'),nl.
