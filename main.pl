/* Init Game */
/*:-dynamic(location/1).*/
:-dynamic(map_element/4).
:-dynamic(position/2).

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

map_element(' X ',' - ',1,1).
map_element(' X ',' - ',1,2).
map_element(' X ',' - ',1,3).
map_element(' X ',' - ',1,4).

position(1,2).


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
  (A == quit).

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
  retract(map_element(_,_,1,1)),
  asserta(map_element(' X ',' - ',1,1)),
  retract(map_element(_,_,1,2)),
  asserta(map_element(' X ',' - ',1,2)),
  retract(map_element(_,_,1,3)),
  asserta(map_element(' X ',' - ',1,3)),
  retract(map_element(_,_,1,4)),
  asserta(map_element(' X ',' - ',1,4)),
  retract(position(_,_)),
  asserta(position(1,2)).

quit :-
  write(' Wangky gagal karena kamu :( ').




map :-
    (map_element(A,_,1,1) -> write(A)),
    (map_element(A,_,1,2) -> write(A)),
    (map_element(A,_,1,3) -> write(A)),
    (map_element(A,_,1,4) -> write(A)).


s :-
    (position(A,B) -> retract(map_element(_,_,A,B)), asserta(map_element(' P ',_,A,B))).

do(help):- help,!.
do(map):-map,!.
do(s) :- s,!.
do(quit):-quit,!.

menang :-
  write('                               ___________________________________________'),nl,
  write('                                        Persembahan kami, untuk Anda.     '),nl,
  write('                               ___________________________________________'),nl.



do(_):- write('Perintah tidak valid!'),nl.
