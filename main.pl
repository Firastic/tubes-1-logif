/* Init Game */
health(player,100).
armor(player,0).
weapon(player, barehand).
inInventory(player,).
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