# Wangky's Adventure
## Deskripsi Game
  Dalam Game ini, anda akan bermain sebagai Wangky. Wangky adalah seorang pejuang muda dengan jiwa Nasionalis yang tinggi. Berbekal senjata - senjata seadanya, anda harus bertahan dalam medan pertempuran dan menghabisi semua musuh Indonesia. Mereka adalah Tentara Belanda, Tentara Jepang, Antek PKI bahkan hingga Koruptor yang melegenda. Mampukah anda memerankan wangky menaklukan medan pertempuran ini? Uji kemampuan Anda dalam game ini.
  
## Pre-Requisite
  Sebelum memulai game ini, anda harus memiliki beberapa hal berikut.
  1. [GNU Prolog](http://www.gprolog.org/#download).
  2. Seluruh file dalam Repo ini.
  3. Your time and determination, trust me, game ini susah.

## Initialisasi Game
  Untuk memainkan game ini, anda bisa menggunakan salah 1 dari 3 cara berikut.
  ### Pertama
  * Buka GNU Prolog
  * Pilih File
  * Pilih Consult
  * Pilih Main.pl
  ### Kedua
  * Buka CMD
  * Tulis
  ```
  $ gplc main.pl
  ```
  * Jalankan via cmd
  ```
  $ main
  ```
  atau untuk Ubuntu
  ```
  $ ./main
  ```
  ### Ketiga
  Jalankan langsung main.pl dengan gprolog.

## How To Play
  Setelah membuka dan compile terhadap main.pl sukses, gunakan command ```|- start. ``` untuk memulai menjalankan game.
  ### Fungsi dalam Game
  ```
  Perintah yang dapat anda jalankan.
  start. -- Mulai game                                                     
  help. -- Memunculkan perintah-perintah yang diperlukan                  
  quit. -- Keluar dari game                                                
  look. -- Lihat sekeliling Anda!                                         
  n. s. e. w. --Bergerak ke utara,selatan,timur,barat!                    
  map. -- Melihat peta!                                                   
  take(ObjectName). -- Mengambil objek pada petak sekarang.                   
  drop(ObjectName), -- Menjatuhkan objek pada petak sekarang.                 
  use(ObjectName), -- Memakai objek yang Anda sudah ambil!                    
  attack. -- Menyerang musuh!                                             
  status. -- Menunjukkan status gim Anda sekarang!                        
  save(namafile.txt). -- Menyimpan file game Anda!                         
  load(namafile.txt). -- Memasukkan data dari file ke game. 
  ```
  ### Simbol simbol dalam Game
  ```
  Beberapa simbol yang Anda perlu tahu:                                   
  S = Senjata                                                             
  L = Ammo/Logistik(nama lainnya pelor lah)                               
  A = Armor/Perlengkapan Perang                                           
  O = Obat                                                                
  P = Pemain                                                              
  M = Musuh                                                               
  - = Petak yang bisa diakses                                             
  X = Zona mati. Jangan bergerak ke Zona ini!              
  ```
  
## How To Win
  Untuk menyelesaikan dan memenangkan game ini, anda cukup menjadi orang terakhir yang tersisa, yaitu dengan membunuh semua musuh yang ada.
  
## Fail State
  Anda dikategorikan kalah dalam game ini, jika
  1. Anda keluar dari game sebelum selesai.
  2. Anda terbunuh baik dari serangan musuh, maupun karena masuk ke DeathZone.
