# GAD - Galaksija debager

GAD je aplikacija za nalaženje i otklanjanje grešaka u programima pisanim za retro računar [Galaksija](https://sr.wikipedia.org/wiki/%D0%93%D0%B0%D0%BB%D0%B0%D0%BA%D1%81%D0%B8%D1%98%D0%B0_(%D1%80%D0%B0%D1%87%D1%83%D0%BD%D0%B0%D1%80)). Napisana je u asemblerskom jeziku procesora Z80 i primarna svrha joj je da olakša izradu drugih asemblerskih programa.

Minimalna konfiguracija za izvršavanje programa je Galaksija sa ugrađenim ROM A i ROM B (nekad označeni i kao ROM 1 i ROM 2) i nekakvim memorijskim proširenjem ili nova verzija Galaksije iz 2024. godine. Originalna Galaksija sa 6 KB RAM-a je tehnički dovoljna za učitavanje i izvršavanje GAD aplikacije ali nema dovoljno preostale memorije za program koji se debagira.

Startna adresa je prvi bajt programa, dakle, ista ona adresa koja je navedena u ORG direktivi. Asemblerski kod sadrži nekoliko nestandardnih definicija struktura koje su napisane prema sintaksnim pravilima SjASMPlus asemblera. U slučaju upotrebe drugog asemblerskog programa, ove definicije se moraju prilagoditi sintaksnim pravilima korišćenog razvojnog alata.

## Osobine programa

- Do četiri softverske prekidne tačke (breakpoint) postavljene na bilo koju asemblersku instrukciju
- Mogućnost izvršavanja instrukciju po instrukciju
- Ulazak u potprogram (step into) ili zaustavljanje po izvršenom potprogramu (step over)
- Pregled i izmena vrednosti svih registara i pojedinačnih statusnih flegova u bilo kom trenutku
- Izvršenje komandi na jedan pritisak tastera gde god je to moguće
- Komandna linija za unos složenijih komandi sa parametrima
- Implementirana većina funkcionalnosti monitorskog programa
- Ne menja sadržaj sistemske ekranske memorije
- Koristi privatni stek i privatni ulazni bafer tastature
- Prilično detaljno obaveštavanje o greškama nastalim tokom izvršavanja komandi programa
- Umereno korišćenje memorije (oko 4,5 kilobajta)

Izvršavanje instrukciju po instrukciju i prekidne tačke se mogu upotrebljavati samo za debagiranje programa u radnoj memoriji (RAM), dok sve ostale funkcionalnosti rade bilo sa RAM ili ROM memorijom.

## Opis

Sledeća slika prikazuje tipičan sadržaj ekrana GAD programa. GAD-ov ekran je izdeljen na šest fiksnih područja.

![GAD screen sections.](/images/dbg-sections.png)

Gornji red pregleda registara (_Registers view_) prikazuje vrednosti glavnog skupa registara, kao i vrednosti registara IX i SP. Donji red prikazuje vrednosti alternativnog skupa registara, registra IY i dva bajta sa memorijske lokacije na koju pokazuje SP registar, respektivno.

Pregled pojedinačnih flegova (_Individual flags view_) je još jedan način za prikaz vrednosti F registra. Svaki fleg F registra je prikazan kao crtica (-) ukoliko je resetovan ili kao slovna oznaka ukoliko je setovan, početno od najnižeg bita na desnoj strani do najvišeg bita na levoj strani. Na primer, `SZ H VNC` je prikazano kada su svi bitovi setovani.

Prozor disasemblirane memorije (_Disassembler window_) prikazuje sadržaj memorije u obliku asemblerskog koda. Deo memorije koji se ovde prikazuje može promenjen komandom `D`, a nekakva osnovna mogućnost skrolovanja je omogućena pritiskom na tastere `/` za skrolovanje unapred, i `B` za skrolovanje unazad. Unazad se skroluje samo u koracima od po jednog bajta, tako da prikazani disasemblirani kod ne mora posle svakog koraka biti tačan!

Vrednost programskog brojača (PC) je uvek adresa prve disasemblirane linije i predstavljena je `>` karakterom postavljenim ispred mnomonika instrukcije. Vrednost registra PC se može promeniti `D from` ili `R PC value` komandama, ili implicitno komandom `G addr`.

Prostor od četiri linije za prikaz sadržaja memorije (_Memory dump window_) prikazuje heksadecimalni sadržaj memorije sa po osam bajtova u svakoj liniji. Menjanje početne adrese ovog prozora se vrši `M` komandom, a za skrolovanje se koriste strelice na gore (`↑`) i dole (`↓`).

Komandna linija (_Command line_) se koristi za unos komandi sa parametrima. Ona je sakrivena za vreme aktivnog režima izdavanja komandi preko prečica na tastaturi. Prelazak na režim unosa komandi iz komandne linije se vrši pritiskom na taster `I` (od eng. reči `input`). Obrnuto, izlazak iz režima komandne linije se izvodi unosom komande `X` (otkucajte X i pritisnite ENTER).

Poslednja linija na dnu ekrana je prostor za poruke programa (_Messages area_). Ovo je linija u kojoj program ispisuje statusne informacije i poruke o greškama.

### Poruke programa

GAD koristi nekoliko opštih poruka o greškama koje uvek imaju isto značenje, gde god da se pojave. Ove poruke su:

- `WHAT?` poruka se ispisuje ako je unet pogrešan broj parametara komande.
- `HOW?` poruka se ispisuje ako neki od parametara ima pogrešnu vrednost.
- `SORRY` poruka se ispisuje ako komanda, zbog nekog razloga, ne može biti izvršena. Razlog zavisi od tipa izdate komande, ali najćešće je to ukoliko se zahteva izvršenje komande nad nedozvoljenim delom memorijskog prostora (na primer, adrese od &2000 do &27FF).
- `OK` poruka se prikazuje ukoliko komanda ne menja sadržaj ekrana programa, a da bi obavestila korisnika da je uspešno izvršena.

Postoje i druge poruke programa koje su specifične za određene komande i one će biti predstavljene u okviru uputstva za te komande.

## Komande programa

### Komande izdate korišćenjem prečice na tastaturi

>  Korisno je napomenuti da prilikom rada u emulatoru prečice na tastaturi odgovaraju Galaksijinom rasporedu tastera, a ne PC-jevom.

#### Osnovne prečice na tastaturi

| Taster | Opis
|------|---------------
| `I`  | Ulazak u režim unosa iz komande linije
| `ENTER` | [Izvršava tekuću instrukciju (step over režim)](#enter-step-over)
| `.`  | [Izvršava tekuću instrukciju sa ulaskom u potprogram (step into režim)](#step-into)
| `;`  | [Nastavlja izvršavanje do prekidne tačke](#continue-execution)
| `/`  | Preskače tekuću instrukciju (uvećava vrednost PC registra)
| `B`  | Umanjuje vrednost PC registra za jedan
| `SPACE` | Preklapa prikaz između sistemskog i GAD ekrana
| `F`  | [Traži unapred](#search-forward)
| `X`  | Izlazak iz programa

#### Komande za menjane vrednosti statusnih flegova

| Taster | Opis
|------|---------------
| `C` | Invertuje Carry fleg
| `N` | Invertuje Negative fleg
| `V` | Invertuje parity/oVerflow fleg
| `H` | Invertuje Half carry fleg
| `Z` | Invertuje Zero fleg
| `S` | Invertuje Sign fleg

#### Komande za navigaciju u prozoru za prikaz sadržaja memorije

| Taster | Opis
|------|---------------
| `↑` | Skroluje prikaz memorije naviše
| `↓` | Skroluje prikaz memorije naniže

### Komande koje se izdaju preko komandne linije

Većina parametara komandi predstavlja memorijske adrese defisane kao heksadecimalne ili decimalne cifre dužine do najviše četiri cifre. Heksadecimalni brojevi moraju imati paran broj cifara i moraju kao prefiks sadržati znak `&`. Decimalni brojevi takođe mogu biti dužine do najviše četiri cifre, što znači da je najveći dozvoljeni decimalni broj 9999. Izuzetak od prethodnog pravila je _hex_string_ parametar, koji je definisan kao paran broj heksadecimalnih cifara, sa opcionim prefiksom `&` (na primer: &114AB7)

| Komanda | Sintaksa | Opis
|-----|--------|------
| `D` | _from_ | [Disasemblira memoriju i postavlja vrednost PC registra](#disassemble)
| `M` | _from_ | [Ispisuje sadržaj memorije](#memory-dump)
| `E` | _from_ _hex_string_ | [Uređuje sadržaj memorije](#edit-memory)
| `B` | _no_ _addr_ | [Postavlja/uklanja/prikazuje prekidne tačke](#breakpoint-set)
| `R` | _reg_ _value_ | [Postavlja vrednost registra ili registarskog para](#set-register)
| `C` | _start_ _end_ _target_ | [Kopira memoriju](#copy-memory)
| `S` | _start_ _end_ _hex_string_ | [Pretražuje memoriju](#search-memory)
| `F` | _start_ _end_ _byte_ | [Puni memoriju](#fill-memory)
| `P` | _addr_ | [Nastavlja izvršavanje do RET instrukcije](#proceed-exec)
| `G` | _addr_ | [Startuje izvršavanje do prekidne tačke](#go-execute)
| `V` | | Ispisuje verziju programa
| `X` | | Izlazak iz komandne linije

## Uputstva za komande programa

### Komande izdate korišćenjem prečice na tastaturi

<h3 id="enter-step-over">ENTER - Izvršavanje tekuće instrukcije (step over režim)</h3>

Pritisak na taster `ENTER` će izvršiti istrukciju na koju pokazuje programski brojač (PC registar). U slučaju da je sledeća instrukcija poziv potprograma, kompletan potprogram će biti izvršen i nova vrednost programskog brojača će pokazivati na sledeću instrukciju posle ovog poziva.

Vrednost registra PC može biti postavljena bilo kada komandama `D from` ili `R PC value`.

> Posle svake instrukcije izvršene pritiskom na taster `ENTER`, ekran trepne jedanput. Možete se zapitati zašto je ovo neophodno? Pa, ovo je neizbežno zato što za svaku, na ovaj način izvršenu instrukciju, GAD kopira sadržaj bafera sistemskog ekrana u video memoriju, izvršava (ili emulira ako je neophodno) instrukciju na koju pokazuje PC registar, kopira nazad sadržaj video memorije u bafer sistemskog ekrana i regeneriše sadržaj svog sopstvenog ekrana.

<h3 id="step-into">. - Izvršavanje tekuće instrukcije sa ulaskom u potprogram (step into režim)</h3>

Ukoliko vrednost programskog brojača pokazuje na instrukciju `CALL`, tj. ukoliko je to poziv potprograma, ova komanda će ući u potprogram i zaustaviti se na prvoj instrukciji tog potprograma. Za sve ostale instrukcije, ova komanda se ponaša jednako kao i komanda za izvršavanje tekuće instrukcije (taster `ENTER`).

<h3 id="continue-execution">; - Nastavak izvršavanja do prekidne tačke</h3>

Ova komanda će nastaviti izvršavanje od tekuće vrednosti registra PC, sve dok ne dostigne sledeću prekidnu tačku ili dođe do kraja programa.

> Ukoliko nema postavljenih prekidnih tačaka, ili se tokom izvršavanja ne dostigne ni jedna od postavljenih prekidnih tačaka, već se dođe do kraja programa, doći će do izlaska iz GAD-a i prepuštanja kontole BASIC editoru. Da bi se ovo izbeglo, u nekim slučajevima je bolje koristiti naredbu za nastavljanje izvršavanja (`P addr`) ili koristiti izvršavanje instrukciju po instrukciju.

<h3 id="search-forward">F - Traženje unapred</h3>

Komanda za traženje unapred se koristi u paru sa komandom `S` koja je prethodno pozvana iz komandne linije. Kao što njeno ime kaže, ova komanda traži sledeće pojavljivanje traženog niza bajtova napred od adrese na kojoj je prethodno pronađen.

### Komande koje se izdaju preko komandne linije

<h3 id="breakpoint-set">B - Postavljanje/uklanjanje/prikazivanje prekidne tačke</h3>

Ova komanda ima više funkcija. Njom se postavlja nova ili uklanja postojeća prekidna tačka (samo po jedna u jednoj komandi), ili se prikazuje vrednost svih prekidnih tačaka. Najviše četiri prekidne tačke mogu postojati istovremeno. Po uspešnom završetku, sve varijacije ove komande, u statusnoj liniji ispisuju vrednosti svih postavjenih prekidnih tačaka.

Komanda za postavljanje prekidne tačke ima sledeći format:

```
B <bp number> <address>
```
gde je _bp number_ broj prekidne tačke (od 1 do 4), a _address_ je adresa na kojoj se prekidna tačka postavlja.

Uklanjanje prekidne tačke je u suštini jednako postavljanju prekidne tačke na adresu nula. Za uklanjanje više prekidnih tačaka, komanda se mora ponoviti za svaki redni broj prekidne tačke.

```
B <bp number> 0
```

PRIMERI:

Sledeći primer postavja prekidnu tačku broj dva na heksadecimalnu adresu 3200. Ostale tri prekidne tačke nisu postavljene.
```
B 2 &3200
1:---- 2:3200 3:---- 4:----
```
Ovaj primer prikazuje kako proveriti koje prekidne tačke su postavljene i prikazuje njihove vrednosti. Posle izdavanja `B` komande bez parametara, GAD ispisuje podatke o svim prekidnim tačkama u statusnoj liniji. Ukoliko prekidna tačka nije postavljena, njena vrednost se prikazuje crticama (`----`).
```
B
1:2F00 2:3200 3:---- 4:----
```

> Adrese manje od `&2C00` nisu dozvoljene za postavljanje prekidnih tačaka (prostor koji zauzima ROM, memorijski mapirana tastatura, video RAM i deo prostora sistemskih promenljivih). Ukoliko korisnik pokuša da postavi prekidnu tačku u ovom opsegu adresa, biće prikazana poruka `SORRY` i komanda neće biti izvršena.

<h3 id="copy-memory">C - Kopiranje memorije</h3>

Kopira odeljak memorije iz jedne memorijske oblasti u drugu.

```
C <start address> <end address> <target address>
```
gde je _start address_ početna adresa odeljka, _end address_ je krajnja adresa odeljka koji se kopira, i _target address_ je početna adresa odredišta na koje se podaci kopiraju.

Ako je _target address_ između _start address_ i _end address_ komanda će kopirati memoriju unazad.

PRIMER:

```
C &3000 &30FF &300A
```
Kopira podake od adrese &3000 do adrese &30FF (uključujući i nju), deset lokacija naviše.

<h3 id="disassemble">D - Disasembliranje memorije</h3>

Disasemblira mašinski kod u asemblerski oblik (mnemonike i operande).
```
D <from address>
```
gde je _from address_ početna adresa disasembliranja, a ona će biti postavljena i kao nova vrednost registra PC, na šta ukazuje znak `>` u prvoj disasembliranoj liniji.

Prozor disasemblirane memorije je namenjen izvršavanju programa instrukciju po instrukciju, ali se može koristiti i za pregled asemblerskog sadržaja memorije sa osnovnim mogućnostima navigacije komandama `/` (napred) i `B` (nazad).

<h3 id="edit-memory">E - Uređivanje memorije</h3>

Ova komanda upisuje heksadecimalni string u memoriju.

```
E <from address> <hex string>
```
gde je _from address_ početna adresa od koje će biti upisan novi sadržaj memorije, a _hex string_ je novi sadržaj memorije u obliku heksadecimalnog stringa sa opcionim početnim znakom `&`, i bez praznih mesta između navedenih heksadecimalnih bajtova.

Dužina heksadecimalnog stringa je ograničena samo veličinom ulaznog bafera. Trenutno je maksimalna dužina cele komande postavljena na 30 karaktera.

<h3 id="fill-memory">F - Popunjavanje memorije</h3>

Ova komanda ispunjava blok memorije vrednošću navedenog bajta. Opšti format je sledeći:

```
F <start address> <end address> <optional byte value>
```
gde je _start address_ adresa prve memorijske lokacije na koju će biti upisana vrednost _byte value_, _end address_ je adresa poslednje lokacije na koju će biti upisana vrednost _byte value_, i _byte value_ je opciona vrednost bajta kojom će blok memorije biti popunjen. Ukoliko _byte value_ nije naveden, memorija će biti popunjena nulama.

U slučaju da komanda bude uspešno izvršena, u liniji za poruke programa se ispisuje poruka `OK`.

PRIMERI:

```
F &3000 &30FF &FF
```
Ova komanda ispunjava memorijske lokacije od adrese &3000 do adrese &30FF (uključujući početnu i kranju adresu) vrednošću &FF.
```
F &4000 &47FF
```
Ovaj primer ispunjava memorijske lokacije &4000 do &47FF nulama.

<h3 id="go-execute">G - 	Startovanje izvršavanja do prekidne tačke</h3>

Startuje izvršavanje od navedene adrese. Format komande je:

```
G <address>
```
gde je _address_ početna adresa od koje će početi izvršavanje debagiranog programa.

> Ukoliko nema postavljenih prekidnih tačaka, ili se tokom izvršavanja ne dostigne ni jedna od postavljenih prekidnih tačaka, već se dođe do kraja programa, doći će do izlaska iz GAD-a i prepuštanja kontole BASIC editoru. Da bi se ovo izbeglo, u nekim slučajevima je bolje koristiti naredbu za nastavljanje izvršavanja (`P addr`) ili koristiti izvršavanje instrukciju po instrukciju.

<h3 id="memory-dump">M - Ispisivanje sadržaja memorije</h3>

Prikazuje heksadecimalne vrednosti navedenog bloka memorije u rezervisanom ekranskom prostoru. Prikazani memorijski prozor može biti skrolovan strelicama gore-dole.

```
M <from address>
```
gde je _from address_ početna adresa prikazane memorije.

<h3 id="proceed-exec">P - Nastavljanje izvršavanja</h3>

Nastavlja izvršavanje do RET instrukcije.

```
P <address>
```
gde je _address_ početna adresa od koje će izvršavanje krenuti. Po izvršenoj RET instrkciji, izvršavanje se prekida i kontrola se vraća nazad GAD-u.

<h3 id="set-register">R - Postavljanje vrednosti registra ili registarskog para</h3>

Ova komanda postavlja vrednost bilo kog registra (8-bitnog) ili registarskog para (16-bitnog), osim registara `I` i `R`. Format komande je:

```
R <register> <value>
```
gde je _register_ naziv registra (na primer `A` ili `BC`), a _value_  je nova vrednost registra dužine jedan ili dva bajta (kod promene vrednosti registarskog para). Po izvršenju, registarski pregled na gornjem delu ekrana će biti ažuriran tako da prikazuje novu vrednost registra.

> Usled ograničenja koju nameće arhitektura Galaksije za korišćenje IY i (u nešto manjoj meri) IX registara od strane korisničkog programa, vrednosti IY i IX registara se trenutno ne mogu promeniti `R` komandom! Promene nad IY i IX registrima će biti prikazane u području ekrana rezervisanom za prikaz registara, ali promenjene vrednosti neće biti upisane i u same registre.

PRIMERI:

Sledeći primer prikazuje upis heksadecimalne vrednosti `FF` u registar `A`.
```
R A &FF
```
U sledećem primeru registru `HL` se dodeljuje decimalna vrednost `1234`.
```
R HL 1234
```
Ovaj primer prikazuje da i vrednost programskog brojača takođe može biti promenjena komandom `R`.
```
R PC &4000
```
<h3 id="search-memory">S - Pretraživanje memorije</h3>

Komanda za pretraživanje memorije traži zahtevani niz bajtova u navedenom memorijskom opsegu. Ukoliko ga pronađe, početna adresa prozora za prikaz memorije se postavlja na adresu na kojoj počinje traženi niz bajtova. Pretraživanje sledećih pojavljivanja istog niza bajtova se može nastaviti pritiskom na taster `F` (pretraživanje unapred). Ukoliko traženi niz bajtova nije pronađen, ili nema više naknadnih pojavljivanja, u liniji za poruke programa se ispisuje `END` poruka.

Sintaksa komande za pretraživanje memorije je:

```
S <start address> <end address> <hex string>
```
gde je _start address_ početna, a _end address_ krajnja adresa bloka memorije koji se pretražuje i _hex string_ je traženi niz bajtova napisan kao heksadecimalni string sa opcionim znakom `&` na početku, i bez praznih mesta između heksadecimalnih bajtova.

Dužina traženog niza je ograničena samo veličinom ulaznog bafera. Trenutno je maksimalna dužina cele komande 30 karaktera.

> Između uzastopnih pretraživanja `S` i `F` komandama, traženi string se čuva u ulaznom baferu komandne linije. To znači, da će uzastopna pretraživanja pravilno funkcionisati samo ukoliko se u međuvremenu iz komandne linije ne startuju nove komande, zato što bi njihov sadržaj uništio sadržaj traženog stringa koji se nalazi u ulaznom baferu.

## Ograničenja

Naizmenično izvršavanje dva programa na platformi koja nema mogućnost zaštite pristupa memoriji svakako nameće neka pravila dobrog ponašanja za oba ova programa. Ipak, postoje i neka, ne tako očigledna ograničenja, i ona će biti ovde navedena.

### Implementacija prekidne tačake

Prekidna tačka u programu GAD je implementirana jednostavno kao `CALL` mašinska instrukcija dužine tri bajta. Kada se izvrši, ova instrukcija poziva rutinu za obradu prekidnih tačaka. Umetanjem na adresu prekidne tačke, ova instrukcija preklapa, od jedne instrukcije (dužine tri bajta), do najviše tri instrukcije (dužine po jedan bajt), originalnog koda programa koji se debagira. Zavisno od sadržaja debagiranog programa, kao i putanje izvršavanja, u nekim retkim slučajevima se može desiti da izvršavanje preskoči adresu na kojoj je postavljena prekidna tačka, ali tako da nastavi izvršavanje od drugog ili trećeg bajta dela memorije koji je zamenjen `CALL` instrukcijom. U ovom slučaju dolazi do nepredviđenog ponašanja procesora i najverovatnijeg kraha sistema (ovo bi se moglo izbeći ukoliko bi na Galaksiji postojao `RST` vektor koji se može koristiti od strane korisnika računara, pošto su `RST` instrukcije dužine od samo jednog bajta, ali ova mogućnost ovde, nažalost, ne postoji).

### Prekidne tačke u petlji

Kada izvršavanje debagiranog programa prvi put stigne do prekidne tačke koja se nalazi u bilo kakvoj vrsti petlje, ono će se pravilno zaustaviti i izvršavanje će preći na prekidnu rutinu, a prikazaće se standardni GAD ekran. Ukoliko se zatim nastavi izvršavanje (na primer pritiskom `;` tastera), u nadi da će se zaustaviti na istoj instrukciji u sledećoj iteraciji petlje, ovo se neće dogoditi, već će se izvršavanje nastaviti dalje kao da prekidna tačka više ne postoji.

Razlog za ovakvo ponašanje je da izvršavanje ne može da se zaustavi na prekidnoj tački postavljenoj na tekuću adresu (onoj od koje i počinje), jer bi inače kontrola prešla odmah na rutinu za obradu prekidnih tačaka, umesto nastavka izvršavanja sledeće iteracije petlje.

Rešenje za ovakvu situaciju je da se prvo izvrši jedna instrukcija (ili više njih ako je ta instrukcija kraća od tri bajta) pritiskom na taster `ENTER`, pa da se tek onda nastavi izvršavanje pritiskom na taster `;`.

## Licenca

The MIT License (MIT)

Copyright (c) 2024 Vitomir Spasojević (https://github.com/DigitalVS/GAD). Sva prava zadržana.
