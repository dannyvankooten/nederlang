# Nederlang 

Nederlang is een geinterpreteerde programmeertaal, in het Nederlands! Met als bestandsnaam extensie.... `.nl`! Het maakt gebruik van dynamische types en is geinspireerd door talen als JavaScript, Python en C.  

De Nederlang code wordt gecompileerd naar bytecode en dan in een virtual machine uitgevoerd. De compiler en virtual machine zijn geschreven in Rust.

Dit hele project is natuurlijk een grapje en vooral bedoeld om van te leren. Het doel is om een taal neer te zetten die compleet genoeg is om [Advent of Code](https://adventofcode.com/) mee te kunnen doen.

#### Types

Nederlang heeft first class support voor de volgende types:

- Null
- Booleans (`ja` / `nee`)
- Floats
- Integers
- ~~Strings~~ 
- ~~Arrays~~
- ~~Maps~~
- ~~Sets~~

#### Expressies

De standaard wiskundige rekenregels gelden voor expressies:

```
1 + 1 * 2 - 3 / 3 % 2   // => 0
!ja                     // => nee
1 > 5                   // => nee
1 > 5 of 5 > 1          // => ja
(1 > 2 en 2 > 1) of ja  // => ja
```

#### Variabelen

Variabelen worden gedeclareerd met `stel`:

```
stel x = 100
```

Verwijzen naar een niet-gedeclareerde variabele resulteert in een fout:

```
x + 100             // ReferenceError: x is niet gedeclareerd
```

Variabelen zijn gelimiteerd tot de scope van het blok waar ze in worden gedeclareerd.

```
stel x = 100
{
    stel y = 100
    y + x           // => 200
}
y                   // ReferenceError: y is niet gedeclareerd
x                   // => 100
```

Eenmaal gedeclareerde variabelen kunnen een nieuwe waarde krijgen.

```
stel x = 1
x = 2
x                   // => 2
```

#### Functies

Functies zijn geldige waardes in Nederlang en kunnen dus als argument aan een andere functie worden meegegeven.

```
stel optellen = functie(a, b) { a + b }
optellen(2, 3)      // => 5
```

Alternatief kun je functies ook declareren in de volgende vorm:

```
functie optellen(a, b) {
    a + b
}
optellen(2, 3)      // => 5
```

De laatste waarde in een functie wordt automatisch als antwoord aan de aanvrager teruggegeven. Alternatief kan je ook eerder antwoorden met `antwoord <expressie>`.

```
functie fibonacci(n) {
    als n < 2 {
        antwoord n
    }

    fibonacci(n - 1) + fibonacci(n - 2)
}
```

#### Voorwaardelijke stellingen

```
als 1 + 1 == 2 {
    print("Ja, 1 + 1 is echt 2!")
} anders {
    print("Mijn hele leven blijkt een grote leugen.")
}
```

#### Loops

```
stel aantal = 5
zolang aantal > 0 {
    aantal = aantal - 1
}
```

### Installeren

Om Nederlang op je computer te installeren moet je voorlopig nog even zelf builden vanaf de source (met [cargo](https://www.rust-lang.org/learn/get-started)).

```
git clone git@github.com:dannyvankooten/nederlang.git
cd nederlang
cargo build --release
```

Na het uitvoeren van bovenstaande stappen staat de executable in `target/release/nederlang`. Om deze system-wide beschikbaar te hebben kan je deze (op Unix systemen) verplaatsen naar `/usr/local/bin`.

```
cp target/release/nederlang /usr/local/bin/nederlang
```

### License

GPLv3, voor nu.