# Nederlang 

Nederlang is een geïnterpreteerde programmeertaal, in het Nederlands! Met als bestandsnaam extensie.... `.nl`! Het maakt gebruik van dynamische types en is geïnspireerd door talen als JavaScript, Python en C.  

De Nederlangcode wordt gecompileerd naar bytecode en dan in een virtuele machine uitgevoerd. De compiler en virtuele machine zijn geschreven in Rust.

Dit hele project is natuurlijk een grapje en vooral bedoeld om van te leren. Het doel is om een taal neer te zetten die compleet genoeg is om [Advent of Code](https://adventofcode.com/) mee te kunnen doen.

## Spelen

Je kunt Nederlang direct vanuit je browser proberen in de [Nederlang speeltuin](https://dannyvankooten.github.io/nederlang/playground/). 


## Installeren

Om Nederlang op je computer te installeren moet je voorlopig nog even zelf builden vanaf de source (met [cargo](https://www.rust-lang.org/learn/get-started)).

```
git clone git@github.com:dannyvankooten/nederlang.git
cd nederlang
cargo build --release
```

Na het uitvoeren van bovenstaande stappen staat de executable in `target/release/nederlang`. Om deze op ieder pad beschikbaar te hebben kan je deze (op Unix systemen) kopiëren naar `/usr/local/bin`.

```
cp target/release/nederlang /usr/local/bin/nederlang
```


### Types

Nederlang heeft eersteklas ondersteuning voor de volgende types:

- Null
- Booleans (`ja` / `nee`)
- Floats
- Integers
- Strings
- Arrays    
- ~~Maps~~
- ~~Sets~~

### Expressies

De standaard wiskundige rekenregels gelden voor expressies:

```
1 + 1 * 2 - 3 / 3 % 2   // => 0
!ja                     // => nee
1 > 5                   // => nee
1 > 5 || 5 > 1          // => ja
(1 > 2 && 2 > 1) || ja  // => ja
```

### Variabelen

Variabelen worden gedeclareerd met `stel`:

```
stel x = 100
```

Verwijzen naar een niet-gedeclareerde variabele resulteert in een fout:

```
z + 100             // VerwijzingsFout: z is niet gedeclareerd
```

Variabelen zijn gelimiteerd tot de reikwijdte van het blok of de functie waarin ze worden gedeclareerd.

```
stel x = 100
{
    stel y = 100
    y + x           // => 200
}                   
y                   // VerwijzingsFout: y is niet gedeclareerd
x                   // => 100
```

Eenmaal gedeclareerde variabelen kunnen een nieuwe waarde krijgen toegewezen.

```
stel x = 1
x = 2
x                   // => 2
```

#### Syntactische suiker

Om een variabele een nieuwe waarde toe te wijzen in relatie tot zichzelf kan je in plaats van de volledige vorm:

```
a = a + 5
a = a * 100
// etc..
```

Ook het volgende schrijven:

```
a += 5
a *= 100
// etc..
```


### Functies

Functies declareer je in de volgende vorm: `functie <naam?> (<parameter?>, ...) { <korpus> }`

```
functie optellen(a, b) { 
    a + b 
}
optellen(2, 3)      // => 5
```

Alternatief kun je functies ook declareren in de volgende vorm:

```
stel optellen = functie(a, b) { a + b }
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

Functies zijn geldige waardes in Nederlang en kan je dus als parameter meegeven aan een andere functie:

```
functie opteller(a, b) {
    a + b
}

functie bereken(f, a, b) {
    f(a, b)
}

bereken(opteller, 2, 3)     // => 5
```

#### Voorwaardelijke stellingen

```
als 1 + 1 == 2 {
    print("Ja, 1 + 1 is echt 2!")
} anders {
    print("Mijn hele leven blijkt een grote leugen.")
}
```

#### Lussen

```
stel aantal = 5
zolang aantal > 0 {
    aantal -= 1
}
```

Om onder voorwaarde uit een lus te stappen gebruik je `stop`:

```
stel aantal = 1
zolang ja {
    aantal += 1

    als aantal == 100 {
        stop
    }
}
aantal                      // => 100
```

Om vroegtijdig naar een volgende iteratie van een lus te gaan gebruik je `volgende`:

```
stel n = 0
zolang n < 10 {
    als n % 2 != 0 {
        volgende
    }

    // n is een even getal
    // print(n)

    n += 1
}
```

### Ingebouwde functies

Nederlang komt met enkele ingebouwde functies die overal beschikbaar zijn.

`print` om iets naar de standaard uitvoer te schrijven.

```
print("Hey {}. Je bent nummer {} die dit echt leest.", "jij", 1337)
``` 

`bool` om een object naar een boolean om te zetten:

```
bool(1)             // ja
```

`int` om een object naar een geheel getal om te zetten:

```
int("15")           // 15
```

`float` om een object naar een decimaal getal om te zetten:

```
float("3.1415")     // 3.1415
```

`string` om een object naar tekst om te zetten:

```
string(1)           // "1"
```



## Licentie

GPLv3, voor nu.
