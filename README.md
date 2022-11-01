# Nederlang 

Nederlang is een geinterpreteerde programmeertaal, in het Nederlands! Met als bestandsnaam extensie.... `.nl`! 

Het maakt gebruik van dynamische types en is geinspireerd door talen als JavaScript, Python en C.  Hieronder vind je een voorbeeld programma in Nederlang.

```
functie kwadraat(x) {
    x * x
}

functie optellen(a, b) {
    a + b
}

functie vermenigvuldigen(a, b) {
    a * b
}

// Geeft het overblijfsel wanneer a door b gedeeld wordt
functie overblijfsel(a, b) {
    a % b
}

functie is_even(a) {
    als overblijfsel(a, 2) == 0 {
        ja
    } anders {
        nee
    }
}

functie is_oneven(a) {
    antwoord ! is_even(a)
}

stel a = 10
stel b = 20
stel resultaat = kwadraat(a) + kwadraat(b) - 100 / 5;
print(resultaat) // Print 480

// Blijf doen tot resultaat 0 is
zolang resultaat > 0 {
    resultaat = resultaat - 1
}
print(resultaat) 

// Lijst
stel getallen = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
getallen[0] = 10;
print(getallen[0])

// Bereken het zoveelste (n) fibonacci getal
// Merk op dat functies gebruikt kunnen worden als waarde.
stel fib = functie(n) {
    als n < 2 {
        antwoord n
    } 

    fib(n - 1) + fib(n - 2)
}
fib(15)
```

De Nederlang code wordt omgezet naar bytecode en dan in een virtual machine uitgevoerd. De compiler en virtual machine zijn geschreven in Rust.

Dit hele project is natuurlijk een grapje en vooral bedoeld om van te leren. Het doel is om een taal neer te zetten die compleet genoeg is om [Advent of Code](https://adventofcode.com/) mee te kunnen doen.

### License

GPLv3, voor nu.