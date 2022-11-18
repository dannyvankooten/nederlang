// Variabelen declareer je met "stel"
stel a = 1;

// Eenmaal gedeclareerde variabelen kun je een nieuwe waarde toeschrijven:
a = 2;

// Functies:
functie is_even(n) {
    n % 2 == 0
}

// Voorwaardelijke stellingen
als is_even(2) {
    print("Ja, 2 is een even getal!");
} 

stel b = als nee { 1 } anders { 2 }

// Lussen
zolang b > 0 {
    b -= 1

    als b == 1 {
        stop
    }
}
print("b = {}", b)

// Karakters in een string vervangen
stel tekst = "🇳🇱💖";
tekst[-1] = "🏆"
print(tekst)

// Lijsten
stel c = [1, 2, 3];
c[0] = c[-1];
print(c)