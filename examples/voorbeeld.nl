// Variabelen declareer je met "stel"
stel a = 1;

// Eenmaal gedeclareerde variabelen kun je een nieuwe waarde toeschrijven:
a = 2;

// Functies:
functie is_even(n) {
    n % 2 == 0
}

als is_even(2) {
    10
    // print("Ja, 2 is een even getal!");
} 

stel b = als nee { 1 } anders { 2 }

als b == 2 {
    b += 1
}

zolang b > 0 {
    b -= 1

    als b == 1 {
        stop
    }
}

b