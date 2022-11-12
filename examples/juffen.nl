// Juffen, knoesten of hoppen
// Spelers tellen samen van 1 tot 100
// Elk getal deelbaar door 7 (7, 14, 21...) of met een 7 (7, 17) wordt vervangen door "juf!"

// Functie om te checken of n een bepaald getal bevat
functie bevat_getal(n, getal) {
    zolang n > 0 {
        als n % 10 == getal {
            antwoord ja
        }

        n /= 10
    }

    nee
}

// Print alle getallen van 1 tot 100, of "juf".
stel n = 1;
zolang n < 100 {
    als n % 7 == 0 || bevat_getal(n, 7) {
        print("Juf!")
    } anders {
        print(n)
    }

    n += 1
}