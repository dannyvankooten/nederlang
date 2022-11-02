// Output: 9227465
stel fibonacci = functie(n) {
    stel a = 0;
    stel b = 1;
    stel c = a + b;
    stel i = 2;

    zolang i < n {
        i = i + 1;
        a = b;
        b = c;
        c = a + b;
    }

    antwoord c;
}

fibonacci(35);