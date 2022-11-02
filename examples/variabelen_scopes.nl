stel a = 1;
stel b = a + 1;
stel c = b + a + 1;

stel ant = (functie(a, b) {
    stel fn = functie(a) {
        a * 2
    }
    a * fn(b)
})(5, 8)
ant