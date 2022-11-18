// Nederlang implementatie van het selection sort algorithme
// https://en.wikipedia.org/wiki/Selection_sort
functie sorteer(a) {
    stel i = 0;
    zolang i < lengte(a) {

        // vind de index van de minimale waarde in de array
        stel min = i;
        stel j = i + 1;
        zolang j < lengte(a) {
            als a[j] < a[min] {
                min = j;
            }
            j += 1;
        }

        // wissel met huidige positie
        stel tmp = a[i]
        a[i] = a[min]
        a[min] = tmp

        i += 1;
    }
    a
}

stel getallen = [ 5, 2, 1, 9, 8, 4, 6, 10, 7, 3]
stel gesorteerd = sorteer(getallen)
print(gesorteerd)