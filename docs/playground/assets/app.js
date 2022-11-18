const editor = ace.edit("nl-editor");
editor.setTheme("ace/theme/nord_dark");

const myWorker = new Worker(window.location.pathname + 'assets/worker.js');
const executeButton = document.getElementById('nl-execute');
const resultElement = document.getElementById('nl-result');

window.addEventListener('load', function () {
    if (window.location.hash.startsWith('#code=')) {
        let code = window.location.hash.substring("#code=".length);
        code = window.decodeURIComponent(code);
        editor.setValue(code);
        window.location.hash = '';
    } else {
        const preset = window.location.hash.startsWith('#preset') ? window.location.hash.substring("#preset=".length) : 'voorbeeld';
        const code = code_presets[preset];
        editor.setValue(code);
    }
})

myWorker.onmessage = (e) => {
    let [success, message, elapsed_time] = e.data;
    if (success) {
        message = message.replaceAll("\n", "<br />");
        message = `<div><strong>Resultaat: </strong> ${message}</div>`
        message += `<div class="m muted">Programma was bezig voor <strong>${elapsed_time}</strong> ms.</div>`
        resultElement.innerHTML = message;
    } else {
        const pos = message.indexOf(':');
        message = '<strong>' + message.substring(0, pos) + ':</strong> ' + message.substring(pos + 1);
        resultElement.innerHTML = message;
    }

    executeButton.disabled = false;
    executeButton.classList.remove('busy');
}

executeButton.addEventListener('click', function (evt) {
    evt.preventDefault();

    resultElement.innerHTML = '<div class="m muted">Bezig...</div>';
    executeButton.disabled = true;
    executeButton.classList.add('busy');

    const code = editor.getValue();
    myWorker.postMessage(code);
})

document.getElementById('nl-share').addEventListener('click', function (evt) {
    evt.preventDefault();
    let code = editor.getValue();
    window.location.hash = 'code=' + window.encodeURIComponent(code);
    window.prompt("You can share this code snippet using the following URL", window.location);
})

document.getElementById('nl-preset').addEventListener('change', function () {
    const code = code_presets[this.value];
    editor.setValue(code);
})

const code_presets = {
    // Sample program with a mix of sytnax and language constructs
    'voorbeeld': `
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

stel b = als (nee || ja) { 1 } anders { 2 }

als b == 2 {
    b = b + 1;
}

zolang b > 0 {
    b -= 1

    als b == 1 {
        stop
    }
}

b`,
    // Recursive fibonacci 
    'fib-rec': `functie fib(n) {
    als n < 2 { 
        antwoord n 
    } 
    
    fib(n - 1) + fib(n - 2) 
} 

fib(35)
`,

    // Fibonacci with looping
    'fib-loop': `functie fibonacci(n) {
    stel a = 0;
    stel b = 1;
    stel c = a + b;
    stel i = 2;

    zolang i < n { 
        i += 1; 
        a = b; 
        b = c; 
        c = a + b; 
    } 
    
    antwoord c; 
} 
fibonacci(35);
`,

    // Project Euler #1
    'euler-1': `// https://projecteuler.net/problem=1

functie probleem_1() {
    stel som = 0
    stel i = 1

    zolang i < 1000 {
        als i % 3 == 0 || i % 5 == 0 {
            som += i
        }

        i += 1
    }

    antwoord som
}

probleem_1()
`,
    'juffen': `
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
}`,
    'selectie-sorteer': `
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
`

}