const editor = ace.edit("nl-editor");
editor.setTheme("ace/theme/nord_dark");

const myWorker = new Worker(window.location.pathname + 'assets/worker.js');
const executeButton = document.getElementById('nl-execute');
const resultElement = document.getElementById('nl-result');

window.addEventListener('load', function () {
    if (window.location.hash.startsWith('#code=')) {
        let code = window.location.hash.substring("#code".length + 1);
        code = window.decodeURIComponent(code);
        editor.setValue(code);
        window.location.hash = '';
    } else {
        const code = code_presets['voorbeeld'];
        editor.setValue(code);
    }
})

myWorker.onmessage = (e) => {
    let [success, message, elapsed_time] = e.data;
    if (success) {
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

stel b = als nee { 1 } anders { 2 }

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
`

}