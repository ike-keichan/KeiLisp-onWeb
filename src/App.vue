<template>
    <div id="app">
        <h4>
            Hello! This is an interpreter that mimics Lisp, "KeiLisp".<br>
            2020.12.04 created by Keisuke Ikeda.
        </h4>
        <pre>{{ output }}{{ prompt }}<input id="clinput" name="clinput" type="text" v-model="input" autocomplete="off"></pre>
        <textarea @focus="messagePrint" id="Message" cols="1" rows="1" readonly></textarea>
        <textarea @focus="clearPrint" id="Clear" cols="1" rows="1" readonly></textarea>
    </div>
</template>

<script>
import { LispInterpreter } from './lib/KeiLisp/LispInterpreter.js'
import { Cons } from './lib/KeiLisp/Cons.js'

export default {
    name: 'App',
    data() {
        return {
            interpreter: {},
            prompt: '>>   ',
            input: '',
            output: '',
            buffer: '',
            aCons: '',
            history: [],
            index: 0,
            leftParentheses: 0,
        }
    },
    created: function() {
        this.interpreter = new LispInterpreter()
    },
    mounted: function() {
        this.addEvent();
    },
    methods: {
        exec () {
            this.buffer += ' ';

            for(let aCharacter of this.input)
            {
                if(aCharacter == '(') { this.leftParentheses++; }
                if(aCharacter == ')') { this.leftParentheses--; }
                this.buffer += aCharacter;
            }

            this.print(this.prompt + this.input + '\n');

            if(this.leftParentheses > 0){ this.prompt = '. . .  ' }
            else{ this.prompt = '>>   ' }

            if(this.leftParentheses <= 0)
            {
                this.aCons = this.interpreter.parse(this.buffer);
                try
                {
                    for(let each of this.aCons.loop()){ this.print(this.interpreter.eval(each).toString() + '\n'); }
                }
                catch (e) 
                {
                    this.print('*** can not eval ' + this.aCons.toString() + ' ***\n');
                    this.print(Cons.nil.toString() + '\n');
                }

                this.leftParentheses = 0;
                this.buffer = ''; 
            }

            if(this.input != ''){ this.history.push(this.input); }
            this.input = ' ';
            this.index = this.history.length;
        },
        addEvent(){
            document.getElementById("clinput").focus()
            document.getElementsByTagName('html')[0].onclick = () => {
                document.getElementById("clinput").focus()
            }
            document.getElementById("clinput").addEventListener('keypress', (e) => {
                const key = e.keyCode || e.charCode || 0;
                if (key == 13) {
                    this.exec();
                    e.preventDefault();
                }
            }, false)
            document.onkeydown = (key) => {
                if (key.keyCode == '38')
                {
                    if(0 < this.index){ this.index --; }
                    else if(this.index <= 0){ this.index = 0; }
                    if(this.history[this.index] != undefined){ this.input = this.history[this.index]; }
                }
                else if (key.keyCode == '40')
                {
                    if(this.index < this.history.length){ this.index ++; }
                    else if(this.history.length  < this.index){ this.index = this.history.length; }
                    if(this.history.length == this.index){ this.input = ''; }
                    else if(this.history[this.index] != undefined){ this.input = this.history[this.index]; }
                }
            }
        },
        clearPrint() {
            this.output = '';
        },
        messagePrint() {
            this.print(document.getElementById('Message').value)
            document.getElementById('Message').value = '';
        },
        print(aString) {
            this.output += aString;
        }
    }
}
</script>
<style>
#app {
    font-size: 1.17em;
    font-weight: bold;
    font-family: Ricty Diminished Discord;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    text-align: start;
}

#clinput {
    width: 95%;
}

.contents {
    height: 100%;
    width: 100%;
}

html {
    height: 100vh;
}

button {
    display: none;
}

input, textarea {
    border:none;
    font-size : 100%;
    font-weight: inherit;
    font-family : inherit;
    outline:none;
    resize: none;
}

textarea {
    display: block;
}

@media (prefers-color-scheme: dark) {
    html, input, textarea {
        background-color: #000;
        color: #0066cc;
    }
}
</style>
