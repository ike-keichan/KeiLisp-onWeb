<template>
    <div id="app">
        <h4>
            Hello! This is an interpreter that mimics Lisp, "KeiLisp".<br>
            2020.12.04 created by Keisuke Ikeda.
        </h4>
        <form name="clform" autocomplete="off">
            <pre>{{ output }}{{ prompt }}<input id="clinput" name="clinput" type="text" v-model="input">
            </pre>
            <button style="display:none" v-on:click.prevent="exec"></button>
        </form>
        <textarea @focus="messagePrint" id="Message" cols="1" rows="1" style="display:block;" readonly></textarea>
        <p id="ClearFlag" style="display:none;"></p>
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
            prompt: '>> ',
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
        document.clform.clinput.focus()
        document.getElementsByTagName('html')[0].onclick = () => {
            document.clform.clinput.focus()
        }
        document.onkeydown = (key) => {
            if (key.keyCode == '38') {
                if(0 < this.index){ this.index --; }
                else if(this.index <= 0){ this.index = 0; }
                if(this.history[this.index] != undefined){ this.input = this.history[this.index]; }
            }
            else if (key.keyCode == '40') {
                if(this.index < this.history.length){ this.index ++; }
                else if(this.history.length  < this.index){ this.index = this.history.length; }
                if(this.history.length == this.index){ this.input = ''; }
                else if(this.history[this.index] != undefined){ this.input = this.history[this.index]; }
            }
        }
    },
    methods: {
        exec () {
            this.buffer += '';

            for(let aCharacter of this.input)
            {
                if(aCharacter == '(') { this.leftParentheses++ }
                if(aCharacter == ')') { this.leftParentheses-- }
                this.buffer += aCharacter;
            }

            this.print(this.prompt + this.input + '\n');

            if(this.leftParentheses <= 0)
            {
                this.aCons = this.interpreter.parse(this.buffer);
                try
                {
                    for(let each of this.aCons.loop())
                    { 
                        this.print(this.interpreter.eval(each).toString() + '\n');
                        this.clearPrint();
                    }
                }
                catch (e) 
                {
                    this.print('*** can not eval ' + this.aCons.toString() + ' ***\n');
                    this.print(Cons.nil.toString() + '\n');
                }

                this.leftParentheses = 0;
                this.buffer = new String(); 
            }
            if(this.input != ''){ this.history.push(this.input); }
            this.input = '';
            this.index = this.history.length
        },
        clearPrint() {
            if(document.getElementById('ClearFlag').value == true)
            {
                this.output = '';
                document.getElementById('ClearFlag').value = '';
            }
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

html {
    height: 100vh;
}

input, textarea {
    border:none;
    outline:none;
    font-family : inherit;
    font-weight: inherit;
    font-size : 100%;
    resize: none;
}



@media (prefers-color-scheme: dark) {
    html {
        background-color: #000;
        color: #0066cc;
    }
    input, textarea {
        background-color: #000;
        color: #0066cc;
    }
}
</style>
