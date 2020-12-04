<template>
    <div id="app">
        <h4 style="text-align: start">
            Hello! This is an interpreter that mimics Lisp, "KeiLisp".<br>
            2020.12.04 created by Keisuke Ikeda.
        </h4>
        <textarea name="" id="Message" cols="30" rows="10" style="display:none;"></textarea>
        <form name="clform" style="text-align: start;" autocomplete="off">
<pre>{{ output }}{{ prompt }}<input id="clinput" name="clinput" style="border:none; outline:none;" type="text" v-model="input">
</pre>
            <button style="display: none" v-on:click.prevent="exec"></button>
        </form>
        <p id="Flag" style="display:none;"></p>
    </div>
</template>

<script>
// import HelloWorld from './components/HelloWorld.vue'
import { LispInterpreter } from './lib/JSLisp/LispInterpreter.js'
import { Cons } from './lib/JSLisp/Cons.js'

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
                    for(let each of this.aCons.loop()){ 
                        const result = this.interpreter.eval(each).toString() + '\n';
                        if (this.hasMessage()){ this.messagePrint(); }
                        this.print(result);
                        this.hasFlag();
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
        hasFlag(){
            if(document.getElementById('Flag').value == true)
            {
                this.output = '';
                document.getElementById('Flag').value = false;
            }
        },
        hasMessage () {
            return document.getElementById('Message').value.length > 0
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
    text-align: center;
}

#clinput {
    width: 95%;
}

html {
    height: 100vh;
}

input {
    font-family : inherit;
    font-weight: inherit;
    font-size : 100%;
}

@media (prefers-color-scheme: dark) {
    html {
        background-color: #000;
        color: #0066cc;
    }
    input {
        background-color: #000;
        color: #0066cc;
    }
}
</style>
