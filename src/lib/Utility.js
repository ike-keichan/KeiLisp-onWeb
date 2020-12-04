// #!/usr/bin/env node

'use strict';

global.console;
global.document;
global.module;
global.print;
global.window;

/**
 * 適切なプリントアウトを行う関数を、実行状況から選び出して、それを応答する関数
 */
export function selectPrintFunction()
{
    let aFunction = null;
 
    if (typeof document === "undefined"){
        if (typeof print === "function") { aFunction = (anObject) => { print(anObject); }; }
        else { aFunction = (anObject) => { console.log(anObject); }; }
    } else {
        aFunction = (anObject, end = '\n') => {
            let anElement = document.getElementById("Message");
            anElement.value += anObject + end; 
            anElement.focus();
            anElement.blur();
        };
    }
 
    return aFunction;
}
/**
 * 出力をクリアする関数
 */
export function clearPrintFunction()
{
    let anElement = document.getElementById("Clear");
    anElement.focus();
    anElement.blur();
 
    return;
}
