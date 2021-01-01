// #!/usr/bin/env node

'use strict';

// ライブラリ「Ramda」を読み込む。
import * as R from 'ramda'; 

// モジュール「Cons」を読み込む。
import { Cons } from './Cons.js';

// モジュール「Evaluator」を読み込む。
import { Evaluator } from './Evaluator.js';

//モジュール「InterpretedSymbol」を読み込む。
import { InterpretedSymbol } from './InterpretedSymbol';

// モジュール「StreamManager」を読み込む。
import { StreamManager } from './StreamManager.js';

// モジュール「Table」を読み込む。
import { Table } from './Table.js';

// モジュール「selectPrintFunction」を読み込む。
import { selectPrintFunction} from '../Utility.js'

global.console;
global.document;
global.module;
global.print;
global.window;

/**
 * @class
 * @classdesc canvas2Dを用いてLispでグラフィック処理を可能にしたクラス
 * @author Keisuke Ikeda
 * @this {Graphist}
 */
export class Graphist extends Object
{
    /**
     * Lispの関数とJSの関数を紐づけるテーブル
     */
    static buildInFunctions = Graphist.setup();

    /**
     * コンストラクタメソッド
     * @constructor
     * @param {Table} aTable 環境のテーブル（予約語）
     * @param {StreamManager} aStreamManager
     * @param {Number} aNumber 呼び出しの深さ
     * @return {Evaluator} 自身
     */
    constructor(aTable, aStreamManager, aNumber)
    {
        super();
        this.environment = aTable;
        this.streamManager = aStreamManager;
        this.depth = aNumber;
        this.canvas = document.querySelector("#glCanvas");
        this.ctx = this.canvas.getContext("2d");
        this.canvasWidth = document.getElementById("glCanvas").clientWidth;
        this.canvasHeight = document.getElementById("glCanvas").clientHeight;
        this.canvasState = 'true' === document.getElementById("canvasState").value ? true : false;

        return this;
    }

    /**
     * 引数と値を束縛するメソッド
     * @param {Cons} parameter 値
     * @param {Cons} args 引数
     * @return {Null} 何も返さない。
     */
    binding(parameter, args)
    {
        if(Cons.isNil(parameter)){ return null; }
        let aCons = parameter;
        let theCons = args;
        
        while(Cons.isNotNil(aCons))
        {
            try { this.environment.set(aCons.car, theCons.car); }
            catch(e) { selectPrintFunction()('sizes do not match.'); return null; }

            if(Cons.isNotCons(aCons.cdr)){ break; }
            aCons = aCons.cdr;
            theCons = theCons.cdr;
        }

        if(Cons.isNotList(aCons.cdr) && (Cons.isNotNil(aCons.cdr)))
        {
            try { this.environment.set(aCons.cdr, theCons.cdr); }
            catch(e) { selectPrintFunction()('sizes do not match.'); return null; }
        }else if(Cons.isNotNil(aCons.cdr)){ throw new Error('Can not binding value to "' + aCons.cdr() + '"'); }

        return null;
    }

     /**
     * 組み込み関数を実行し、結果を応答するメソッド
     * @param {InterpretedSymbol} procedure 関数名、又はオペレータ
     * @param {Cons} args 引数
     * @return {*} 計算結果
     */
    buildInFunction(procedure, args)
    {
        let answer = Cons.nil;
        let methodName = new String();

        if(this.isSpy(procedure))
        {
            this.spyPrint(this.streamManager.spyStream(procedure), (new Cons(procedure, args)).toString());
            this.setDepth(this.depth + 1);
        }

        methodName = Graphist.buildInFunctions.get(procedure);

        try 
        { 
            let method = this[methodName];
            ((x) => {x})(method); // 何もしない。 
        }
        catch(e){ selectPrintFunction()('Not Found Method: ' + methodName); }

        answer = R.invoker(1, methodName)(args, this); 

        if(this.isSpy(procedure))
        {
            this.setDepth(this.depth - 1);
            this.spyPrint(this.streamManager.spyStream(procedure), answer + ' <== ' + new Cons(procedure, args));
        }
       
        return answer;
    }

    /**
     * Webブラウザがcanvas2Dをサポートしているかを確認するメソッド
     * @return {Boolean} 真偽値
     */
    checkSupport() 
    {
        if (this.ctx === null)
        {
            selectPrintFunction()('Unable to initialize canvas. The browser or machine may not support it.');
            return false;
        }

        return true;
    }

    /**
     * Graphistを実行するメソッド
     * @param {*} procedure 関数名、又はオペレータ
     * @param {*} args 引数の値
     * @param {Table} environment 環境のテーブル（予約語）
     * @param {StreamManager} aStreamManager 
     * @param {Number} depth 呼び出しの深さ
     * @return {*} 計算結果
     */
    static draw(procedure, args, environment, aStreamManager = new StreamManager(), depth = 1)
    {
        return new Graphist(environment, aStreamManager, depth).draw(procedure, args);
    }
    
     /**
     * Graphistを実行するメソッド
     * @param {*} procedure 関数名、又はオペレータ
     * @param {*} args 引数の値
     * @return {*} 計算結果
     */
    draw(procedure, args)
    {
        if(Cons.isSymbol(procedure)){ return this.selectProcedure(procedure, args); }
        return this.entrustEvaluator(procedure, args);
    }

    /**
     * GraphistでできないことをEvaluatorに任せ、結果を応答するメソッド
     * @param {Cons} procedure 関数名、又はオペレータ
     * @param {Cons} args 引数の値
     */
    entrustEvaluator(procedure, args)
    {
        let anObject = Cons.nil;
        let aCons = procedure.cdr;
        this.binding(aCons.car, args);
        aCons = aCons.cdr;

        for(let each of aCons.loop())
        {
            if(each instanceof Table){ break; }
            anObject = Evaluator.eval(each, this.environment, this.streamManager, this.depth);
        }

        return anObject;
    }

    /**
     * 描画時の色の透明度を指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gAlpha(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1 && Cons.isNumber(args.car))
                {
                    let aNumber = args.car <= 0 ? 0 : (args.car >= 1 ? 1 : args.car);
                    this.ctx.globalAlpha = aNumber;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set alpha.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set alpha.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 円弧を描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gArc(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 6 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.cdr.cdr.car))
                {
                    let aFlag = args.cdr.cdr.cdr.cdr.cdr.car >= 0 ? true : false;
                    this.ctx.arc(args.car, args.cdr.car, args.cdr.cdr.car, (Math.PI/180)*args.cdr.cdr.cdr.car, (Math.PI/180)*args.cdr.cdr.cdr.cdr.car, aFlag);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw arc.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw arc.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 弧状に線を描画メソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gArcTo(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 5 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.cdr.car))
                {
                    this.ctx.arcTo(args.car, args.cdr.car, args.cdr.cdr.car, args.cdr.cdr.cdr.car, args.cdr.cdr.cdr.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw arc to.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw arc to.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * べジェ曲線を描画メソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gBezCurveTo(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 6 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.cdr.cdr.car))
                {
                    this.ctx.bezierCurveTo(args.car, args.cdr.car, args.cdr.cdr.car, args.cdr.cdr.cdr.car, args.cdr.cdr.cdr.cdr.car, args.cdr.cdr.cdr.cdr.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw bezier curve.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw bezier curve.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * キャンバスを綺麗にするメソッド
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gClear()
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                this.ctx.fillStyle = "#ffffff"
                this.ctx.fillRect(0, 0, this.canvasWidth, this.canvasHeight);
                this.ctx.fillStyle = "#000000"
                this.ctx.save();
                return InterpretedSymbol.of('t');
            }
            catch(e)
            {
                selectPrintFunction()('Can not clear.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * キャンバスを閉じるメソッド
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gClose()
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                this.canvas.style = "display: none; border-color: white;";
                this.ctx.clearRect(0, 0, this.canvasWidth, this.canvasHeight);
                document.getElementById("canvasState").value = false;
                return InterpretedSymbol.of('t');
            }
            catch(e)
            {
                selectPrintFunction()('Can not close.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas has already been closed.');
            return Cons.nil;
        }
    }

    /**
     * 描画時の色を指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gColor(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1)
                {
                    let aColor = this.selectColor(args);
                    this.ctx.fillStyle = aColor;
                    this.ctx.strokeStyle = aColor;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set color.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set color.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * パスで指定した領域の塗り潰しを描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gFill()
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                this.ctx.fill();
                this.ctx.save();
                return InterpretedSymbol.of('t');
            }
            catch(e)
            {
                selectPrintFunction()('Can not fill.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 塗り潰しの描画時の色を指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gFillColor(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1)
                {
                    let aColor = this.selectColor(args);
                    this.ctx.fillStyle = aColor;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set fill color.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set fill color.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 塗り潰された長方形を描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gFillRect(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 4 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car))
                {
                    this.ctx.fillRect(args.car, args.cdr.car, args.cdr.cdr.car, args.cdr.cdr.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw fill rectangle.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw fill rectangle.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 塗り潰されたテキストを描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gFillText(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 3 && Cons.isString(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car))
                {
                    this.ctx.fillText(args.car, args.cdr.car, args.cdr.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw fill text.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw fill text.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 塗り潰された三角形を描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gFillTri(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 6 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.cdr.cdr.car))
                {
                    this.ctx.beginPath();
                    this.ctx.moveTo(args.car, args.cdr.car);
                    this.ctx.lineTo(args.cdr.cdr.car, args.cdr.cdr.cdr.car);
                    this.ctx.lineTo(args.cdr.cdr.cdr.cdr.car, args.cdr.cdr.cdr.cdr.cdr.car);
                    this.ctx.fill();
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw fill triangle.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw fill triangle.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 現在のパスを終了するメソッド
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gFinishPath()
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                this.ctx.closePath();
                this.ctx.save();
                return InterpretedSymbol.of('t');
            }
            catch(e)
            {
                selectPrintFunction()('Can not finish path.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 描画時のパターンを指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gImage(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 3 && Cons.isString(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car))
                {
                    let anImage  = new Image();
                    anImage.src = args.car;
                    anImage.onload = () => {
                        this.ctx.fillStyle = this.ctx.drawImage(anImage, args.cdr.car, args.cdr.cdr.car);
                    }
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else if(args.length() == 5 && Cons.isString(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.cdr.car))
                {
                    let anImage  = new Image();
                    anImage.src = args.car;
                    anImage.onload = () => {
                        this.ctx.fillStyle = this.ctx.drawImage(anImage, args.cdr.car, args.cdr.cdr.car, args.cdr.cdr.cdr.car, args.cdr.cdr.cdr.cdr.car);
                    }
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw Image.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw Image.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 現在のパスの座標から指定した座標まで線を描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gLineTo(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 2 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car))
                {
                    this.ctx.lineTo(args.car, args.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw line to');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw line to');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 線の先端の形状を指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gLineCap(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1 && Cons.isNumber(args.car))
                {
                    let aString = args.car == 0 ? "butt" : ( args.car > 0 ? "round" : "square" );
                    this.ctx.lineCap = aString;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set line cap.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set line cap.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 線と線の結合部分の形状を指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gLineJoin(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1 && Cons.isNumber(args.car))
                {
                    let aString = args.car == 0 ? "miter" : ( args.car > 0 ? "round" : "bevel" );
                    this.ctx.lineJoin = aString;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set line join.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set line join.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 線の太さを指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gLineWidth(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1 && Cons.isNumber(args.car))
                {
                    let aNumber = args.car <= 0 ? 1 : args.car;
                    this.ctx.lineWidth = aNumber;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set line width.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set line width.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 現在のパスの座標から指定した座標まで何も描画せずに移動するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gMoveTo(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 2 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car))
                {
                    this.ctx.moveTo(args.car, args.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not move');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not move');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * キャンバスを開くメソッド
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gOpen()
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == false)
        {
            try
            {
                this.canvas.style = "display: block; background-color: white; border: solid; border-color: black; width: 100%;";
                document.getElementById("canvasState").value = true;
                this.ctx.fillStyle = "#ffffff"
                this.ctx.fillRect(0, 0, this.canvasWidth, this.canvasHeight);
                this.ctx.fillStyle = "#000000"
                this.ctx.save();
                selectPrintFunction()('canvas size, width :' + this.canvasWidth + ' height :' + this.canvasHeight);
                return InterpretedSymbol.of('t');
            }
            catch(e)
            {
                selectPrintFunction()('Can not open.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas has already been opened.');
            return Cons.nil;
        }
    }

    /**
     * 描画時のパターンを指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gPattern(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 2 && Cons.isString(args.car) && Cons.isNumber(args.cdr.car))
                {
                    let aString = args.cdr.car == 0 ? "repeat" : ( args.cdr.car > 0 ? "repeat-x" : "repeat-y" );
                    let anImage  = new Image();
                    anImage.src = args.car;
                    anImage.onload = () => {
                        this.ctx.fillStyle = this.ctx.createPattern(anImage, aString);
                    }
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set pattern.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set pattern.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 二次曲線を描画メソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gQuadCurveTo(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 4 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car))
                {
                    this.ctx.quadraticCurveTo(args.car, args.cdr.car, args.cdr.cdr.car, args.cdr.cdr.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw quadratic curve.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw quadratic curve.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 現在の描画結果をJpegファイルに変換し、保存するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gSaveJpeg()
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                let anImage = new Image();
                anImage.crossOrigin = 'Anonymous';
                anImage.src = this.canvas.toDataURL("image/jpeg");
                let link = document.createElement('a');
                link.href = anImage.src;
                link.download = "canvas";
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link)

                return InterpretedSymbol.of('t')
            }
            catch(e)
            {
                selectPrintFunction()('Can not save jpeg.  If you are using an image in the canvas, you can\'t save jpeg.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas has already been opened.');
            return Cons.nil;
        }
    }

    /**
     * 現在の描画結果をPngファイルに変換し、保存するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gSavePng()
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                let anImage = new Image();
                anImage.crossOrigin = 'Anonymous';
                anImage.src = this.canvas.toDataURL("image/png");
                let link = document.createElement('a');
                link.href = anImage.src;
                link.download = "canvas";
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link)

                return InterpretedSymbol.of('t')
            }
            catch(e)
            {
                selectPrintFunction()('Can not save png. If you are using an image in the canvas, you can\'t save png.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas has already been opened.');
            return Cons.nil;
        }
    }

    /**
     * キャンバスをスケーリングするメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gScale(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 2 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car))
                {
                    this.ctx.scale(args.car, args.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not scale.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not scale.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

     /**
     * 描画時の影のぼかしを指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gShadowBlur(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            if(args.length() == 1 && Cons.isNumber(args.car))
            {
                this.ctx.Blur = args.car;
                this.ctx.save();
                return InterpretedSymbol.of('t');
            }
            else
            {
                selectPrintFunction()('Can not set shadow blur.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 描画時の影の色を指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gShadowColor(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1)
                {
                    let aColor = this.selectColor(args);
                    this.ctx.shadowColor = aColor;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set shadow color.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set shadow color.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 描画時の影のx方向のオフセットを指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gShadowOffsetX(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            if(args.length() == 1 && Cons.isNumber(args.car))
            {
                this.ctx.shadowOffsetX = args.car;
                this.ctx.save();
                return InterpretedSymbol.of('t');
            }
            else
            {
                selectPrintFunction()('Can not set shadow offsetX.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

     /**
     * 描画時の影のy方向のオフセットを指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gShadowOffsetY(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            if(args.length() == 1 && Cons.isNumber(args.car))
            {
                this.ctx.shadowOffsetY = args.car;
                this.ctx.save();
                return InterpretedSymbol.of('t');
            }
            else
            {
                selectPrintFunction()('Can not set shadow offsetY.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

     /**
     * 指定した時間(ms)待機するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gSleep(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            const sleep = (ms) => { 
                let time = new Date().getTime() + ms;
                while (new Date().getTime() < time); 
            }

            if(args.length() == 1 && Cons.isNumber(args.car))
            {
                sleep(args.car);
                return InterpretedSymbol.of('t');
            }
            else 
            {
                selectPrintFunction()('Can not sleep');
                return Cons.nil;
            }
            
        }
        else
        {
            selectPrintFunction()('The canvas has already been opened.');
            return Cons.nil;
        }
    }

    /**
     * 新しいパスを作成するメソッド
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gStartPath()
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                this.ctx.beginPath();
                this.ctx.save();
                return InterpretedSymbol.of('t');
            }
            catch(e)
            {
                selectPrintFunction()('Can not start path.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * パスで指定した輪郭を描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gStroke()
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                this.ctx.stroke();
                this.ctx.save();
                return InterpretedSymbol.of('t');
            }
            catch(e)
            {
                selectPrintFunction()('Can not stroke');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 輪郭の描画時の色を指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gStrokeColor(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1)
                {
                    let aColor = this.selectColor(args);
                    this.ctx.strokeStyle = aColor;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set stroke color');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set stroke color');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 長方形の輪郭を描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gStrokeRect(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 4 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car))
                {
                    this.ctx.strokeRect(args.car, args.cdr.car, args.cdr.cdr.car, args.cdr.cdr.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw stroke rectangle.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw stroke rectangle.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * テキストの輪郭を描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gStrokeText(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 3 && Cons.isString(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car))
                {
                    this.ctx.strokeText(args.car, args.cdr.car, args.cdr.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw fill text.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw fill text.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 三角形の輪郭を描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gStrokeTri(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 6 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.cdr.cdr.car))
                {
                    this.ctx.beginPath();
                    this.ctx.moveTo(args.car, args.cdr.car);
                    this.ctx.lineTo(args.cdr.cdr.car, args.cdr.cdr.cdr.car);
                    this.ctx.lineTo(args.cdr.cdr.cdr.cdr.car, args.cdr.cdr.cdr.cdr.cdr.car);
                    this.ctx.closePath();
                    this.ctx.stroke();
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw stroke triangle.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw stroke triangle.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 描画するテキストの配置を指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gTextAlign(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1 && Cons.isString(args.car))
                {
                    this.ctx.textAlign = args.car;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set text align.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set text align.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 描画するテキストのベースラインアライトメントを指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gTextBaseline(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1 && Cons.isString(args.car))
                {
                    this.ctx.textBaseline = args.car;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set text baseline.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set text baseline.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 描画するテキストの方向を指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gTextDirection(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1 && Cons.isNumber(args.car))
                {
                    let aString = args.car == 0 ? "inherit" : ( args.car > 0 ? "rtl" : "ltr" );
                    this.ctx.direction = aString;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set text direction.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set text direction.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 描画するテキストのフォントを指定するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gTextFont(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1 && Cons.isString(args.car))
                {
                    this.ctx.font = args.car;
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not set text font.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not set text font.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * キャンバスの原点を移動させるメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gTranslate(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 2 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car))
                {
                    this.ctx.translate(args.car, args.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not translate.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not translate.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * 長方形を描画するメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gRect(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 4 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car))
                {
                    this.ctx.rect(args.car, args.cdr.car, args.cdr.cdr.car, args.cdr.cdr.cdr.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not draw rectangle.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not draw rectangle.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * キャンバスの原点を中心に描画時の座標回転させるメソッド
     * @param {Cons} args 引数
     * @return {*} インタプリテッドシンボルt、もしくはnil
     */
    gRotate(args)
    {
        if(!this.checkSupport()){ return Cons.nil; }
        if(this.canvasState == true)
        {
            try
            {
                if(args.length() == 1 && Cons.isNumber(args.car))
                {
                    this.ctx.rotate((Math.PI/180)*args.car);
                    this.ctx.save();
                    return InterpretedSymbol.of('t');
                }
                else
                {
                    selectPrintFunction()('Can not rotate.');
                    return Cons.nil;
                }
            }
            catch(e)
            {
                selectPrintFunction()('Can not rotate.');
                return Cons.nil;
            }
        }
        else
        {
            selectPrintFunction()('The canvas is closed and cannot be executed.');
            return Cons.nil;
        }
    }

    /**
     * スパイする必要があるかどうかを判別し、応答するメソッド
     * @param {InterpretedSymbol} aSymbol
     * @return {Boolean} 真偽値
     */
    isSpy(aSymbol)
    {
        return this.streamManager.isSpy(aSymbol);
    }

     /**
     * 引数から色を指定するメソッド
     * @param {Cons} args 引数
     * @return {String} 色を表す文字列
     */
    selectColor(args)
    {
        let aColor = 'black';
        if(args.length() == 1 && Cons.isString(args.car))
        {
            aColor = args.car;
        }
        else if(args.length() == 3 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car))
        {
            aColor = "rgb(" + args.car + ", " +  args.cdr.car + ", " +  args.cdr.cdr.car + ")";
        }
        else if(args.length() == 4 && Cons.isNumber(args.car) && Cons.isNumber(args.cdr.car) && Cons.isNumber(args.cdr.cdr.car) && Cons.isNumber(args.cdr.cdr.cdr.car))
        {
            aColor = "rgba(" + args.car + ", " +  args.cdr.car + ", " +  args.cdr.cdr.car + ", " +  args.cdr.cdr.cdr.car + ")";
        }
        else
        {
            selectPrintFunction()('Can not set color. set color "black".');
        }

        return aColor;
    }

    /**
     * 実行する処理を選択し、実行するメソッド
     * @param {InterpretedSymbol} procedure 関数名、又はオペレータ
     * @param {Cons} args 引数
     * @return {*} 計算結果
     */
    selectProcedure(procedure, args)
    {
        if(Graphist.buildInFunctions.has(procedure)){ return this.buildInFunction(procedure, args); }
        selectPrintFunction()('I could find no procedure description for ' + procedure);

        return Cons.nil;
    }

    /**
     * 呼び出しの深さを設定するメソッド
     * @param {Number} aNumber 呼び出しの深さ
     * @return {Null} 何も返さない。
     */
    setDepth(aNumber)
    {
        this.depth = aNumber;
        return null;
    }

    /**
     * Lispの関数とJSの関数を紐づけるテーブルを応答するメソッド
     * @return {Table} 生成したテーブル
     */
    static setup()
    {
        try
        {
            let aTable = new Map();

            aTable.set(InterpretedSymbol.of("galpha"), "gAlpha");
            aTable.set(InterpretedSymbol.of("garc"), "gArc");
            aTable.set(InterpretedSymbol.of("garc-to"), "gArcTo");
            aTable.set(InterpretedSymbol.of("gbezcurve-to"), "gBezCurveTo");
            aTable.set(InterpretedSymbol.of("gclear"), "gClear");
            aTable.set(InterpretedSymbol.of("gclose"), "gClose");
            aTable.set(InterpretedSymbol.of("gcolor"), "gColor");
            aTable.set(InterpretedSymbol.of("gfill"), "gFill");
            aTable.set(InterpretedSymbol.of("gfill-color"), "gFillColor");
            aTable.set(InterpretedSymbol.of("gfill-rect"), "gFillRect");
            aTable.set(InterpretedSymbol.of("gfill-text"), "gFillText");
            aTable.set(InterpretedSymbol.of("gfill-tri"), "gFillTri");
            aTable.set(InterpretedSymbol.of("gfinish-path"), "gFinishPath");
            aTable.set(InterpretedSymbol.of("gimage"), "gImage");
            aTable.set(InterpretedSymbol.of("gmove-to"), "gMoveTo");
            aTable.set(InterpretedSymbol.of("gline-to"), "gLineTo");
            aTable.set(InterpretedSymbol.of("gline-cap"), "gLineCap");
            aTable.set(InterpretedSymbol.of("gline-join"), "gLineJoin");
            aTable.set(InterpretedSymbol.of("gline-width"), "gLineWidth");
            aTable.set(InterpretedSymbol.of("gopen"), "gOpen");
            aTable.set(InterpretedSymbol.of("gpattern"), "gPattern");
            aTable.set(InterpretedSymbol.of("gquadcurve-to"), "gQuadCurveTo");
            aTable.set(InterpretedSymbol.of("gsave-jpeg"), "gSaveJpeg");
            aTable.set(InterpretedSymbol.of("gsave-png"), "gSavePng");
            aTable.set(InterpretedSymbol.of("gscale"), "gScale");
            aTable.set(InterpretedSymbol.of("gshadow-blur"), "gShadowBlur");
            aTable.set(InterpretedSymbol.of("gshadow-color"), "gShadowColor");
            aTable.set(InterpretedSymbol.of("gshadow-offsetx"), "gShadowOffsetX");
            aTable.set(InterpretedSymbol.of("gshadow-offsety"), "gShadowOffsetY");
            aTable.set(InterpretedSymbol.of("gsleep"), "gSleep");
            aTable.set(InterpretedSymbol.of("gstart-path"), "gStartPath");
            aTable.set(InterpretedSymbol.of("gstroke"), "gStroke");
            aTable.set(InterpretedSymbol.of("gstroke-color"), "gStrokeColor");
            aTable.set(InterpretedSymbol.of("gstroke-rect"), "gStrokeRect");
            aTable.set(InterpretedSymbol.of("gstroke-text"), "gStrokeText");
            aTable.set(InterpretedSymbol.of("gstroke-tri"), "gStrokeTri");
            aTable.set(InterpretedSymbol.of("gtext-align"), "gTextAlign");
            aTable.set(InterpretedSymbol.of("gtext-dire"), "gTextDirection");
            aTable.set(InterpretedSymbol.of("gtext-font"), "gTextFont");
            aTable.set(InterpretedSymbol.of("gtext-line"), "gTextBaseline");
            aTable.set(InterpretedSymbol.of("gtranslate"), "gTranslate");
            aTable.set(InterpretedSymbol.of("grect"), "gRect");
            aTable.set(InterpretedSymbol.of("grotate"), "gRotate");
            
            return aTable;
        }
        catch(e){ throw new Error('NullPointerException (Graphist, initialize)'); }
    }

    spyPrint(aStream, line)
    {
        let aPrintStream = process.stdout;
        if(aStream != null){ selectPrintFunction()(aStream); }
        selectPrintFunction()(this.indent() + line);
        if(aStream != null){ selectPrintFunction()(aPrintStream); }
        return null;
    }
}
