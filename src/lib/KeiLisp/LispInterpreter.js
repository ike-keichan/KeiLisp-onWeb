// #!/usr/bin/env node

'use strict';

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
import { selectPrintFunction } from '../Utility.js'

/**
 * @class
 * @classdesc インタプリタのクラス
 * @author Keisuke Ikeda
 * @this {LispInterpreter}
 */
export class LispInterpreter extends Object
{
    /**
	 * コンストラクタメソッド
     * @constructor
     * @return {LispInterpreter} 自身
	 */
    constructor()
	{
        super();
        // このインスタンスの環境を保持する変数
        this.root = this.initializeTable();
        // ストリームを管理する変数
        this.streamManager = new StreamManager();

        return this;
    }

    /**
     * 引数のリストを評価し、評価値を応答するメソッド
     * @param {Cons} aCons 評価するリスト
     * @return {Object} 評価値
     */
    eval(aCons)
    {
        let anObject = Cons.nil;

        try { anObject = Evaluator.eval(aCons, this.root, this.streamManager); }
        catch (e) 
        {
            selectPrintFunction()('*** can not eval ' + aCons.toString() + ' ***');
            anObject = Cons.nil; 
        }

        // anObject = Evaluator.eval(aCons, this.root, this.streamManager); //デバック用

        return anObject;
    }

    /**
     * 引数の文字列をパースし、リストにして応答するメソッド
     * @param {String} aString パースする文字列
     * @return {Cons} パースしたリスト
     */
    parse(aString)
    {
        let aCons = null;

        try { aCons = Cons.parse('(' + aString + '\n);'); }
        catch (e)
        {
            selectPrintFunction()('*** can not parse ' + aString.replace( /\n/g , "" ) + ' ***');
            aCons = Cons.nil;
        }

        return aCons;
    }

    /**
	 * 指定された環境を環境の根として設定する.
	 * @param {Table} environment
     * @return {Null} 何も返さない。
	 */
    setRoot(environment)
    {
        if(environment instanceof Table)
        {
            environment.setRoot(true);
            this.root = environment;
        }

        return null;
    }

    /**
     * 環境のテーブルを初期化するメソッド
     * @return {Table} 初期化した環境のテーブル
     */
    initializeTable()
    {
        let aList = new Array();
        let aTable = new Table();
        aTable.setRoot(true);

        aList.push('galpha');
        aList.push('garc');
        aList.push('garc-to');
        aList.push('gbezcurve-to');
        aList.push('gclear');
        aList.push('gclose');
        aList.push('gcolor');
        aList.push('gfill');
        aList.push('gfill-color');
        aList.push('gfill-rect');
        aList.push('gfill-text');
        aList.push('gfill-tri');
        aList.push('gfinish-path');
        aList.push('gimage');
        aList.push('gmove-to');
        aList.push('gline-to');
        aList.push('gline-cap');
        aList.push('gline-join');
        aList.push('gline-width');
        aList.push('gopen');
        aList.push('gpattern');
        aList.push('gquadcurve-to');
        aList.push('gsave-jpeg');
        aList.push('gsave-png');
        aList.push('gscale');
        aList.push('gshadow-blur');
        aList.push('gshadow-color');
        aList.push('gshadow-offsetx');
        aList.push('gshadow-offsety');
        aList.push('gsleep');
        aList.push('gstart-path');
        aList.push('gstroke');
        aList.push('gstroke-color');
        aList.push('gstroke-rect');
        aList.push('gstroke-text');
        aList.push('gstroke-tri');
        aList.push('gtext-align');
        aList.push('gtext-dire');
        aList.push('gtext-font');
        aList.push('gtext-line');
        aList.push('gtranslate');
        aList.push('grect');
        aList.push('grotate');

        aList.push('abs');
        aList.push('add');
        aList.push('and');
        aList.push('apply');
        aList.push('assoc');
        aList.push('atom');
        aList.push('bind');
        aList.push('car');
        aList.push('cdr');
        aList.push('characterp');
        aList.push('clear');
        aList.push('cond');
        aList.push('cons');
        aList.push('consp');
        aList.push('copy');
        aList.push('cos');
        aList.push('floatp');
        aList.push('defun');
        aList.push('divide');
        aList.push('do');
        aList.push('do*');
        aList.push('dolist');
        aList.push('doublep');
        aList.push('eq');
        aList.push('equal');
        aList.push('exit');
        aList.push('exp');
        aList.push('gc');
        aList.push('gensym');
        aList.push('if');
        aList.push('integerp');
        aList.push('lambda');
        aList.push('let');
        aList.push('let*');
        aList.push('last');
        aList.push('list');
        aList.push('listp');
        aList.push('mapcar');
        aList.push('member');
        aList.push('memq');
        aList.push('mod');
        aList.push('multiply');
        aList.push('napier');
        aList.push('neq');
        aList.push('nequal');
        aList.push('not');
        aList.push('notrace');
        aList.push('nth');
        aList.push('null');
        aList.push('numberp');
        aList.push('or');
        aList.push('pi');
        aList.push('pop');
        aList.push('progn');
        aList.push('printc');
        aList.push('print');
        aList.push('push');
        aList.push('quote');
        aList.push('random');
        aList.push('reload');
        aList.push('round');
        aList.push('rplaca');
        aList.push('rplacd');
        aList.push('setq');
        aList.push('set-allq');
        aList.push('sin');
        aList.push('sqrt');
        aList.push('subtract');
        aList.push('stringp');
        aList.push('symbolp');
        aList.push('tan');
        aList.push('terpri');
        aList.push('time');
        aList.push('trace');
        aList.push('unless');
        aList.push('when');
        aList.push('+');
        aList.push('-');
        aList.push('*');
        aList.push('/');
        aList.push('//');
        aList.push('=');
        aList.push('==');
        aList.push('~=');
        aList.push('~~');
        aList.push('<');
        aList.push('<=');
        aList.push('>');
        aList.push('>=');

        aList.forEach(each => {
            let aSymbol = InterpretedSymbol.of(each);
            aTable.set(aSymbol, aSymbol)
        });

        let aString = new String();
        let aCons = new Cons();
        aString = "(lambda (list1 list2) (cond ((null (listp list1)) nil) ((null (listp list2)) nil) ((null list1) list2) (t (cons (car list1) (append (cdr list1) list2)))))";
        aCons = Cons.parse(aString);
        aCons.last().setCdr(new Cons(aTable, Cons.nil));
        aTable.set(InterpretedSymbol.of('append'), aCons);

        aString = "(lambda (l n) (cond ((<= (length l) n) nil) (t (cons (car l) (butlast (cdr l) n)))))";
        aCons = Cons.parse(aString);
        aCons.last().setCdr(new Cons(aTable, Cons.nil));
		aTable.set(InterpretedSymbol.of('butlast'), aCons);

		aString = "(lambda (l) (cond ((null (listp l)) nil) ((null l) 0) (t (+ 1 (length (cdr l))))))";
        aCons = Cons.parse(aString);
        aCons.last().setCdr(new Cons(aTable, Cons.nil));
		aTable.set(InterpretedSymbol.of('length'), aCons);

		aString = "(lambda (n l) (cond ((> n (length l)) nil) ((= 0 n) l) (t (nthcdr (- n 1) (cdr l)))))";
        aCons = Cons.parse(aString);
        aCons.last().setCdr(new Cons(aTable, Cons.nil));
		aTable.set(InterpretedSymbol.of('nthcdr'), aCons);

		aString = "(lambda (l) (cond ((null (listp l)) l) ((null l) '()) (t (append (reverse (cdr l)) (list (car l))))))";
        aCons = Cons.parse(aString);
        aCons.last().setCdr(new Cons(aTable, Cons.nil));
        aTable.set(InterpretedSymbol.of('reverse'), aCons);
        
        aTable.set(InterpretedSymbol.of('t'), InterpretedSymbol.of('t'));

        return aTable;
    }
}
