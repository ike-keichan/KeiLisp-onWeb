# KeiLisp-onWeb
（this document fix： 2020/01/03 create: 2020/12/04）<br>
[https://ike-keichan.github.io/KeiLisp-onWeb/](https://ike-keichan.github.io/KeiLisp-onWeb/)

## Status
Creation Period：2020.7~<br>
Version： 1.0 (2020/12/4)

## About program
Hello! I am developing it as a graduation research project of "Special Research on Computer Science and Engineering IIA/IIB" in Department of Computer Science and Engineering, Kyoto Sangyo University. 

This is an interpreter that mimics Lisp. The interpreter can be started right [here](https://ike-keichan.github.io/KeiLisp-onWeb/).
I hope this will be the beginning of your encounter with Lisp and functional programming.
This program is intended to be run in a web browser.
A program that can be run in the CLI is [here](https://github.com/ike-keichan/KeiLisp).

As of December 4, 2020, version 1.0 is complete.
## Execution environment
### OS
```
$ sw_vers
ProductName:	Mac OS X
ProductVersion:	10.15.7
BuildVersion:	19H2
```

### NVM
```
$ nvm --version
0.35.3
```

### Node.js
```
$ node --version
v12.18.3
```

### Vue.js
```
$ vue --version
@vue/cli 4.5.9
```

If the OS, Node.js and Vue.js versions match, the following software will be installed automatically when you setup your environment.

### Node module
```
$ npm list --depth=0
KeiLisp-onWeb@0.1.0 ~/KeiLisp-onWeb
├── @vue/cli-plugin-babel@4.5.9
├── @vue/cli-service@4.5.9
├── core-js@3.8.0
├── expose-gc@1.0.0
├── ramda@0.27.1
├── v8@0.1.0
├── vue@2.6.12
└── vue-template-compiler@2.6.12
```

## Reference
+ [Atom](./README_Atom.md)
+ [Cons](./README_Cons.md)
+ [Function](./README_Function.md)
+ [2D Graphic Function](./README_2DGraphic.md)

## Quick start
The interpreter can be started right [here](https://ike-keichan.github.io/KeiLisp-onWeb/).

## Quick start（local）
If you want to run it in a local environment, please follow these steps。

### Install
```
$ git clone https://github.com/ike-keichan/KeiLisp-onWeb.git
```

### Setup & Launch
```
$ cd ./KeiLisp-onWeb
$ npm install
$ npm run serve
$ open http://localhost:8080/
```

## Example
+ [Basic Functions](./README_Example.md#basic-funcitons-example)
+ [2D Graphic Functions](./README_Example.md#2d-graphic-functions-example)

---
## Others

### Compiles and minifies for production
```
$ npm run build
```

### Lints and fixes files
```
$ npm run lint
```

### Customize configuration
See [Configuration Reference](https://cli.vuejs.org/config/).
