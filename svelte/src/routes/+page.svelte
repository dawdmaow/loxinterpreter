<!-- <script context="module" lang="ts">
	// Declare the external function from lox.js
	declare function runCodeFromJs(code: string): void;
</script> -->

<script lang="ts">
	import { onDestroy, onMount } from 'svelte';
	import * as monaco from 'monaco-editor';
	import editorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';
	import jsonWorker from 'monaco-editor/esm/vs/language/json/json.worker?worker';
	import cssWorker from 'monaco-editor/esm/vs/language/css/css.worker?worker';
	import htmlWorker from 'monaco-editor/esm/vs/language/html/html.worker?worker';
	import tsWorker from 'monaco-editor/esm/vs/language/typescript/ts.worker?worker';
	import { Terminal } from 'xterm';
	import { FitAddon } from 'xterm-addon-fit';

	let editorElement: HTMLDivElement;
	let editor: monaco.editor.IStandaloneCodeEditor;
	// let model: monaco.editor.ITextModel;

	// function loadCode(code: string) {
	// 	model = monaco.editor.createModel(code, 'javascript');
	// 	editor.setModel(model);
	// }

	const fitAddon = new FitAddon();

	let term = new Terminal({
		disableStdin: true
	});
	term.loadAddon(fitAddon);

	function writeLine(data: string) {
		term.write(data + '\r\n');
	}

	function writeErrorLine(data: string) {
		term.write('\x1b[31m' + data + '\x1b[0m' + '\r\n');
	}

	function xterm(node: HTMLDivElement) {
		term.open(node);
		term.write('\x1b[?25l');
		fitAddon.fit();
		const resizeObserver = new ResizeObserver(() => {
			fitAddon.fit();
		});
		resizeObserver.observe(node);
	}

	onMount(async () => {
		self.MonacoEnvironment = {
			getWorker: function (_: any, label: string) {
				if (label === 'json') {
					return new jsonWorker();
				}
				if (label === 'css' || label === 'scss' || label === 'less') {
					return new cssWorker();
				}
				if (label === 'html' || label === 'handlebars' || label === 'razor') {
					return new htmlWorker();
				}
				if (label === 'typescript' || label === 'javascript') {
					return new tsWorker();
				}
				return new editorWorker();
			}
		};

		monaco.languages.typescript.typescriptDefaults.setEagerModelSync(true);

		monaco.languages.register({ id: 'lox' });

		monaco.languages.setMonarchTokensProvider('lox', {
			defaultToken: 'invalid',
			tokenizer: {
				root: [
					// Comments
					[/\/\/.*$/, 'comment'],

					// Whitespace
					[/[ \t\r\n]+/, 'white'],

					// Keywords
					[
						/\b(super|this|if|else|while|nil|var|for|class|proc|print|return|continue|break)\b/,
						'keyword'
					],

					// Boolean literals
					[/\b(true|false)\b/, 'boolean'],

					// Number literals (integers and floats)
					[/\b\d+\.\d+\b/, 'number.float'],
					[/\b\d+\b/, 'number'],

					// String literals
					[/"([^"\\]|\\.)*$/, 'string.invalid'], // non-terminated string
					[/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

					// Operators
					[/(\+|-|\*|\/|mod|\*\*|&)/, 'operator'],
					[/(==|!=|<=|>=|<|>)/, 'operator.comparison'],
					[/\b(and|or|not)\b/, 'operator.logical'],

					// Identifiers
					[/[a-zA-Z_][a-zA-Z0-9_]*/, 'identifier'],

					// Delimiters and brackets
					[/[{}()\[\]]/, '@brackets'],
					[/[;,.]/, 'delimiter'],

					// Assignment
					[/=/, 'operator.assignment']
				],
				string: [
					[/[^\\"]+/, 'string'],
					[/\\./, 'string.escape'],
					[/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
				]
			}
		});

		editor = monaco.editor.create(editorElement, {
			automaticLayout: true,
			language: 'lox',
			theme: 'vs-dark'
		});

		// Initialize editor with sample Lox code
		const sampleCode = `// Lox language sample
class Breakfast {
    init() {
        this.food = "eggs and bacon";
    }

    getResult() {
        print "Eating:";
        print this.food;
    }
}

class Calculator < Breakfast {
  init() {
    super.init();
    this.result = 0;
    super.getResult();
  }

  add(value) {
    this.result = this.result + value;
    return this;
  }

  multiply(value) {
    this.result = this.result * value;
    return this;
  }

  getResult() {
    return this.result;
  }
}

for (var i = 1; i <= 5; i = i + 1) {
	print i;
}

var time0 = clock();
var calc = Calculator();
calc.add(5).multiply(2).add(10);
print "Calculator result:";
print calc.getResult();
print "Elapsed milliseconds:";
print clock() - time0;
`;

		editor.setValue(sampleCode);
	});

	onDestroy(() => {
		monaco?.editor.getModels().forEach((model) => model.dispose());
		editor?.dispose();
	});

	function runCode() {
		const code = editor.getValue();
		term.clear();
		writeLine('\x1b[34mRunning code...\x1b[0m');
		(window as any).Module.callMain([code]);
	}

	(window as any).Module = {
		// arguments: ['a', 'b', 'c'],
		noInitialRun: true,
		print: (...args: string[]) => {
			console.log(...args);
			for (const arg of args) {
				writeLine(arg);
			}
		},
		printErr: (...args: string[]) => {
			console.error(...args);
			for (const arg of args) {
				writeErrorLine(arg);
			}
		}
	};

	const loxwasm = document.createElement('script');
	if (import.meta.env.DEV) {
		loxwasm.src = '/loxwasm.js';
	} else {
		loxwasm.src = '/loxinterpreter/loxwasm.js';
	}
	document.head.appendChild(loxwasm);
</script>

<svelte:head>
	<link rel="stylesheet" href="xterm.css" />
	<!-- <script src="/lox.js"></script> -->
	<!-- <script src="/loxwasm.js"></script> -->
	<style>
		:root {
			color-scheme: dark;
		}
	</style>
</svelte:head>

<div class="relative flex h-screen w-full flex-col">
	<div class="flex w-full gap-2 bg-gray-950 p-2">
		<button
			class="cursor-pointer rounded-md bg-blue-800 px-4 py-0 transition-all hover:brightness-125"
			on:click={runCode}>Run</button
		>
	</div>
	<div class="grid h-full w-full grid-cols-2">
		<div class="flex-grow" bind:this={editorElement}></div>
		<div class="pointer-events-none" use:xterm></div>
	</div>
</div>
